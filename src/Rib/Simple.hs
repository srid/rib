{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- Sensible defaults for writing the most simple static site
module Rib.Simple where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import GHC.Generics (Generic)

import Development.Shake
import Development.Shake.FilePath
import Lucid
import Text.Pandoc (Pandoc)

import Rib.App (ribInputDir, ribOutputDir)
import Rib.Pandoc (getPandocMetaValue, parsePandoc)
import Rib.Shake (Action, jsonCacheAction)

-- | Represents a HTML page that will be generated
data Page
  = Page_Index [Post]
  | Page_Post Post
  deriving (Generic, Show, FromJSON, ToJSON)

-- | A Post corresponding to the Markdown content
data Post = Post
  { _post_srcPath :: FilePath
  , _post_doc :: Pandoc
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

isDraft :: Post -> Bool
isDraft = fromMaybe False . getPandocMetaValue "draft" . _post_doc

buildAction :: (Page -> Html ()) -> Action ()
buildAction renderPage = do
  void $ buildStaticFiles ["static/**"]
  posts <- fmap (fmap $ uncurry Post) <$>
    buildHtmlMulti ["*.md"] $ renderPage . Page_Post . uncurry Post
  let publicPosts = filter (not . isDraft) posts
  buildHtml "index.html" $  renderPage $ Page_Index publicPosts

-- XXX: everything below is independent of Page/Post type. yay!

-- | Shake action to copy static files as is
buildStaticFiles :: [FilePattern] -> Action [FilePath]
buildStaticFiles staticFilePatterns = do
  files <- getDirectoryFiles ribInputDir staticFilePatterns
  void $ forP files $ \f ->
    copyFileChanged (ribInputDir </> f) (ribOutputDir </> f)
  pure files

-- | Build multiple HTML files given a pattern of source files
--
-- Call `mkA` to create the final value given a file and its pandoc structure.
-- Return the list of final values used to render their HTMLs.
buildHtmlMulti
  -- :: (ToJSON a, FromJSON a)
  -- => (FilePath -> Pandoc -> a)
  -- ^ TODO: Just deal with `(FilePath, Pandoc)` instead of some `a`
  :: [FilePattern]
  -- ^ Source file patterns
  -> ((FilePath, Pandoc) -> Html ())
  -> Action [(FilePath, Pandoc)]
buildHtmlMulti pat r = do
  fs <- getDirectoryFiles ribInputDir pat
  forP fs $ \f -> do
    let out = ribOutputDir </> f -<.> "html"
    v <- jsonCacheAction f $ (f, ) <$> readPandoc f
    writeHtml out $ r v
    pure v

-- | Build a single HTML file with the given value
buildHtml :: FilePath -> Html () -> Action ()
buildHtml f html = do
  let out = ribOutputDir </> f
  writeHtml out html

readPandoc :: FilePath -> Action Pandoc
readPandoc =
    fmap (parsePandoc . T.decodeUtf8With T.lenientDecode . BSC.pack)
  . readFile'
  . (ribInputDir </>)

writeHtml :: MonadIO m => FilePath -> Html () -> m ()
writeHtml f = liftIO . renderToFile f
