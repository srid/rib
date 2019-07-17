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

-- | An HTML page that will be generated
data Page
  = Page_Index [(FilePath, Pandoc)]
  | Page_Post (FilePath, Pandoc)
  deriving (Generic, Show, FromJSON, ToJSON)

-- TODO: Eventually this should be subsumed into our Pandoc metadata system.
isDraft :: Pandoc -> Bool
isDraft = fromMaybe False . getPandocMetaValue "draft"

buildAction :: (Page -> Html ()) -> Action ()
buildAction renderPage = do
  void $ buildStaticFiles ["static/**"]
  posts <- buildHtmlMulti ["*.md"] $ renderPage . Page_Post
  let publicPosts = filter (not . isDraft . snd) posts
  buildHtml "index.html" $  renderPage $ Page_Index publicPosts

-- XXX: everything below is independent of Page type. yay!
-- TODO: Move to `Rib.Shake`?
-- TODO: And get rid of the above, and have Main.hs do it manually.

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
  xs <- readPandocMulti pat
  void $ forP xs $ \x ->
    buildHtml (fst x -<.> "html") (r x)
  pure xs

readPandocMulti :: [FilePattern] -> Action [(FilePath, Pandoc)]
readPandocMulti pat = do
  fs <- getDirectoryFiles ribInputDir pat
  forP fs $ \f ->
    jsonCacheAction f $ (f, ) <$> readPandoc f

readPandoc :: FilePath -> Action Pandoc
readPandoc =
    fmap (parsePandoc . T.decodeUtf8With T.lenientDecode . BSC.pack)
  . readFile'
  . (ribInputDir </>)

-- | Build a single HTML file with the given value
buildHtml :: FilePath -> Html () -> Action ()
buildHtml f html = do
  let out = ribOutputDir </> f
  writeHtml out html

writeHtml :: MonadIO m => FilePath -> Html () -> m ()
writeHtml f = liftIO . renderToFile f
