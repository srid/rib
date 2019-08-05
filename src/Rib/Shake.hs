{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Combinators for working with Shake.
--
-- The functions in this module work with `ribInputDir` and `ribOutputDir`.
--
-- See the source of `Rib.Simple.buildAction` for example usage.
module Rib.Shake
  (
  -- * Basic helpers
    buildHtmlMulti
  , buildHtml
  -- * Read helpers
  , readPandoc
  , readPandocMulti
  -- * Misc
  , buildStaticFiles
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Binary
import Data.Bool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Typeable

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Forward (cacheAction)
import Lucid (Html)
import qualified Lucid
import Text.Pandoc (Pandoc (Pandoc), PandocIO, ReaderOptions)

import Rib.App (ribInputDir, ribOutputDir)
import qualified Rib.Pandoc

-- FIXME: Auto create ./b directory


-- | Shake action to copy static files as is
buildStaticFiles :: [FilePattern] -> Action [FilePath]
buildStaticFiles staticFilePatterns = do
  files <- getDirectoryFiles ribInputDir staticFilePatterns
  void $ forP files $ \f ->
    copyFileChanged (ribInputDir </> f) (ribOutputDir </> f)
  pure files

-- | Convert the given pattern of source files into their HTML.
buildHtmlMulti
  :: (FilePattern, ReaderOptions -> Text -> PandocIO Pandoc)
  -- ^ Source file patterns & their associated Pandoc readers
  -> ((FilePath, Pandoc) -> Html ())
  -- ^ How to render the given Pandoc document to HTML
  -> Action [(FilePath, Pandoc)]
  -- ^ List of relative path to generated HTML and the associated Pandoc document
buildHtmlMulti spec r = do
  xs <- readPandocMulti spec
  void $ forP xs $ \x ->
    buildHtml (fst x -<.> "html") (r x)
  pure xs

-- | Like `readPandoc` but operates on multiple files
readPandocMulti
  :: ( FilePattern
     , ReaderOptions -> Text -> PandocIO Pandoc
     )
     -- ^ Tuple of pattern of files to work on and document format.
  -> Action [(FilePath, Pandoc)]
readPandocMulti (pat, r) = do
  fs <- getDirectoryFiles ribInputDir [pat]
  forP fs $ \f ->
    jsonCacheAction f $ (f, ) <$> readPandoc r f

-- | Read and parse a Pandoc source document
--
-- If an associated metadata file exists (same filename, with @.yaml@ as
-- extension), use it to specify the metadata of the document.
readPandoc
  :: (ReaderOptions -> Text -> PandocIO Pandoc)
  -- ^ Document format. Example: `Text.Pandoc.Readers.readMarkdown`
  -> FilePath
  -> Action Pandoc
readPandoc r f = do
  let inp = ribInputDir </> f
  need [inp]
  content <- T.decodeUtf8 <$> liftIO (BS.readFile inp)
  doc <- liftIO $ Rib.Pandoc.parse r content
  -- FIXME: When _creating_ the yaml file for first time, Shake doesn't know to
  -- rebuild this.
  boolFileExists (inp -<.> "yaml") (pure doc) $
    fmap (overrideMeta doc) . readMeta
  where
    overrideMeta (Pandoc _ bs) meta = Pandoc meta bs
    readMeta mf = do
      need [mf]
      liftIO $ Rib.Pandoc.parseMeta =<< BSL.readFile mf
    -- | Like `bool` but works on file existence value
    --
    -- The second function takes the filepath as value.
    boolFileExists fp missingF existsF =
      doesFileExist fp >>= bool missingF (existsF fp)

-- | Build a single HTML file with the given value
buildHtml :: FilePath -> Html () -> Action ()
buildHtml f html = do
  let out = ribOutputDir </> f
  writeHtml out html

writeHtml :: MonadIO m => FilePath -> Html () -> m ()
writeHtml f = liftIO . BSL.writeFile f . Lucid.renderBS

-- | Like `Development.Shake.cacheAction` but uses JSON instance instead of Typeable / Binary on `b`.
jsonCacheAction :: (FromJSON b, Typeable k, Binary k, Show k, ToJSON a) => k -> Action a -> Action b
jsonCacheAction k =
    fmap (either error id . Aeson.eitherDecode)
  . cacheAction k
  . fmap Aeson.encode
