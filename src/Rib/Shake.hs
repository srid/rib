{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Combinators for working with Shake.
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
  , Dirs(..)
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Binary
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
import System.Directory (createDirectoryIfMissing)
import Text.Pandoc (Pandoc (Pandoc), PandocIO, ReaderOptions)

import Rib.Reader

newtype Dirs = Dirs (FilePath, FilePath)

getDirs :: Action (FilePath, FilePath)
getDirs = getShakeExtra >>= \case
  Just (Dirs d) -> return d
  Nothing -> fail "Input output directories are not initialized"

ribInputDir :: Action FilePath
ribInputDir = fst <$> getDirs

ribOutputDir :: Action FilePath
ribOutputDir = do
  output <- snd <$> getDirs
  liftIO $ createDirectoryIfMissing True output
  return output

-- | Shake action to copy static files as is
buildStaticFiles :: [FilePattern] -> Action [FilePath]
buildStaticFiles staticFilePatterns = do
  input <- ribInputDir
  output <- ribOutputDir
  files <- getDirectoryFiles input staticFilePatterns
  void $ forP files $ \f ->
    copyFileChanged (input </> f) (output </> f)
  pure files

-- | Convert the given pattern of source files into their HTML.
buildHtmlMulti
  :: forall doc. (RibReader doc, FromJSON doc, ToJSON doc)
  => FilePattern
  -- ^ Source file patterns
  -> ((FilePath, doc) -> Html ())
  -- ^ How to render the given Pandoc document to HTML
  -> Action [(FilePath, doc)]
  -- ^ List of relative path to generated HTML and the associated Pandoc document
buildHtmlMulti pat r = do
  xs <- readPandocMulti pat
  void $ forP xs $ \x ->
    buildHtml (fst x -<.> "html") (r x)
  pure xs

-- | Like `readPandoc` but operates on multiple files
readPandocMulti
  :: forall doc. (RibReader doc, FromJSON doc, ToJSON doc)
  => FilePattern
     -- ^ Source file patterns
  -> Action [(FilePath, doc)]
readPandocMulti pat = do
  input <- ribInputDir
  fs <- getDirectoryFiles input [pat]
  forP fs $ \f ->
    jsonCacheAction @(FilePath, doc) f $ fmap (f, ) $ readPandoc f

-- | Read and parse a Pandoc source document
--
-- If an associated metadata file exists (same filename, with @.yaml@ as
-- extension), use it to specify the metadata of the document.
readPandoc :: forall doc. (RibReader doc) => FilePath -> Action doc
readPandoc f = do
  input <- ribInputDir
  let inp = input </> f
  need [inp]
  content <- T.decodeUtf8 <$> liftIO (BS.readFile inp)
  readDoc content

-- | Build a single HTML file with the given value
buildHtml :: FilePath -> Html () -> Action ()
buildHtml f html = do
  output <- ribOutputDir
  let out = output </> f
  writeHtml out html

writeHtml :: MonadIO m => FilePath -> Html () -> m ()
writeHtml f = liftIO . BSL.writeFile f . Lucid.renderBS

-- | Like `Development.Shake.cacheAction` but uses JSON instance instead of Typeable / Binary on `b`.
jsonCacheAction :: forall a b k. (FromJSON b, Typeable k, Binary k, Show k, ToJSON a) => k -> Action a -> Action b
jsonCacheAction k =
    fmap (either error id . Aeson.eitherDecode)
  . cacheAction k
  . fmap Aeson.encode
