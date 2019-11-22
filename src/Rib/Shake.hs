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
  , readDocMulti
  -- * Misc
  , buildStaticFiles
  , Dirs(..)
  )
where

import Control.Monad
import Control.Monad.IO.Class
-- import Data.Binary
import qualified Data.ByteString.Lazy as BSL
-- import Data.Typeable

import Development.Shake
import Development.Shake.FilePath
-- import Development.Shake.Forward (cacheAction)
import Lucid (Html)
import qualified Lucid
import System.Directory (createDirectoryIfMissing)

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
  -- TODO: pure ()
  pure files

-- | Convert the given pattern of source files into their HTML.
buildHtmlMulti
  :: forall t. Markup t
  => FilePattern
  -- ^ Source file patterns
  -> (Article t -> Html ())
  -- ^ How to render the given document to HTML
  -> Action [Article t]
  -- ^ List of relative path to generated HTML and the associated document
buildHtmlMulti pat r = do
  xs <- readDocMulti pat
  void $ forP xs $ \x ->
    buildHtml (_article_path x -<.> "html") (r x)
  pure xs

-- | Like `readDoc'` but operates on multiple files
readDocMulti
  :: forall t. Markup t
  => FilePattern
     -- ^ Source file patterns
  -> Action [Article t]
readDocMulti pat = do
  input <- ribInputDir
  fs <- getDirectoryFiles input [pat]
  forP fs $ \f -> do
    need [input </> f]
    liftIO . readDocIO f $ input </> f

-- | Build a single HTML file with the given value
buildHtml :: FilePath -> Html () -> Action ()
buildHtml f html = do
  output <- ribOutputDir
  writeHtml (output </> f) html

writeHtml :: MonadIO m => FilePath -> Html () -> m ()
writeHtml f = liftIO . BSL.writeFile f . Lucid.renderBS

-- | Like `Development.Shake.cacheAction` but uses JSON instance instead of Typeable / Binary on `b`.
-- jsonCacheAction
--   :: forall a b k. (FromJSON b, Typeable k, Binary k, Show k, ToJSON a)
--   => k -> Action a -> Action b
-- jsonCacheAction k =
--     fmap (either error id . Aeson.eitherDecode)
--   . cacheAction k
--   . fmap Aeson.encode
