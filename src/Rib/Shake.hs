{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Combinators for working with Shake.
--
-- See the source of `Rib.Simple.buildAction` for example usage.
module Rib.Shake
  ( -- * Basic helpers
    buildHtmlMulti,
    buildHtml,

    -- * Read helpers
    readDocMulti,

    -- * Misc
    buildStaticFiles,
    Dirs (..),
  )
where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Typeable
import Development.Shake
import Lucid (Html)
import qualified Lucid
import Named
import Path
import Path.IO
import Rib.Markup

data Dirs b = Dirs (Path b Dir, Path b Dir)
  deriving (Typeable)

getDirs :: Typeable b => Action (Path b Dir, Path b Dir)
getDirs = getShakeExtra >>= \case
  Just (Dirs d) -> return d
  Nothing -> fail "Input output directories are not initialized"

ribInputDir :: Typeable b => Action (Path b Dir)
ribInputDir = fst <$> getDirs

ribOutputDir :: Typeable b => Action (Path b Dir)
ribOutputDir = do
  output <- snd <$> getDirs
  liftIO $ createDirIfMissing True output
  return output

-- | Shake action to copy static files as is
buildStaticFiles :: [Path Rel File] -> Action ()
buildStaticFiles staticFilePatterns = do
  input <- ribInputDir @Rel
  output <- ribOutputDir @Rel
  files <- getDirectoryFiles' input staticFilePatterns
  void $ forP files $ \f ->
    copyFileChanged' (input </> f) (output </> f)
  where
    copyFileChanged' old new =
      copyFileChanged (toFilePath old) (toFilePath new)

-- | Convert the given pattern of source files into their HTML.
buildHtmlMulti ::
  forall t.
  Markup t =>
  -- | Source file patterns
  Path Rel File ->
  -- | How to render the given document to HTML
  (Document t -> Html ()) ->
  -- | List of relative path to generated HTML and the associated document
  Action [Document t]
buildHtmlMulti pat r = do
  xs <- readDocMulti pat
  void $ forP xs $ \x -> do
    outfile <- liftIO $ _document_path x -<.> "html"
    buildHtml outfile (r x)
  pure xs

-- | Like `readDoc'` but operates on multiple files
readDocMulti ::
  forall t.
  Markup t =>
  -- | Source file patterns
  Path Rel File ->
  Action [Document t]
readDocMulti pat = do
  input <- ribInputDir @Rel
  fs <- getDirectoryFiles' input [pat]
  forP fs $ \f -> do
    need $ toFilePath <$> [input </> f]
    result <-
      liftIO $
        readDoc
          ! #relpath f
          ! #path (input </> f)
    pure $ either (error . T.unpack . showMarkupError @t) id result

-- | Build a single HTML file with the given value
buildHtml :: Path Rel File -> Html () -> Action ()
buildHtml f html = do
  output <- ribOutputDir @Rel
  writeHtml (output </> f) html

writeHtml :: MonadIO m => Path b File -> Html () -> m ()
writeHtml f = liftIO . BSL.writeFile (toFilePath f) . Lucid.renderBS

-- | Like `getDirectoryFiles` but work with `Path`
getDirectoryFiles' :: Path b Dir -> [Path Rel File] -> Action [Path Rel File]
getDirectoryFiles' dir pat =
  traverse (liftIO . parseRelFile) =<< getDirectoryFiles (toFilePath dir) (toFilePath <$> pat)
