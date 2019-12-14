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

import Data.Aeson
import Development.Shake
import Lucid (Html)
import qualified Lucid
import Named
import Path
import Path.IO
import Relude.Extra.Map
import Rib.Document
import Rib.Markup

data Dirs = Dirs (Path Rel Dir, Path Rel Dir)
  deriving (Typeable)

getDirs :: Action (Path Rel Dir, Path Rel Dir)
getDirs = getShakeExtra >>= \case
  Just (Dirs d) -> return d
  Nothing -> fail "Input output directories are not initialized"

ribInputDir :: Action (Path Rel Dir)
ribInputDir = fst <$> getDirs

ribOutputDir :: Action (Path Rel Dir)
ribOutputDir = do
  output <- snd <$> getDirs
  liftIO $ createDirIfMissing True output
  return output

-- | Shake action to copy static files as is
buildStaticFiles :: [Path Rel File] -> Action ()
buildStaticFiles staticFilePatterns = do
  input <- ribInputDir
  output <- ribOutputDir
  files <- getDirectoryFiles' input staticFilePatterns
  void $ forP files $ \f ->
    copyFileChanged' (input </> f) (output </> f)
  where
    copyFileChanged' old new =
      copyFileChanged (toFilePath old) (toFilePath new)

-- | Convert the given pattern of source files into their HTML.
buildHtmlMulti ::
  forall meta doc.
  (FromJSON meta, Markup doc) =>
  -- | Source file patterns
  Map (Path Rel File) (ParsedDoc doc) ->
  -- | How to render the given document to HTML
  (Document meta -> Html ()) ->
  -- | List of relative path to generated HTML and the associated document
  Action [Document meta]
buildHtmlMulti pat r = do
  xs <- readDocMulti pat
  void $ forP xs $ \x -> do
    outfile <- liftIO $ replaceExtension ".html" $ documentPath x
    buildHtml outfile (r x)
  pure xs

-- | Like `readDoc'` but operates on multiple files
readDocMulti ::
  forall meta doc.
  (FromJSON meta, Markup doc) =>
  -- | Source file patterns
  Map (Path Rel File) (ParsedDoc doc) ->
  Action [Document meta]
readDocMulti pats = do
  input <- ribInputDir
  fmap concat $ forM (toPairs pats) $ \(pat, dp) -> do
    fs <- getDirectoryFiles' input [pat]
    forP fs $ \f -> do
      need $ toFilePath <$> [input </> f]
      result <-
        runExceptT $
          mkDocumentFrom dp
            ! #relpath f
            ! #path (input </> f)
      case result of
        Left e ->
          fail $ "Error converting " <> toFilePath f <> " to HTML: " <> show e
        Right v -> pure v

-- | Build a single HTML file with the given value
buildHtml :: Path Rel File -> Html () -> Action ()
buildHtml f html = do
  output <- ribOutputDir
  writeHtml (output </> f) html
  where
    writeHtml :: MonadIO m => Path b File -> Html () -> m ()
    writeHtml p htmlVal =
      writeFileLText (toFilePath p) $! Lucid.renderText htmlVal

-- | Like `getDirectoryFiles` but work with `Path`
getDirectoryFiles' :: Path b Dir -> [Path Rel File] -> Action [Path Rel File]
getDirectoryFiles' dir pat =
  traverse (liftIO . parseRelFile) =<< getDirectoryFiles (toFilePath dir) (toFilePath <$> pat)
