{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Combinators for working with Shake.
--
-- See the source of `Rib.Simple.buildAction` for example usage.
module Rib.Shake
  ( -- * Basic helpers
    buildStaticFiles,
    buildHtmlMulti,
    buildHtml,

    -- * Read helpers
    readSourceMulti,

    -- * Misc
    Dirs (..),
    ribInputDir,
    ribOutputDir,
  )
where

import Development.Shake
import Lucid (Html)
import qualified Lucid
import Path
import Path.IO
import Rib.Source

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
  -- | Source file patterns
  [Path Rel File] ->
  -- | How to parse the source
  SourceReader repr ->
  -- | How to render the given source to HTML
  (Source repr -> Html ()) ->
  -- | Result
  Action [Source repr]
buildHtmlMulti pat parser r = do
  srcs <- readSourceMulti pat parser
  void $ forP srcs $ \src -> do
    outfile <- liftIO $ replaceExtension ".html" $ sourcePath src
    buildHtml outfile $ r src
  pure srcs

-- | Like `readSource'` but operates on multiple files
readSourceMulti ::
  -- | Source file patterns
  [Path Rel File] ->
  -- | How to parse the source
  SourceReader repr ->
  -- | Result
  Action [Source repr]
readSourceMulti pats parser = do
  input <- ribInputDir
  fs <- getDirectoryFiles' input pats
  forP fs $ \k -> do
    let f = input </> k
    need $ toFilePath <$> [f]
    readSource parser k f >>= \case
      Left e ->
        fail $ "Error converting " <> toFilePath k <> " to HTML: " <> show e
      Right v -> pure v

-- | Build a single HTML file with the given value
buildHtml :: Path Rel File -> Html () -> Action ()
buildHtml f html = do
  output <- ribOutputDir
  writeHtml (output </> f) html
  where
    writeHtml :: MonadIO m => Path b File -> Html () -> m ()
    writeHtml p htmlVal = do
      -- TODO: Is there a way to make Shake automatically do this for us?
      createDirIfMissing True $ parent p
      writeFileLText (toFilePath p) $! Lucid.renderText htmlVal

-- | Like `getDirectoryFiles` but works with `Path`
getDirectoryFiles' :: Path b Dir -> [Path Rel File] -> Action [Path Rel File]
getDirectoryFiles' dir pat =
  traverse (liftIO . parseRelFile) =<< getDirectoryFiles (toFilePath dir) (toFilePath <$> pat)
