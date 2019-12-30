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

    -- * Misc
    RibSettings (..),
    ribInputDir,
    ribOutputDir,
  )
where

import Data.Binary
import Data.UUID (UUID)
import Development.Shake
import Development.Shake.Forward
import Lucid (Html)
import qualified Lucid
import Path
import Path.IO
import Rib.Source

data RibSettings
  = RibSettings
      { _ribSettings_inputDir :: Path Rel Dir,
        _ribSettings_outputDir :: Path Rel Dir,
        _ribSettings_processUUID :: UUID
      }
  deriving (Typeable)

ribSettings :: Action RibSettings
ribSettings = getShakeExtra >>= \case
  Just v -> pure v
  Nothing -> fail "RibSettings not initialized"

ribInputDir :: Action (Path Rel Dir)
ribInputDir = _ribSettings_inputDir <$> ribSettings

ribOutputDir :: Action (Path Rel Dir)
ribOutputDir = do
  output <- _ribSettings_outputDir <$> ribSettings
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
buildHtmlMulti pats parser r = do
  input <- ribInputDir
  fs <- getDirectoryFiles' input pats
  forP fs $ \k -> do
    let f = input </> k
    outfile <- liftIO $ replaceExtension ".html" k
    content <- fmap toText <$> readFile' $ toFilePath f
    readSource parser k content >>= \case
      Left e ->
        fail $ "Error parsing source " <> toFilePath k <> ": " <> show e
      Right src -> do
        cacheActionWithFileContent f content $ do
          buildHtml outfile $ r src
        pure src

cacheActionWithFileContent ::
  (Typeable a, Binary a, Show a) =>
  Path b File ->
  Text ->
  Action a ->
  Action a
cacheActionWithFileContent f content act = do
  -- FIXME: This does not preserve cache across multiple runs of the Haskell
  -- process. On the good side, within a single session (which is the typical
  -- run use case, at least for the author, esp. with ghcid) cache is observed.
  uuid <- _ribSettings_processUUID <$> ribSettings
  cacheActionWith (toFilePath f) (show uuid <> content) act

-- | Build a single HTML file with the given value
buildHtml :: Path Rel File -> Html () -> Action ()
buildHtml f html = do
  output <- ribOutputDir
  writeHtml (output </> f) html
  putInfo $ "[Rib] Wrote " <> toFilePath (output </> f)
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
