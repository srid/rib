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
    content <- fmap toText <$> readFile' $ toFilePath f
    -- NOTE: We don't really use cacheActionWith prior to parsing content,
    -- because the parsed representation (`repr`) may not always have instances
    -- for Typeable/Binary/Generic (for example, MMark does not expose its
    -- structure.). Consequently we are forced to cache merely the HTML writing
    -- stage (see below).
    readSource parser k content >>= \case
      Left e ->
        fail $ "Error parsing source " <> toFilePath k <> ": " <> show e
      Right src -> do
        outfile <- liftIO $ replaceExtension ".html" k
        let html = toString $ Lucid.renderText $ r src
            cacheClosure = (toFilePath outfile, html)
        cacheActionWith ("buildHtmlMulti" :: Text, toFilePath k) cacheClosure $ do
          buildHtml' outfile html
        pure src

-- | Build a single HTML file with the given value
buildHtml :: Path Rel File -> Html () -> Action ()
buildHtml f = buildHtml' f . toString . Lucid.renderText

buildHtml' :: Path Rel File -> String -> Action ()
buildHtml' k s = do
  output <- ribOutputDir
  let f = toFilePath $ output </> k
  writeFile' f $! s
  putInfo $ "[Rib] Wrote " <> f

-- | Like `getDirectoryFiles` but works with `Path`
getDirectoryFiles' :: Path b Dir -> [Path Rel File] -> Action [Path Rel File]
getDirectoryFiles' dir pat =
  traverse (liftIO . parseRelFile) =<< getDirectoryFiles (toFilePath dir) (toFilePath <$> pat)
