{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Combinators for working with Shake.
module Rib.Shake
  ( -- * Basic helpers
    buildStaticFiles,
    forEvery,

    -- * Writing only
    writeFileCached,

    -- * Misc
    RibSettings (..),
    ribInputDir,
    ribOutputDir,
    getDirectoryFiles',
  )
where

import Development.Shake
import Path
import Path.IO
import Relude

-- | RibSettings is initialized with the values passed to `Rib.App.run`
data RibSettings
  = RibSettings
      { _ribSettings_inputDir :: Path Abs Dir,
        _ribSettings_outputDir :: Path Abs Dir,
        _ribSettings_workingDir :: Path Abs Dir
      }
  deriving (Typeable)

-- | Get rib settings from a shake Action monad.
ribSettings :: Action RibSettings
ribSettings = getShakeExtra >>= \case
  Just v -> pure v
  Nothing -> fail "RibSettings not initialized"

-- | Input directory containing source files
--
-- This is same as the first argument to `Rib.App.run`, but relative to the
-- working directory.
ribInputDir :: Action (Path Rel Dir)
ribInputDir = do
  RibSettings {..} <- ribSettings
  liftIO $ makeRelative _ribSettings_workingDir _ribSettings_inputDir

-- Output directory containing generated files
--
-- This is same as the second argument to `Rib.App.run`, but relative to the
-- working directory.
ribOutputDir :: Action (Path Rel Dir)
ribOutputDir = do
  RibSettings {..} <- ribSettings
  outputDir <- liftIO $ makeRelative _ribSettings_workingDir _ribSettings_outputDir
  liftIO $ createDirIfMissing True outputDir
  return outputDir

-- | Shake action to copy static files as is.
buildStaticFiles :: [Path Rel File] -> Action ()
buildStaticFiles staticFilePatterns = do
  input <- ribInputDir
  output <- ribOutputDir
  files <- getDirectoryFiles' input staticFilePatterns
  void $ forP files $ \f ->
    copyFileChanged' (input </> f) (output </> f)
  where
    copyFileChanged' (toFilePath -> old) (toFilePath -> new) =
      copyFileChanged old new

-- | Run the given action when any file matching the patterns changes
forEvery ::
  -- | Source file patterns (relative to `ribInputDir`)
  [Path Rel File] ->
  (Path Rel File -> Action a) ->
  Action [a]
forEvery pats f = do
  input <- ribInputDir
  fs <- getDirectoryFiles' input pats
  forP fs f

-- | Write the given file unless it has not changed.
--
-- Also, always writes under ribOutputDir
writeFileCached :: Path Rel File -> String -> Action ()
writeFileCached !k !s = do
  f <- fmap (toFilePath . (</> k)) ribOutputDir
  currentS <- liftIO $ forgivingAbsence $ readFile f
  unless (Just s == currentS) $ do
    writeFile' f $! s
    -- Use a character (like +) that contrasts with what Shake uses (#) for
    -- logging modified files being read.
    putInfo $ "+ " <> f

-- | Like `getDirectoryFiles` but works with `Path`
getDirectoryFiles' :: Typeable b => Path b Dir -> [Path Rel File] -> Action [Path Rel File]
getDirectoryFiles' (toFilePath -> dir) (fmap toFilePath -> pat) =
  traverse (liftIO . parseRelFile) =<< getDirectoryFiles dir pat
