{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
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
    ribInputDir,
    ribOutputDir,
    getDirectoryFiles',
  )
where

import Development.Shake
import Path
import Path.IO
import Relude
import Rib.Settings

-- | Get rib settings from a shake Action monad.
ribSettings :: Action RibSettings
ribSettings = getShakeExtra >>= \case
  Just v -> pure v
  Nothing -> fail "RibSettings not initialized"

-- | Input directory containing source files
--
-- This is same as the first argument to `Rib.App.run`
ribInputDir :: Action (Path Rel Dir)
ribInputDir = _ribSettings_inputDir <$> ribSettings

-- | Output directory where files are generated
--
-- This is same as the second argument to `Rib.App.run`
ribOutputDir :: Action (Path Rel Dir)
ribOutputDir = do
  output <- _ribSettings_outputDir <$> ribSettings
  liftIO $ createDirIfMissing True output
  return output

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

-- | Write the given file but only when it has been modified.
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
