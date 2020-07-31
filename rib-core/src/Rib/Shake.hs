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
    getCliConfig,
    ribInputDir,
    ribOutputDir,
  )
where

import Control.Monad.Catch
import Development.Shake
import Relude
import Rib.Cli (CliConfig)
import qualified Rib.Cli as Cli
import System.Directory
import System.FilePath
import System.IO.Error (isDoesNotExistError)

-- | Get rib settings from a shake Action monad.
getCliConfig :: Action CliConfig
getCliConfig = getShakeExtra >>= \case
  Just v -> pure v
  Nothing -> fail "CliConfig not initialized"

-- | Input directory containing source files
--
-- This is same as the first argument to `Rib.App.run`
ribInputDir :: Action FilePath
ribInputDir = Cli.inputDir <$> getCliConfig

-- | Output directory where files are generated
--
-- This is same as the second argument to `Rib.App.run`
ribOutputDir :: Action FilePath
ribOutputDir = do
  output <- Cli.outputDir <$> getCliConfig
  liftIO $ createDirectoryIfMissing True output
  return output

-- | Shake action to copy static files as is.
buildStaticFiles :: [FilePath] -> Action ()
buildStaticFiles staticFilePatterns = do
  input <- ribInputDir
  output <- ribOutputDir
  files <- getDirectoryFiles input staticFilePatterns
  void $ forP files $ \f ->
    copyFileChanged (input </> f) (output </> f)

-- | Run the given action when any file matching the patterns changes
forEvery ::
  -- | Source file patterns (relative to `ribInputDir`)
  [FilePath] ->
  (FilePath -> Action a) ->
  Action [a]
forEvery pats f = do
  input <- ribInputDir
  fs <- getDirectoryFiles input pats
  forP fs f

-- | Write the given file but only when it has been modified.
--
-- Also, always writes under ribOutputDir
writeFileCached :: FilePath -> String -> Action ()
writeFileCached !k !s = do
  f <- fmap (</> k) ribOutputDir
  currentS <- liftIO $ forgivingAbsence $ readFile f
  unless (Just s == currentS) $ do
    writeFile' f $! s
    -- Use a character (like +) that contrasts with what Shake uses (#) for
    -- logging modified files being read.
    putInfo $ "+ " <> f

-- | If argument of the function throws a
-- 'System.IO.Error.doesNotExistErrorType', 'Nothing' is returned (other
-- exceptions propagate). Otherwise the result is returned inside a 'Just'.
forgivingAbsence :: (MonadIO m, MonadCatch m) => m a -> m (Maybe a)
forgivingAbsence f =
  catchIf
    isDoesNotExistError
    (Just <$> f)
    (const $ return Nothing)
