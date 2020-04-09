{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CLI interface for Rib.
--
-- Mostly you would only need `Rib.App.run`, passing it your Shake build action.
module Rib.App
  ( run,
    runWith,
  )
where

import Control.Concurrent.Async (race_)
import Control.Exception.Safe (catch)
import Development.Shake hiding (command)
import Development.Shake.Forward (shakeForward)
import Options.Applicative
import Path
import Path.IO
import Relude
import Rib.Cli (CliConfig (CliConfig), cliParser)
import qualified Rib.Cli as Cli
import Rib.Log
import qualified Rib.Server as Server
import Rib.Watch (onTreeChange)
import System.FSNotify (Event (..), eventIsDirectory, eventPath)
import System.IO (BufferMode (LineBuffering), hSetBuffering)

-- | Run Rib using arguments passed in the command line.
run ::
  -- | Default value for `Cli.inputDir`
  Path Rel Dir ->
  -- | Deault value for `Cli.outputDir`
  Path Rel Dir ->
  -- | Shake build rules for building the static site
  Action () ->
  IO ()
run src dst buildAction = runWith buildAction =<< execParser opts
  where
    opts =
      info
        (cliParser src dst <**> helper)
        ( fullDesc
            <> progDesc "Generate a static site at OUTPUTDIR using input from INPUTDIR"
        )

-- | Like `run` but with an explicitly passed `CliConfig`
runWith :: Action () -> CliConfig -> IO ()
runWith buildAction cfg@CliConfig {..} = do
  when (inputDir == currentRelDir) $
    -- Because otherwise our use of `watchTree` can interfere with Shake's file
    -- scaning.
    fail "cannot use '.' as source directory."
  -- For saner output
  flip hSetBuffering LineBuffering `mapM_` [stdout, stderr]
  case (watch, serve) of
    (True, Just (host, port)) -> do
      race_
        (Server.serve cfg host port $ toFilePath outputDir)
        (runShakeAndObserve cfg buildAction)
    (True, Nothing) ->
      runShakeAndObserve cfg buildAction
    (False, Just (host, port)) ->
      Server.serve cfg host port $ toFilePath outputDir
    (False, Nothing) ->
      runShakeBuild cfg buildAction
  where
    currentRelDir = [reldir|.|]

shakeOptionsFrom :: CliConfig -> ShakeOptions
shakeOptionsFrom cfg'@CliConfig {..} =
  shakeOptions
    { shakeVerbosity = verbosity,
      shakeFiles = toFilePath shakeDbDir,
      shakeRebuild = bool [] [(RebuildNow, "**")] rebuildAll,
      shakeLintInside = [""],
      shakeExtra = addShakeExtra cfg' (shakeExtra shakeOptions)
    }

runShakeBuild :: CliConfig -> Action () -> IO ()
runShakeBuild cfg@CliConfig {..} buildAction = do
  runShake cfg $ do
    logStrLn cfg $ "[Rib] Generating " <> toFilePath inputDir <> " (rebuildAll=" <> show rebuildAll <> ")"
    buildAction

runShake :: CliConfig -> Action () -> IO ()
runShake cfg shakeAction = do
  shakeForward (shakeOptionsFrom cfg) shakeAction
    `catch` handleShakeException
  where
    handleShakeException (e :: ShakeException) =
      -- Gracefully handle any exceptions when running Shake actions. We want
      -- Rib to keep running instead of crashing abruptly.
      logErr $
        "[Rib] Unhandled exception when building " <> shakeExceptionTarget e <> ": " <> show e

runShakeAndObserve :: CliConfig -> Action () -> IO ()
runShakeAndObserve cfg@CliConfig {..} buildAction = do
  -- Begin with a *full* generation as the HTML layout may have been changed.
  -- TODO: This assumption is not true when running the program from compiled
  -- binary (as opposed to say via ghcid) as the HTML layout has become fixed
  -- by being part of the binary. In this scenario, we should not do full
  -- generation (i.e., toggle the bool here to False). Perhaps provide a CLI
  -- flag to disable this.
  runShakeBuild (cfg {Cli.rebuildAll = True}) buildAction
  -- And then every time a file changes under the current directory
  logStrLn cfg $ "[Rib] Watching " <> toFilePath inputDir <> " for changes"
  onSrcChange $ runShakeBuild cfg buildAction
  where
    onSrcChange f = do
      workDir <- getCurrentDir
      -- Top-level directories to ignore from notifications
      dirBlacklist <- traverse makeAbsolute [shakeDbDir, inputDir </> [reldir|.git|]]
      let isBlacklisted :: FilePath -> Bool
          isBlacklisted p = or $ flip fmap dirBlacklist $ \b -> toFilePath b `isPrefixOf` p
      onTreeChange inputDir $ \allEvents -> do
        let events = filter (not . isBlacklisted . eventPath) allEvents
        unless (null events) $ do
          -- Log the changed events for diagnosis.
          logEvent workDir `mapM_` events
          f
    logEvent workDir e = do
      eventRelPath <-
        if eventIsDirectory e
          then fmap toFilePath . makeRelative workDir =<< parseAbsDir (eventPath e)
          else fmap toFilePath . makeRelative workDir =<< parseAbsFile (eventPath e)
      logStrLn cfg $ eventLogPrefix e <> " " <> eventRelPath
    eventLogPrefix = \case
      -- Single character log prefix to indicate file actions is a convention in Rib.
      Added _ _ _ -> "A"
      Modified _ _ _ -> "M"
      Removed _ _ _ -> "D"
      Unknown _ _ _ -> "?"
