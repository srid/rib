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
  ( CliConfig (..),
    cliParser,
    run,
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
import qualified Rib.Server as Server
import Rib.Settings (RibSettings (..), logErr, logStrLn)
import Rib.Watch (onTreeChange)
import System.FSNotify (Event (..), eventIsDirectory, eventPath)
import System.IO (BufferMode (LineBuffering), hSetBuffering)

data CliConfig
  = CliConfig
      { rebuildAll :: Bool,
        watch :: Bool,
        serve :: Maybe Int,
        quiet :: Bool
      }
  deriving (Show, Eq, Generic)

cliParser :: Parser CliConfig
cliParser =
  CliConfig
    <$> switch (long "rebuild-all" <> help "Rebuild all sources")
    <*> switch (long "watch" <> short 'w' <> help "Watch for changes and regenerate")
    <*> optional
      (option auto (long "serve" <> short 's' <> metavar "PORT" <> help "Run a HTTP server on the generated directory"))
    <*> switch (long "quiet" <> help "Log nothing")

-- | Run Rib using arguments passed in the command line.
run ::
  -- | Directory from which source content will be read.
  Path Rel Dir ->
  -- | The path where static files will be generated.  Rib's server uses this
  -- directory when serving files.
  Path Rel Dir ->
  -- | Shake build rules for building the static site
  Action () ->
  IO ()
run src dst buildAction = runWith src dst buildAction =<< execParser opts
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "Rib static site generator CLI"
        )

-- | Like `run` but with an explicitly passed `CliConfig`
runWith :: Path Rel Dir -> Path Rel Dir -> Action () -> CliConfig -> IO ()
runWith src dst buildAction CliConfig {..} = do
  when (src == currentRelDir) $
    -- Because otherwise our use of `watchTree` can interfere with Shake's file
    -- scaning.
    fail "cannot use '.' as source directory."
  -- For saner output
  flip hSetBuffering LineBuffering `mapM_` [stdout, stderr]
  let ribSettings = RibSettings src dst (bool Verbose Silent quiet) rebuildAll
  case (watch, serve) of
    (True, Just port) -> do
      race_
        (Server.serve ribSettings port $ toFilePath dst)
        (runShakeAndObserve ribSettings)
    (True, Nothing) ->
      runShakeAndObserve ribSettings
    (False, Just port) ->
      Server.serve ribSettings port $ toFilePath dst
    (False, Nothing) ->
      runShakeBuild ribSettings
  where
    currentRelDir = [reldir|.|]
    -- Keep shake database directory under the src directory instead of the
    -- (default) current working directory, which may not always be a project
    -- root (as in the case of neuron).
    shakeDatabaseDir :: Path Rel Dir = src </> [reldir|.shake|]
    runShakeAndObserve ribSettings = do
      -- Begin with a *full* generation as the HTML layout may have been changed.
      -- TODO: This assumption is not true when running the program from compiled
      -- binary (as opposed to say via ghcid) as the HTML layout has become fixed
      -- by being part of the binary. In this scenario, we should not do full
      -- generation (i.e., toggle the bool here to False). Perhaps provide a CLI
      -- flag to disable this.
      runShakeBuild $ ribSettings {_ribSettings_fullGen = True}
      -- And then every time a file changes under the current directory
      logStrLn ribSettings $ "[Rib] Watching " <> toFilePath src <> " for changes"
      onSrcChange ribSettings $ runShakeBuild ribSettings
    runShakeBuild ribSettings = do
      runShake ribSettings $ do
        logStrLn ribSettings $ "[Rib] Generating " <> toFilePath src <> " (full=" <> show (_ribSettings_fullGen ribSettings) <> ")"
        buildAction
    runShake ribSettings shakeAction = do
      shakeForward (shakeOptionsFrom ribSettings) shakeAction
        `catch` handleShakeException
    handleShakeException (e :: ShakeException) =
      -- Gracefully handle any exceptions when running Shake actions. We want
      -- Rib to keep running instead of crashing abruptly.
      logErr $
        "[Rib] Unhandled exception when building " <> shakeExceptionTarget e <> ": " <> show e
    shakeOptionsFrom settings =
      shakeOptions
        { shakeVerbosity = _ribSettings_verbosity settings,
          shakeFiles = toFilePath shakeDatabaseDir,
          shakeRebuild = bool [] [(RebuildNow, "**")] (_ribSettings_fullGen settings),
          shakeLintInside = [""],
          shakeExtra = addShakeExtra settings (shakeExtra shakeOptions)
        }
    onSrcChange ribSettings f = do
      workDir <- getCurrentDir
      -- Top-level directories to ignore from notifications
      dirBlacklist <- traverse makeAbsolute [shakeDatabaseDir, src </> [reldir|.git|]]
      let isBlacklisted :: FilePath -> Bool
          isBlacklisted p = or $ flip fmap dirBlacklist $ \b -> toFilePath b `isPrefixOf` p
      onTreeChange src $ \allEvents -> do
        let events = filter (not . isBlacklisted . eventPath) allEvents
        unless (null events) $ do
          -- Log the changed events for diagnosis.
          logEvent ribSettings workDir `mapM_` events
          f
    logEvent ribSettings workDir e = do
      eventRelPath <-
        if eventIsDirectory e
          then fmap toFilePath . makeRelative workDir =<< parseAbsDir (eventPath e)
          else fmap toFilePath . makeRelative workDir =<< parseAbsFile (eventPath e)
      logStrLn ribSettings $ eventLogPrefix e <> " " <> eventRelPath
    eventLogPrefix = \case
      -- Single character log prefix to indicate file actions is a convention in Rib.
      Added _ _ _ -> "A"
      Modified _ _ _ -> "M"
      Removed _ _ _ -> "D"
      Unknown _ _ _ -> "?"
