{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CLI interface for Rib.
--
-- Mostly you would only need `Rib.App.run`, passing it your Shake build action.
module Rib.App
  ( Command (..),
    commandParser,
    run,
    runWith,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception.Safe (catch)
import Development.Shake hiding (command)
import Development.Shake.Forward (shakeForward)
import Options.Applicative
import Path
import Path.IO
import Relude
import qualified Rib.Server as Server
import Rib.Shake (RibSettings (..))
import Rib.Watch (onTreeChange)
import System.FSNotify (Event (..), eventIsDirectory, eventPath)
import System.IO (BufferMode (LineBuffering), hSetBuffering)

-- | Rib CLI commands
data Command
  = -- | Generate static files once.
    Generate
      { -- | Force a full generation of /all/ files even if they were not modified
        full :: Bool
      }
  | -- | Watch for changes in the input directory and run `Generate`
    Watch
  | -- | Run a HTTP server serving content from the output directory
    Serve
      { -- | Port to bind the server
        port :: Int,
        -- | Unless set run `WatchAndGenerate` automatically
        dontWatch :: Bool
      }
  deriving (Show, Eq, Generic)

-- | Commandline parser `Parser` for the Rib CLI
commandParser :: Parser Command
commandParser =
  hsubparser $
    mconcat
      [ command "generate" $ info generateCommand $ progDesc "Run one-off generation of static files",
        command "watch" $ info watchCommand $ progDesc "Watch the source directory, and generate when it changes",
        command "serve" $ info serveCommand $ progDesc "Like watch, but also starts a HTTP server"
      ]
  where
    generateCommand =
      Generate <$> switch (long "full" <> help "Do a full generation (toggles shakeRebuild)")
    watchCommand =
      pure Watch
    serveCommand =
      Serve
        <$> option auto (long "port" <> short 'p' <> help "HTTP server port" <> showDefault <> value 8080 <> metavar "PORT")
        <*> switch (long "no-watch" <> help "Serve only; don't watch and regenerate")

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
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Rib static site generator CLI"
        )

-- | Like `run` but with an explicitly passed `Command`
runWith :: Path Rel Dir -> Path Rel Dir -> Action () -> Command -> IO ()
runWith src dst buildAction ribCmd = do
  when (src == currentRelDir) $
    -- Because otherwise our use of `watchTree` can interfere with Shake's file
    -- scaning.
    fail "cannot use '.' as source directory."
  -- For saner output
  flip hSetBuffering LineBuffering `mapM_` [stdout, stderr]
  case ribCmd of
    Generate fullGen ->
      -- FIXME: Shouldn't `catch` Shake exceptions when invoked without fsnotify.
      runShake fullGen
    Watch ->
      runShakeAndObserve
    Serve p dw -> do
      race_ (Server.serve p $ toFilePath dst) $ do
        if dw
          then threadDelay maxBound
          else runShakeAndObserve
  where
    currentRelDir = [reldir|.|]
    -- Keep shake database directory under the src directory instead of the
    -- (default) current working directory, which may not always be a project
    -- root (as in the case of neuron).
    shakeDatabaseDir :: Path Rel Dir = src </> [reldir|.shake|]
    runShakeAndObserve = do
      -- Begin with a *full* generation as the HTML layout may have been changed.
      -- TODO: This assumption is not true when running the program from compiled
      -- binary (as opposed to say via ghcid) as the HTML layout has become fixed
      -- by being part of the binary. In this scenario, we should not do full
      -- generation (i.e., toggle the bool here to False). Perhaps provide a CLI
      -- flag to disable this.
      runShake True
      -- And then every time a file changes under the current directory
      putStrLn $ "[Rib] Watching " <> toFilePath src <> " for changes"
      onSrcChange $ runShake False
    runShake fullGen = do
      putStrLn $ "[Rib] Generating " <> toFilePath src <> " (full=" <> show fullGen <> ")"
      let settings = RibSettings src dst
      shakeForward (ribShakeOptions settings fullGen) buildAction
        -- Gracefully handle any exceptions when running Shake actions. We want
        -- Rib to keep running instead of crashing abruptly.
        `catch` \(e :: ShakeException) ->
          putStrLn $
            "[Rib] Unhandled exception when building " <> shakeExceptionTarget e <> ": " <> show e
    ribShakeOptions settings fullGen =
      shakeOptions
        { shakeVerbosity = Verbose,
          shakeFiles = toFilePath shakeDatabaseDir,
          shakeRebuild = bool [] [(RebuildNow, "**")] fullGen,
          shakeLintInside = [""],
          shakeExtra = addShakeExtra settings (shakeExtra shakeOptions)
        }
    onSrcChange f = do
      workDir <- getCurrentDir
      -- Top-level directories to ignore from notifications
      dirBlacklist <- traverse makeAbsolute [shakeDatabaseDir, src </> [reldir|.git|]]
      let isBlacklisted :: FilePath -> Bool
          isBlacklisted p = or $ flip fmap dirBlacklist $ \b -> toFilePath b `isPrefixOf` p
      onTreeChange src $ \allEvents -> do
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
      putStrLn $ eventLogPrefix e <> " " <> eventRelPath
    eventLogPrefix = \case
      -- Single character log prefix to indicate file actions is a convention in Rib.
      Added _ _ _ -> "A"
      Modified _ _ _ -> "M"
      Removed _ _ _ -> "D"
      Unknown _ _ _ -> "?"
