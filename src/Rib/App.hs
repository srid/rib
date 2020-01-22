{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CLI interface for Rib.
--
-- Mostly you would only need `Rib.App.run`, passing it your Shake build action.
module Rib.App
  ( App (..),
    run,
    runWith,
  )
where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.Chan
import Control.Exception (catch)
import Development.Shake
import Development.Shake.Forward (shakeForward)
import Path
import Relude
import qualified Rib.Server as Server
import Rib.Shake (RibSettings (..))
import System.Console.CmdArgs
import System.FSNotify (watchTreeChan, withManager)
import System.IO (BufferMode (LineBuffering), hSetBuffering)

-- | Application modes
--
-- The mode in which to run the Rib CLI
data App
  = -- | Generate static files once.
    Generate
      { -- | Force a full generation of /all/ files even if they were not modified
        full :: Bool
      }
  | -- | Watch for changes in the input directory and run `Generate`
    WatchAndGenerate
  | -- | Run a HTTP server serving content from the output directory
    Serve
      { -- | Port to bind the server
        port :: Int,
        -- | Unless set run `WatchAndGenerate` automatically
        dontWatch :: Bool
      }
  deriving (Data, Typeable, Show, Eq)

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
run src dst buildAction = runWith src dst buildAction =<< cmdArgs ribCli
  where
    ribCli =
      modes
        [ Serve
            { port = 8080 &= help "Port to bind to",
              dontWatch = False &= help "Do not watch in addition to serving generated files"
            }
            &= help "Serve the generated site"
            &= auto,
          WatchAndGenerate
            &= help "Watch for changes and generate",
          Generate
            { full = False &= help "Force a full generation of all files"
            }
            &= help "Generate the site"
        ]

-- | Like `run` but with an explicitly passed `App` mode
runWith :: Path Rel Dir -> Path Rel Dir -> Action () -> App -> IO ()
runWith src dst buildAction app = do
  when (src == currentRelDir) $
    -- Because otherwise our use of `watchTree` can interfere with Shake's file
    -- scaning.
    fail "cannot use '.' as source directory."
  -- For saner output
  hSetBuffering stdout LineBuffering
  case app of
    WatchAndGenerate -> do
      -- Begin with a *full* generation as the HTML layout may have been changed.
      -- TODO: This assumption is not true when running the program from compiled
      -- binary (as opposed to say via ghcid) as the HTML layout has become fixed
      -- by being part of the binary. In this scenario, we should not do full
      -- generation (i.e., toggle the bool here to False). Perhaps provide a CLI
      -- flag to disable this.
      runShake True
      -- And then every time a file changes under the current directory
      onTreeChange src $
        runShake False
    Serve p dw ->
      concurrently_
        (unless dw $ runWith src dst buildAction WatchAndGenerate)
        (Server.serve p $ toFilePath dst)
    Generate fullGen -> do
      runShake fullGen
  where
    currentRelDir = [reldir|.|]
    runShake fullGen = do
      when fullGen
        $ putStrLn
        $ "[Rib] Running full generation of " <> toFilePath src
      shakeForward (ribShakeOptions fullGen) buildAction
        -- Gracefully handle any exceptions when running Shake actions. We want
        -- Rib to keep running instead of crashing abruptly.
        `catch` \(e :: SomeException) -> putStrLn $ "[Rib] Shake error: " <> show e
    ribShakeOptions fullGen =
      shakeOptions
        { shakeVerbosity = Verbose,
          shakeRebuild = bool [] [(RebuildNow, "**")] fullGen,
          shakeLintInside = [""],
          shakeExtra = addShakeExtra (RibSettings src dst) (shakeExtra shakeOptions)
        }
    onTreeChange fp f = do
      putStrLn $ "[Rib] Watching " <> toFilePath src <> " for changes"
      withManager $ \mgr -> do
        events <- newChan
        void $ watchTreeChan mgr (toFilePath fp) (const True) events
        forever $ do
          -- TODO: debounce
          void $ readChan events
          f
