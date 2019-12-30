{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CLI interface for Rib.
--
-- Typically you would call `Rib.App.run` passing your Shake build action.
module Rib.App
  ( App (..),
    run,
    runWith,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Exception (catch)
import Development.Shake
import Development.Shake.Forward (shakeForward)
import Path
import qualified Rib.Server as Server
import Rib.Shake (Dirs (..))
import System.Console.CmdArgs
import System.FSNotify (watchTree, withManager)

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
  --
  -- NOTE: This should ideally *not* be `"."` as our use of watchTree (of
  -- `runWith`) can interfere with Shake's file scaning.
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
runWith src dst buildAction = \case
  WatchAndGenerate -> withManager $ \mgr -> do
    -- Begin with a *full* generation as the HTML layout may have been changed.
    runWith src dst buildAction $ Generate True
    -- And then every time a file changes under the current directory
    putStrLn $ "[Rib] Watching " <> toFilePath src
    void $ watchTree mgr (toFilePath src) (const True) $ \_ -> do
      runWith src dst buildAction (Generate False)
        `catch` \(e :: SomeException) -> putStrLn $ show e
    -- Wait forever, effectively.
    forever $ threadDelay maxBound
  Serve p dw ->
    concurrently_
      (unless dw $ runWith src dst buildAction WatchAndGenerate)
      (Server.serve p $ toFilePath dst)
  Generate fullGen ->
    flip shakeForward buildAction $
      shakeOptions
        { shakeVerbosity = Verbose,
          shakeRebuild = bool [] [(RebuildNow, "**")] fullGen,
          shakeLintInside = [""],
          shakeExtra = addShakeExtra (Dirs (src, dst)) (shakeExtra shakeOptions)
        }
