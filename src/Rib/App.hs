{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CLI interface for Rib.
--
-- Typically you would call `Rib.App.run` passing your Shake build action.
module Rib.App
  ( App(..)
  , run
  , runWith
  , ribOutputDir
  , ribInputDir
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad
import Data.Bool (bool)

import Development.Shake
import Development.Shake.Forward (shakeForward)
import System.Console.CmdArgs
import System.FSNotify (watchTree, withManager)

import qualified Rib.Server as Server

data App
  = Watch
  | Serve { port :: Int, dontWatch :: Bool }
  | Generate { force :: Bool }
  deriving (Data,Typeable,Show,Eq)

-- | The path where static files will be generated.
--
-- Rib's server uses this directory when serving files.
ribOutputDir :: FilePath
ribOutputDir = "b"

-- | Directory from which source content will be read.
ribInputDir :: FilePath
ribInputDir = "a"
-- NOTE: ^ This should ideally *not* be `"."` as our use of watchTree (of
-- `runWith`) can interfere with Shake's file scaning.

-- | Run Rib using arguments passed in the command line.
run
  :: Action ()
  -- ^ Shake build rules for building the static site
  -> IO ()
run buildAction = runWith buildAction =<< cmdArgs ribCli
  where
    ribCli = modes
      [ Serve
          { port = 8080 &= help "Port to bind to"
          , dontWatch = False &= help "Do not watch in addition to serving generated files"
          } &= help "Serve the generated site"
            &= auto
      , Watch
          &= help "Watch for changes and generate"
      , Generate
          { force = False &= help "Force generation of all files"
          } &= help "Generate the site"
      ]

-- | Like `run` but with an explicitly passed `App` mode
runWith :: Action () -> App -> IO ()
runWith buildAction = \case
  Watch -> withManager $ \mgr -> do
    -- Begin with a *full* generation as the HTML layout may have been changed.
    runWith buildAction $ Generate True
    -- And then every time a file changes under the current directory
    putStrLn $ "[Rib] Watching " <> ribInputDir
    void $ watchTree mgr ribInputDir (const True) $ const $
      runWith buildAction $ Generate False
    -- Wait forever, effectively.
    forever $ threadDelay maxBound

  Serve p dw -> concurrently_
    (unless dw $ runWith buildAction Watch)
    (Server.serve p ribOutputDir)

  Generate forceGen ->
    let opts = shakeOptions
          { shakeVerbosity = Chatty
          , shakeRebuild = bool [] [(RebuildNow, "**")] forceGen
          }
    in shakeForward opts buildAction
