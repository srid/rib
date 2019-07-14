{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Rib.App
  ( App(..)
  , run
  , runWith
  , ribOutputDir
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad (forever, void, when)

import System.Console.CmdArgs
import System.FSNotify (watchTree, withManager)

import qualified Rib.Server as Server
import qualified Rib.Shake as Shake

data App
  = Watch
  | Serve { port :: Int, watch :: Bool }
  | Generate { force :: Bool }
  deriving (Data,Typeable,Show,Eq)

ribOutputDir :: FilePath
ribOutputDir = ".riboutput"

-- | CLI entry point for running the Rib app
run :: Shake.RibAction page -> IO ()
run action = runWith action =<< cmdArgs ribCli
  where
    ribCli = modes
      [ Watch
          &= help "Watch for changes and generate"
      , Serve
          { port = 8080 &= help "Port to bind to"
          , watch = False &= help "Watch in addition to serving generated files"
          } &= help "Serve the generated site"
      , Generate
          { force = False &= help "Force generation of all files"
          } &= help "Generate the site"
            &= auto  -- Generate is the default command.
      ]

-- | Like `run` but uses the given `App` mode instead of reading it from CLI
-- arguments.
runWith :: Shake.RibAction page -> App -> IO ()
runWith action = \case
  Watch -> withManager $ \mgr -> do
    -- Begin with a *full* generation as the HTML layout may have been changed.
    runWith action $ Generate True
    -- And then every time a file changes under the current directory
    void $ watchTree mgr "." (const True) $ const $
      runWith action $ Generate False
    -- Wait forever, effectively.
    forever $ threadDelay maxBound

  Serve p w -> concurrently_
    (when w $ runWith action Watch)
    (Server.serve p ribOutputDir)

  Generate forceGen ->
    Shake.ribShake forceGen action
