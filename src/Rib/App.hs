{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Rib.App
  ( App(..)
  , run
  , runWith
  , dev
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad (forever, void, when)

import System.Console.CmdArgs (Data, Typeable, auto, cmdArgs, help, modes, (&=))
import System.FSNotify (watchTree, withManager)

import qualified Rib.Server as Server
import qualified Rib.Settings as S
import qualified Rib.Shake as Shake

data App
  = Watch
  | Serve { port :: Int, watch :: Bool }
  | Generate { force :: Bool }
  deriving (Data,Typeable,Show,Eq)

cli :: App
cli = modes
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

-- | CLI entry point for running the Rib app
run :: S.Settings page -> IO ()
run cfg = runWith cfg =<< cmdArgs cli

-- | Run development server that watches and generates files in addition to
-- serving them.
--
-- This should be used with ghcid's `-T` argument.
dev :: S.Settings page -> IO ()
dev cfg = runWith cfg $ Serve devPort True
  where
    devPort = 8080

-- | Like `run` but uses the given `App` mode instead of reading it from CLI
-- arguments.
runWith :: S.Settings page -> App -> IO ()
runWith cfg = \case
  Watch -> withManager $ \mgr -> do
    -- Begin with a *full* generation as the HTML layout may have been changed.
    runWith cfg $ Generate True
    -- And then every time a file changes under the content directory.
    void $ watchTree mgr (S.contentDir cfg) (const True) $ const $
      runWith cfg $ Generate False
    -- Wait forever, effectively.
    forever $ threadDelay maxBound

  Serve p w -> concurrently_
    (when w $ runWith cfg Watch)
    (Server.serve p $ S.destDir cfg)

  Generate forceGen ->
    Shake.ribShake forceGen cfg
