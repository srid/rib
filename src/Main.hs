{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad (void, when, forever)
import Data.Default (def)

import System.Console.CmdArgs (Data, Typeable, auto, cmdArgs, help, modes, (&=))
import System.FSNotify (watchTree, withManager)

import qualified Rib.Server as Server
import qualified Rib.Shake as Shake

import qualified Settings

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
        &= auto  -- | Generate is the default command.
  ]

main :: IO ()
main = runApp def =<< cmdArgs cli

runApp :: Settings.Settings -> App -> IO ()
runApp cfg = \case
  Watch -> withManager $ \mgr -> do
    -- Begin with a *full* generation as the HTML layout may have been changed.
    runApp cfg $ Generate True
    -- And then every time a file changes under the content directory.
    void $ watchTree mgr (Settings.contentDir cfg) (const True) $ const $ runApp cfg $ Generate False
    -- Wait forever, effectively.
    forever $ threadDelay maxBound

  Serve p w -> concurrently_
    (when w $ runApp cfg Watch)
    $ Server.serve p $ Settings.destDir cfg

  Generate forceGen ->
    Shake.ribShake forceGen cfg
