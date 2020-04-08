{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Rib.Settings
  ( RibSettings (..),
    logStrLn,
    logErr,
  )
where

import Development.Shake (Verbosity (..))
import Path
import Relude
import System.IO (hPutStrLn)

-- | The settings with which Rib is run
--
-- RibSettings is initialized with the values passed to `Rib.App.run`
data RibSettings
  = RibSettings
      { _ribSettings_inputDir :: Path Rel Dir,
        _ribSettings_outputDir :: Path Rel Dir,
        -- | Shake verbosity level
        _ribSettings_verbosity :: Verbosity,
        -- | Whether we must try to generate all files even if they have not
        -- been modified since last generation.
        _ribSettings_fullGen :: Bool
      }
  deriving (Typeable)

logStrLn :: MonadIO m => RibSettings -> String -> m ()
logStrLn RibSettings {..} s =
  unless (_ribSettings_verbosity == Silent) $ do
    putStrLn s

logErr :: MonadIO m => String -> m ()
logErr s =
  liftIO $ hPutStrLn stderr s
