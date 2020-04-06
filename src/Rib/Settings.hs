{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Settings for rib
module Rib.Settings
  ( RibSettings (..),
  )
where

import Development.Shake (Verbosity)
import Path
import Relude

-- | RibSettings is initialized with the values passed to `Rib.App.run`
data RibSettings
  = RibSettings
      { _ribSettings_inputDir :: Path Rel Dir,
        _ribSettings_outputDir :: Path Rel Dir,
        _ribSettings_verbosity :: Verbosity,
        -- | Whether we must try to generate all files even if they have not
        -- been modified since last generation.
        _ribSettings_fullGen :: Bool
      }
  deriving (Typeable)
