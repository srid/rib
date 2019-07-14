{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Shake
  ( ribShake
  , jsonCacheAction
  , Action
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Binary
import Data.Bool (bool)
import Data.Maybe
import Data.Typeable

import Development.Shake
import Development.Shake.Forward (cacheAction, shakeForward)


-- TODO: Should we get rid of this, and have the user directly call shakeForward?
-- As this way we can get rid of the "framework" feel in Main.hs
ribShake
  :: Bool
  -- ^ Force generate of requested targets
  -> Action ()
  -- ^ Site build action
  -> IO ()
ribShake forceGen buildAction = do
  let opts = shakeOptions
        { shakeVerbosity = Chatty
        , shakeRebuild = bool [] [(RebuildNow, "**")] forceGen
        }
  shakeForward opts buildAction

-- | Like `Development.Shake.cacheAction` but uses JSON instance instead of Typeable / Binary on `b`.
jsonCacheAction :: (FromJSON b, Typeable k, Binary k, Show k, ToJSON a) => k -> Action a -> Action b
jsonCacheAction k =
    fmap (fromMaybe (error "cache error") . Aeson.decode)
  . cacheAction k
  . fmap Aeson.encode
