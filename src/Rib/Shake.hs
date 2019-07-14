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
import Data.Bool (bool)
import Data.Maybe
import Data.Typeable
import Data.Binary

import Development.Shake
import Development.Shake.Forward (cacheAction, shakeForward)


ribShake
  :: Bool
  -- ^ Force generate of requested targes
  -> Action ()
  -- ^ Site build action
  -> IO ()
ribShake forceGen buildAction = do
  let opts = shakeOptions
        { shakeVerbosity = Chatty
        , shakeRebuild = bool [] [(RebuildNow, "**")] forceGen
        }
  shakeForward opts buildAction

-- jsonCacheAction :: (FromJSON b, ToJSON b, Typeable a, Binary a, Show a, Typeable b, Binary b, Show b) => a -> Action b -> Action b
jsonCacheAction :: (FromJSON b, Typeable k, Binary k, Show k, ToJSON a) => k -> Action a -> Action b
jsonCacheAction k =
    fmap (fromMaybe (error "cache error") . Aeson.decode)
  . cacheAction k
  . fmap Aeson.encode
