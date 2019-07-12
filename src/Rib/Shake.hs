{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Rib.Shake
  ( ribShake
  , parsePandocCached
  ) where

import Data.Aeson
import Data.Bool (bool)
import Data.Maybe

import Development.Shake
import Development.Shake.Forward (cacheAction, shakeForward)

import qualified Rib.Settings as S


ribShake
  :: Bool
  -- ^ Force generate of requested targes
  -> S.Settings page
  -- ^ Site settings
  -> IO ()
ribShake forceGen cfg = do
  let opts = shakeOptions
        { shakeVerbosity = Chatty
        , shakeRebuild = bool [] ((RebuildNow,) <$> S.rebuildPatterns cfg) forceGen
        }
  shakeForward opts $
    S.buildRules cfg cfg

parsePandocCached :: (FromJSON page, ToJSON page) => S.Settings page -> FilePath -> Action page
parsePandocCached cfg f =
  jsonCacheAction f $ S.parsePage cfg f
  where
    jsonCacheAction k =
      fmap (fromMaybe (error "cache error") . decode) . cacheAction k . fmap encode
