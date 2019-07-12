{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Rib.Shake
  ( ribShake
  , parsePandocCached
  ) where

import Data.Aeson (decode, encode)
import Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe
import qualified Data.Text.Encoding as T

import Development.Shake
import Development.Shake.Forward (cacheAction, shakeForward)
import Text.Pandoc (Pandoc)

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

parsePandocCached :: S.Settings page -> FilePath -> Action Pandoc
parsePandocCached cfg f =
  jsonCacheAction f $ parsePandoc $ S.parsePage cfg
  where
    jsonCacheAction k =
      fmap (fromMaybe (error "cache error") . decode) . cacheAction k . fmap encode
    parsePandoc parse =
      parse . T.decodeUtf8 . BS8.pack <$> readFile' f
