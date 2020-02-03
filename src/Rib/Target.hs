{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Target
  ( -- * Target type
    Target,
    mkTarget,
    mkTargetWithSource,

    -- * Target properties
    targetPath,
    targetUrl,
    targetVal,
    targetSrc,
  )
where

import qualified Data.Text as T
import Path
import Relude

-- | A generated file on disk
data Target src a
  = Target
      { _target_src :: src,
        -- | Path to the generated HTML file (relative to `Rib.Shake.ribOutputDir`)
        _target_path :: Path Rel File,
        _target_val :: a
      }
  deriving (Generic, Functor, Show)

mkTarget :: Path Rel File -> a -> Target () a
mkTarget = Target ()

mkTargetWithSource :: src -> Path Rel File -> a -> Target src a
mkTargetWithSource = Target

-- | Path to the source file (relative to `Rib.Shake.ribInputDir`)
targetPath :: Target src a -> Path Rel File
targetPath = _target_path

targetSrc :: Target src a -> src
targetSrc = _target_src

-- | Parsed representation of the source.
targetVal :: Target src a -> a
targetVal = _target_val

-- | Relative URL to the generated source HTML.
targetUrl :: Target src a -> Text
targetUrl = urlForPath . _target_path

-- | Given a path to a HTML file, return its relative URL
urlForPath :: Path Rel File -> Text
urlForPath = stripIndexHtml . relPathToUrl
  where
    relPathToUrl = toText . toFilePath . ([absdir|/|] </>)
    stripIndexHtml s =
      if T.isSuffixOf "index.html" s
        then T.dropEnd (T.length $ "index.html") s
        else s
