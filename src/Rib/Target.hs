{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Target
  ( -- * Target type
    Target (..),

    -- * Target properties
    targetPath,
    targetUrl,
    targetVal,
  )
where

import qualified Data.Text as T
import Path
import Relude

-- | A generated file on disk
data Target a
  = Target
      { _target_path :: Path Rel File,
        -- ^ Path to the generated HTML file (relative to `Rib.Shake.ribOutputDir`)
        _target_val :: a
      }
  deriving (Generic, Functor, Show)

-- | Path to the source file (relative to `Rib.Shake.ribInputDir`)
targetPath :: Target repr -> Path Rel File
targetPath = _target_path

-- | Parsed representation of the source.
targetVal :: Target repr -> repr
targetVal = _target_val

-- | Relative URL to the generated source HTML.
targetUrl :: Target repr -> Text
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

