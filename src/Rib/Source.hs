{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rib.Source
  ( -- * Source type
    Source (..),
    SourceReader,

    -- * Source properties
    sourcePath,
    sourceVal,
    sourceUrl,
  )
where

import Development.Shake (Action)
import Path
import Relude

-- | A source file on disk
data Source repr
  = Source
      { -- | Path to the source; relative to the source directory.
        _source_path :: Path Rel File,
        -- | Relative URL (begins with `/`) to the generated HTML file
        _source_url :: Text,
        -- | Parsed representation of the source.
        _source_val :: repr
      }
  deriving (Generic, Functor)

sourcePath :: Source repr -> Path Rel File
sourcePath = _source_path

sourceUrl :: Source repr -> Text
sourceUrl = _source_url

sourceVal :: Source repr -> repr
sourceVal = _source_val

-- | A function that parses a source representation out of the given file
type SourceReader repr = forall b. Path b File -> Action (Either Text repr)
