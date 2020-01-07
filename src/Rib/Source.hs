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

import qualified Data.Text as T
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
sourceUrl = stripIndexHtml . _source_url
  where
    stripIndexHtml s =
      if T.isSuffixOf "index.html" s
        then T.dropEnd (T.length $ "index.html") s
        else s

sourceVal :: Source repr -> repr
sourceVal = _source_val

-- | A function that parses a source representation out of the given file
type SourceReader repr = forall b. Path b File -> Action (Either Text repr)
