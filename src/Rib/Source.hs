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
    sourceUrl,
    sourceVal,
  )
where

import qualified Data.Text as T
import Development.Shake (Action)
import Path
import Relude

-- | A source file on disk
data Source repr
  = Source
      { _source_path :: Path Rel File,
        -- | Path to the generated HTML file (relative to `Rib.Shake.ribOutputDir`)
        _source_builtPath :: Path Rel File,
        _source_val :: repr
      }
  deriving (Generic, Functor)

-- | Path to the source file (relative to `Rib.Shake.ribInputDir`)
sourcePath :: Source repr -> Path Rel File
sourcePath = _source_path

-- | Relative URL to the generated source HTML.
sourceUrl :: Source repr -> Text
sourceUrl = stripIndexHtml . relPathToUrl . _source_builtPath
  where
    relPathToUrl = toText . toFilePath . ([absdir|/|] </>)
    stripIndexHtml s =
      if T.isSuffixOf "index.html" s
        then T.dropEnd (T.length $ "index.html") s
        else s

-- | Parsed representation of the source.
sourceVal :: Source repr -> repr
sourceVal = _source_val

-- | A function that parses a source representation out of the given file
type SourceReader repr = forall b. Path b File -> Action (Either Text repr)
