{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Parsing Dhall files
module Rib.Parser.Dhall
  ( -- * Parsing
    parse,
  )
where

import Development.Shake
import Dhall (FromDhall, auto, input)
import Path
import Relude
import Rib.Shake (ribInputDir)
import Rib.Source (SourceReader)
import System.Directory

-- | `SourceReader` for parsing Dhall files
parse ::
  FromDhall a =>
  -- | Dependent .dhall files, which must trigger a rebuild
  [Path Rel File] ->
  SourceReader a
parse (map toFilePath -> deps) (toFilePath -> f) = do
  inputDir <- toFilePath <$> ribInputDir
  need deps
  s <- toText <$> readFile' f
  liftIO $ withCurrentDirectory inputDir $
    Right <$> input auto s
