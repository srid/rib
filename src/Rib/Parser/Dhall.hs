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
import System.Directory

-- | Parse Dhall files
parse ::
  FromDhall a =>
  -- | Dependent .dhall files, which must trigger a rebuild
  [Path Rel File] ->
  Path Rel File ->
  Action a
parse (map toFilePath -> deps) f = do
  inputDir <- ribInputDir
  need deps
  s <- toText <$> readFile' (toFilePath $ inputDir </> f)
  liftIO $ withCurrentDirectory (toFilePath inputDir) $
    input auto s
