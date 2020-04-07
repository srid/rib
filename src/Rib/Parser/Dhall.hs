{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Parser for Dhall configuration files.
--
-- Use `Dhall.TH.makeHaskellTypes` to create the Haskell type first. And then
-- call `parse` from your Shake action.
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

-- | Parse a Dhall file as Haskell type.
parse ::
  FromDhall a =>
  -- | Dependent .dhall files, which must trigger a rebuild
  [Path Rel File] ->
  -- | The Dhall file to parse. Relative to `ribInputDir`.
  Path Rel File ->
  Action a
parse (map toFilePath -> deps) f = do
  inputDir <- ribInputDir
  need deps
  s <- toText <$> readFile' (toFilePath $ inputDir </> f)
  liftIO $ withCurrentDirectory (toFilePath inputDir) $
    input auto s
