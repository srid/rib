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
import Relude
import Rib.Shake (ribInputDir)
import System.Directory
import System.FilePath

-- | Parse a Dhall file as Haskell type.
parse ::
  FromDhall a =>
  -- | Dependent .dhall files, which must trigger a rebuild
  [FilePath] ->
  -- | The Dhall file to parse. Relative to `ribInputDir`.
  FilePath ->
  Action a
parse deps f = do
  inputDir <- ribInputDir
  need deps
  s <- toText <$> readFile' (inputDir </> f)
  liftIO $ withCurrentDirectory inputDir $
    input auto s
