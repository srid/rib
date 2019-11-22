{-# LANGUAGE OverloadedStrings #-}

module Rib.Reader.MMark where

import Data.Aeson (fromJSON, Result(..))
import Text.MMark (MMark)
import qualified Text.MMark as MMark
import qualified Text.Megaparsec   as M

import Rib.Reader


instance RibReader MMark where
  readDoc s = case MMark.parse "TODO" s of
    -- TODO: Don't error here!
    Left e -> error $ M.errorBundlePretty e
    Right v -> v

  renderDoc = MMark.render
  getMetadata doc = do
    meta <- MMark.projectYaml doc
    case fromJSON meta of
      Success v -> Just v
      _ -> Nothing
