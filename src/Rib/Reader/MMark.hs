{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rib.Reader.MMark
  ( MMark
  )
where

import Data.Aeson
import Text.MMark (MMark)
import qualified Text.MMark as MMark
import qualified Text.Megaparsec   as M

import Rib.Reader


instance FromJSON meta => RibReader MMark meta where
  readDoc s = case MMark.parse "TODO" s of
    -- TODO: Don't error here!
    Left e -> error $ M.errorBundlePretty e
    Right v -> (v, ) $ do
      meta <- MMark.projectYaml v
      case fromJSON meta of
        Success v -> Just v
        _ -> Nothing

  renderDoc = MMark.render . fst
  -- getMetadata doc = do
  --   meta <- MMark.projectYaml doc
  --   case fromJSON meta of
  --     Success v -> Just v
  --     _ -> Nothing
