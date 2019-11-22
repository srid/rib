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
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Rib.Reader


instance FromJSON meta => RibReader MMark meta where
  readDoc f s = case MMark.parse f s of
    -- TODO: Don't error here!
    Left e -> error $ M.errorBundlePretty e
    Right v -> Document f v $ getMetadata v

  readDocIO k f = do
    content <- T.decodeUtf8 <$> BS.readFile f
    pure $ readDoc k content

  renderDoc = MMark.render . _document_doc

getMetadata :: FromJSON meta => MMark -> Maybe meta
getMetadata v = do
  meta <- MMark.projectYaml v
  case fromJSON meta of
    Success x -> Just x
    _ -> Nothing
