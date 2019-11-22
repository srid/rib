{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rib.Reader.MMark
  ( MMark
  )
where

import qualified Data.Text as T

import Text.MMark (MMark)
import qualified Text.MMark as MMark
import qualified Text.Megaparsec   as M
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Rib.Reader


instance Markup MMark where
  readDoc f s = case MMark.parse f s of
    Left e -> Left $ T.pack $ M.errorBundlePretty e
    Right v -> Right $ Document f v $ MMark.projectYaml v

  readDocIO k f = do
    content <- T.decodeUtf8 <$> BS.readFile f
    pure $ readDoc k content

  renderDoc = MMark.render . _document_val
