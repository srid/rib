{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rib.Markup.MMark
  ( MMark
  )
where

import Data.Text (Text)
import qualified Data.Text as T

import Text.MMark (MMark)
import qualified Text.MMark as MMark
import qualified Text.Megaparsec as M
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Rib.Markup


instance Markup MMark where
  type MarkupError MMark = M.ParseErrorBundle Text MMark.MMarkErr
  readDoc f s = case MMark.parse f s of
    Left e -> Left e
    Right v -> Right $ Document f v $ MMark.projectYaml v

  readDocIO k f = do
    content <- T.decodeUtf8 <$> BS.readFile f
    pure $ readDoc k content

  renderDoc = MMark.render . _document_val

  showMarkupError = T.pack . M.errorBundlePretty
