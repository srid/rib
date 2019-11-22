{-# OPTIONS_GHC -fno-warn-orphans #-}  -- for `Markup MMark` instance
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Rib.Markup.MMark
  ( MMark
  )
where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Named

import qualified Text.Megaparsec as M
import Text.MMark (MMark)
import qualified Text.MMark as MMark

import Rib.Markup


instance Markup MMark where
  type MarkupError MMark = M.ParseErrorBundle Text MMark.MMarkErr

  parseDoc f s = case MMark.parse f s of
    Left e -> Left e
    Right v -> Right $ Document f v $ MMark.projectYaml v

  readDoc (Arg k) (Arg f) = do
    content <- T.decodeUtf8 <$> BS.readFile f
    pure $ parseDoc k content

  renderDoc = MMark.render . _document_val

  showMarkupError = T.pack . M.errorBundlePretty
