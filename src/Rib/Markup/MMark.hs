{-# OPTIONS_GHC -fno-warn-orphans #-}  -- for `Markup MMark` instance
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Rib.Markup.MMark
  (
  -- * Extracting information
    getFirstImg
  -- * Re-exports
  , MMark
  )
where

import Control.Applicative ((<|>))
import Control.Foldl hiding (head)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Named

import qualified Text.Megaparsec as M
import Text.MMark (MMark)
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension as Ext
import qualified Text.MMark.Extension.Common as Ext
import Text.URI (URI)

import Rib.Markup

instance Markup MMark where
  type MarkupError MMark = M.ParseErrorBundle Text MMark.MMarkErr

  parseDoc f s = case MMark.parse f s of
    Left e -> Left e
    Right doc ->
      let doc' = MMark.useExtensions exts $ useTocExt doc
          meta = MMark.projectYaml doc
      in Right $ Document f doc' meta

  readDoc (Arg k) (Arg f) = do
    content <- T.decodeUtf8 <$> BS.readFile f
    pure $ parseDoc k content

  renderDoc = MMark.render . _document_val

  showMarkupError = T.pack . M.errorBundlePretty


-- | Get the first image in the document if one exists
getFirstImg :: MMark -> Maybe URI
getFirstImg = flip MMark.runScanner $ Fold f Nothing id
  where
    f acc blk = acc <|> listToMaybe (mapMaybe getImgUri (inlinesContainingImg blk))
    getImgUri = \case
      Ext.Image _ uri _ -> Just uri
      _ -> Nothing
    inlinesContainingImg :: Ext.Bni -> [Ext.Inline]
    inlinesContainingImg = \case
      Ext.Naked xs -> NE.toList xs
      Ext.Paragraph xs -> NE.toList xs
      _ -> []

exts :: [MMark.Extension]
exts =
  [ Ext.fontAwesome
  , Ext.footnotes
  , Ext.kbd
  , Ext.linkTarget
  , Ext.mathJax (Just '$')
  , Ext.obfuscateEmail "protected-email"
  , Ext.punctuationPrettifier
  , Ext.ghcSyntaxHighlighter
  , Ext.skylighting
  ]

useTocExt :: MMark -> MMark
useTocExt doc = MMark.useExtension (Ext.toc "toc" toc) doc
  where
    toc = MMark.runScanner doc $ Ext.tocScanner (\x -> x > 1 && x < 5)
