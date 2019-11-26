{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Suppressing orphans warning for `Markup MMark` instance

module Rib.Markup.MMark
  ( -- * Manual rendering
    renderMarkdown,

    -- * Extracting information
    getFirstImg,

    -- * Re-exports
    MMark,
  )
where

import Control.Foldl (Fold (..))
import Control.Monad.Except
import Lucid (Html)
import Named
import Path
import Rib.Markup
import Text.MMark (MMark)
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension as Ext
import qualified Text.MMark.Extension.Common as Ext
import qualified Text.Megaparsec as M
import Text.URI (URI)

instance Markup MMark where

  type MarkupError MMark = M.ParseErrorBundle Text MMark.MMarkErr

  parseDoc f s = case MMark.parse (toFilePath f) s of
    Left e -> Left e
    Right doc ->
      let doc' = MMark.useExtensions exts $ useTocExt doc
          meta = MMark.projectYaml doc
       in Right $ Document f doc' (MMark.render doc') meta

  readDoc (Arg k) (Arg f) = do
    content <- readFileText (toFilePath f)
    pure $ parseDoc k content

  showMarkupError = toText . M.errorBundlePretty

-- | Parse and render the markup directly to HTML
renderMarkdown :: Text -> Html ()
renderMarkdown s = either (error . showMarkupError @MMark) id $ runExcept $ do
  doc <- liftEither $ parseDoc @MMark [relfile|<memory>.md|] s
  pure $ MMark.render $ _document_val doc

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
      Ext.Naked xs -> toList xs
      Ext.Paragraph xs -> toList xs
      _ -> []

exts :: [MMark.Extension]
exts =
  [ Ext.fontAwesome,
    Ext.footnotes,
    Ext.kbd,
    Ext.linkTarget,
    Ext.mathJax (Just '$'),
    Ext.obfuscateEmail "protected-email",
    Ext.punctuationPrettifier,
    Ext.ghcSyntaxHighlighter,
    Ext.skylighting
  ]

useTocExt :: MMark -> MMark
useTocExt doc = MMark.useExtension (Ext.toc "toc" toc) doc
  where
    toc = MMark.runScanner doc $ Ext.tocScanner (\x -> x > 1 && x < 5)
