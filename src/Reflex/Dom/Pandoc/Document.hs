{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Dom.Pandoc.Document
  ( elPandocDoc
  , elPandocInlines
  , elPandocHeading1
  ) where

import Control.Monad (forM_)
import qualified Data.Text as T

import Reflex.Dom.Core hiding (Link, Space)

import Text.Pandoc (Block (..), Inline (..), Pandoc (..))

import Reflex.Dom.Pandoc.SyntaxHighlighting (elCodeHighlighted)
import Reflex.Dom.Pandoc.Util (elPandocAttr, headerElement, renderAttr)

-- | Convert Markdown to HTML
--
-- NOTE: Not all Markdown features are supported yet.
-- TODO: Implement the notImplemented
elPandocDoc :: DomBuilder t m => Pandoc -> m ()
elPandocDoc (Pandoc _meta blocks) = mapM_ renderBlock blocks

-- | Render the first level of heading
elPandocHeading1 :: DomBuilder t m => Pandoc -> m ()
elPandocHeading1 (Pandoc _meta blocks) = forM_ blocks $ \case
  Header 1 _ xs -> elPandocInlines xs
  _ -> blank

-- | Render list of Pandoc inlines
--
-- Useful when dealing with metadata values
elPandocInlines :: DomBuilder t m => [Inline] -> m ()
elPandocInlines = mapM_ renderInline

renderBlock :: DomBuilder t m => Block -> m ()
renderBlock = \case
  Plain inlines -> mapM_ renderInline inlines
  Para xs -> el "p" $ mapM_ renderInline xs
  LineBlock xss -> forM_ xss $ \xs -> do
    mapM_ renderInline xs
    text "\n"
  CodeBlock attr x -> elCodeHighlighted attr x
  v@(RawBlock _ _) -> notImplemented v
  BlockQuote xs -> el "blockquote" $ mapM_ renderBlock xs
  OrderedList _lattr xss -> el "ol" $
    -- TODO: Implement list attributes.
    forM_ xss $ \xs -> el "li" $ mapM_ renderBlock xs
  BulletList xss -> el "ul" $ forM_ xss $ \xs -> el "li" $ mapM_ renderBlock xs
  DefinitionList defs -> el "dl" $ forM_ defs $ \(term, descList) -> do
    el "dt" $ mapM_ renderInline term
    forM_ descList $ \desc ->
      el "dd" $ mapM_ renderBlock desc
  Header level attr xs -> elPandocAttr (headerElement level) attr $ do
    mapM_ renderInline xs
  HorizontalRule -> el "hr" blank
  v@(Table _ _ _ _ _) -> notImplemented v
  Div attr xs -> elPandocAttr "div" attr $
    mapM_ renderBlock xs
  Null -> blank

renderInline :: DomBuilder t m => Inline -> m ()
renderInline = \case
  Str x -> text $ T.pack x
  Emph xs -> el "em" $ mapM_ renderInline xs
  Strong xs -> el "strong" $ mapM_ renderInline xs
  Strikeout xs -> el "strike" $ mapM_ renderInline xs
  Superscript xs -> el "sup" $ mapM_ renderInline xs
  Subscript xs -> el "sub" $ mapM_ renderInline xs
  SmallCaps xs -> el "small" $ mapM_ renderInline xs
  v@(Quoted _qt _xs) -> notImplemented v
  v@(Cite _ _) -> notImplemented v
  Code attr x -> elPandocAttr "code" attr $
    text $ T.pack x
  Space -> text " "
  SoftBreak -> text " "
  LineBreak -> text "\n"
  v@(Math _ _) -> notImplemented v
  v@(RawInline _ _) -> notImplemented v
  Link attr xs (lUrl, lTitle) -> do
    let attr' = renderAttr attr <> ("href" =: T.pack lUrl <> "title" =: T.pack lTitle)
    elAttr "a" attr' $ mapM_ renderInline xs
  Image attr xs (iUrl, iTitle) -> do
    let attr' = renderAttr attr <> ("src" =: T.pack iUrl <> "title" =: T.pack iTitle)
    elAttr "img" attr' $ mapM_ renderInline xs
  Note xs -> el "aside" $ mapM_ renderBlock xs
  Span attr xs -> elPandocAttr "span" attr $
    mapM_ renderInline xs

notImplemented :: (DomBuilder t m, Show a) => a -> m ()
notImplemented x = do
  el "strong" $ text "NotImplemented: "
  el "pre" $ el "code" $ text $ T.pack $ show x
