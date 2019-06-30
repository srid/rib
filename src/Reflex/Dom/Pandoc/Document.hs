{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Pandoc.Document where

import Control.Monad (forM_)
import qualified Data.Text as T

import Reflex.Dom.Core hiding (Link, Space)

import Text.Pandoc (Block (..), Inline (..), Pandoc (..))

import Reflex.Dom.Pandoc.SyntaxHighlighting (elCodeHighlighted)
import Reflex.Dom.Pandoc.Util (elPandocAttr, headerElement, renderAttr)

-- | Convert Markdown to HTML
-- TODO: Implement the notImplemented
elPandocDoc :: DomBuilder t m => Pandoc -> m ()
elPandocDoc (Pandoc _meta blocks) = renderBlocks blocks
  where
    renderBlocks = mapM_ renderBlock
    renderBlock = \case
      Plain inlines -> renderInlines inlines
      Para xs -> el "p" $ renderInlines xs
      LineBlock xss -> forM_ xss $ \xs -> do
        renderInlines xs
        text "\n"
      CodeBlock attr x -> elCodeHighlighted attr x
      v@(RawBlock _ _) -> notImplemented v
      BlockQuote xs -> el "blockquote" $ renderBlocks xs
      OrderedList _lattr xss -> el "ol" $
        -- TODO: Implement list attributes.
        forM_ xss $ \xs -> el "li" $ renderBlocks xs
      BulletList xss -> el "ul" $ forM_ xss $ \xs -> el "li" $ renderBlocks xs
      DefinitionList defs -> el "dl" $ forM_ defs $ \(term, descList) -> do
        el "dt" $ renderInlines term
        forM_ descList $ \desc ->
          el "dd" $ renderBlocks desc
      Header level attr xs -> elPandocAttr (headerElement level) attr $ do
        renderInlines xs
      HorizontalRule -> el "hr" blank
      v@(Table _ _ _ _ _) -> notImplemented v
      Div attr xs -> elPandocAttr "div" attr $
        renderBlocks xs
      Null -> blank

    renderInlines = mapM_ renderInline
    renderInline = \case
      Str x -> text $ T.pack x
      Emph xs -> el "em" $ renderInlines xs
      Strong xs -> el "strong" $ renderInlines xs
      Strikeout xs -> el "strike" $ renderInlines xs
      Superscript xs -> el "sup" $ renderInlines xs
      Subscript xs -> el "sub" $ renderInlines xs
      SmallCaps xs -> el "small" $ renderInlines xs
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
        elAttr "a" attr' $ renderInlines xs
      Image attr xs (iUrl, iTitle) -> do
        let attr' = renderAttr attr <> ("src" =: T.pack iUrl <> "title" =: T.pack iTitle)
        elAttr "img" attr' $ renderInlines xs
      Note xs -> el "aside" $ renderBlocks xs
      Span attr xs -> elPandocAttr "span" attr $
        renderInlines xs

    notImplemented :: (DomBuilder t m, Show a) => a -> m ()
    notImplemented x = do
      el "strong" $ text "NotImplemented: "
      el "pre" $ el "code" $ text $ T.pack $ show x
