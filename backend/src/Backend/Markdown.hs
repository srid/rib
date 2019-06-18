{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Markdown where

import Control.Foldl hiding (mapM_, mconcat)
import Control.Monad (forM_)
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

import Reflex.Dom.Core hiding (Link, Value)

import qualified Text.MMark as MMark
import Text.MMark.Extension (Block (..), Inline (..))
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Text.MMark.Extension.Common as Ext
import qualified Text.URI as URI

data Page = Page
  { _page_title :: Text
  }

-- NOTE: This is for future; not used yet.
parsePage :: Applicative f => MMark.MMark -> f (Maybe Page)
parsePage r = case MMark.projectYaml r of
  Nothing -> pure Nothing
  Just doc -> do
    let f = Yaml.withObject "metadata" $ \metadata -> do
          title <- metadata .: "title"
          pure $ Page title
    either error (pure . Just) $ Yaml.parseEither f doc

-- TODO: Just send the parsed structure as JSON to frontend, and have it render the Dom.
markdownView :: DomBuilder t m => Text -> m (Maybe Page)
markdownView source = case MMark.parse "<nofile>" source of
  Left errs -> elClass "tt" "markdown-error" $ do
    el "h2" $ text "Error parsing markdown:"
    text $ T.pack $ errorBundlePretty errs
    pure Nothing
  Right r -> do
    let r' = MMark.useExtensions extensions r
    MMark.runScannerM r' $ FoldM (const renderBlock) blank pure
    parsePage r'
  where
    -- FIXME: None of these extensions, except kbd, work.
    extensions =
      [ Ext.punctuationPrettifier
      , Ext.skylighting
      , Ext.kbd
      ]
    renderBlocks = mapM_ renderBlock
    renderBlock = \case
      ThematicBreak -> el "tt" $ text "TODO: ThematicBreak"
      Heading1 xs -> el "h1" $ renderInlines xs
      Heading2 xs -> el "h2" $ renderInlines xs
      Heading3 xs -> el "h3" $ renderInlines xs
      Heading4 xs -> el "h4" $ renderInlines xs
      Heading5 xs -> el "h5" $ renderInlines xs
      Heading6 xs -> el "h6" $ renderInlines xs
      CodeBlock info xs -> el "pre" $ elClass "code" (fromMaybe "" info) $ text xs
      Naked xs -> do
        -- text $ "TODO: Naked"
        renderInlines xs
      Paragraph xs -> el "p" $ renderInlines xs
      Blockquote bs -> el "blockquote" $ renderBlocks bs
      OrderedList _start bs -> el "ol" $ do
        -- TODO: What to do with 'start'?
        forM_ bs $ \b -> do
          el "li" $ renderBlocks b
      UnorderedList bs -> el "ul" $ do
        forM_ bs $ \b -> do
          el "li" $ renderBlocks b
      Table _ _ -> el "tt" $ text "TODO: Table"
    renderInlines = mapM_ renderInline . NE.toList
    renderInline = \case
      Plain s -> text s
      LineBreak -> el "tt" $ text "TODO: LineBreak"
      Emphasis xs -> el "em" $ renderInlines xs
      Strong xs -> el "strong" $ renderInlines xs
      Strikeout xs -> el "strike" $ renderInlines xs
      Subscript xs -> el "sub" $ renderInlines xs
      Superscript xs -> el "sup" $ renderInlines xs
      CodeSpan s -> el "code" $ text s
      Link xs dest title -> referringElement "a" "href" "title" dest title $ renderInlines xs
      Image xs dest title -> referringElement "img" "src" "alt" dest title $ renderInlines xs
    referringElement t refAttr titleAttr dest title = elAttr t attrs
      where
        attrs = mconcat $ catMaybes
          [ Just $ refAttr =: URI.render dest
          , (titleAttr =:) <$> title
          ]
