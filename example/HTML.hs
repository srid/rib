{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HTML where

import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Clay
import Reflex.Dom.Core
import Text.Pandoc (Block (Plain), Inline (Str), Pandoc (Pandoc))
import Text.Pandoc.UTF8 (fromStringLazy)

import Reflex.Dom.Pandoc.Document (elPandocDoc)
import qualified Reflex.Dom.Pandoc.SyntaxHighlighting as SyntaxHighlighting

import Rib.Types (Page (..), Post (..), PostCategory (..), getPostAttribute)

import CSS (codeFont, contentFont, headerFont, siteStyle)

pageHTML :: Page -> IO String
pageHTML = fmap BS8.unpack . renderHTML . pageWidget
  where
    -- | Convert a Reflex DOM widget into HTML
    renderHTML :: StaticWidget x a -> IO BS8.ByteString
    renderHTML = fmap snd . renderStatic

-- | The entire HTML layout is here.
pageWidget :: DomBuilder t m => Page -> m ()
pageWidget page = do
  let pageTitle = case page of
        Page_Index _ -> text "Srid's notes"
        Page_Post post -> postTitle post
  elAttr "html" ("lang" =: "en") $ el "head" $ do
    elMeta "charset" "UTF-8"
    elMeta "description" "Sridhar's notes"
    elMeta "author" "Sridhar Ratnakumar"
    elMeta "viewport" "width=device-width, initial-scale=1"
    el "title" pageTitle
    elAttr "style" ("type" =: "text/css") $ text $ TL.toStrict $ Clay.render siteStyle
    elAttr "style" ("type" =: "text/css") $ text $ TL.toStrict $ Clay.render SyntaxHighlighting.style
    elAttr "link" ("rel" =: "stylesheet" <> "href" =: semUiCdn) blank
  el "body" $ do
    elAttr "div" ("class" =: "ui text container" <> "id" =: "thesite") $ do
      el "br" blank
      divClass "ui raised segment" $ do
        -- Header
        elAttr "a" ("class" =: "ui violet ribbon label" <> "href" =: "/") $ text "Srid's notes"
        -- Main content
        elClass "h1" "ui huge header" pageTitle
        case page of
          Page_Index posts -> do
            let (progPosts, otherPosts) =
                  partition ((== Just Programming) . postCategory) posts
            elClass "h2" "ui header" $ text "Haskell & Nix notes"
            postList progPosts
            elClass "h2" "ui header" $ text "Other notes"
            postList otherPosts
          Page_Post post ->
            elClass "article" "post" $
              elPandocDoc $ _post_doc post
        -- Footer
        elAttr "a" ("class" =: "ui green right ribbon label" <> "href" =: "https://www.srid.ca") $ text "Sridhar Ratnakumar"
    el "br" blank
    el "br" blank
    mapM_ elLinkGoogleFont [headerFont, contentFont, codeFont]
  where
    postList ps = divClass "ui relaxed divided list" $ forM_ ps $ \p ->
      divClass "item" $ do
        elAttr "a" ("class" =: "header" <> "href" =: _post_url p) $
          postTitle p
        el "small" $ maybe blank pandocInlines $ getPostAttribute "description" p

    postTitle = maybe (text "Untitled") pandocInlines . getPostAttribute "title"
    postCategory post = getPostAttribute "category" post >>= \case
      [Str category] -> do
        let categoryJson = "\"" <> category <> "\""
        Aeson.decode $ fromStringLazy categoryJson
      _ -> error "Invalid category format"

    -- TODO: Put this in Markdown module, and reuse renderBlocks
    pandocInlines xs = elPandocDoc $ Pandoc mempty [Plain xs]

    semUiCdn = "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
    elLinkGoogleFont name =
      elAttr "link" ("href" =: fontUrl <> "rel" =: "stylesheet" <> "type" =: "text/css") blank
      where
        fontUrl = "https://fonts.googleapis.com/css?family=" <> (T.replace " " "-" name)
    elMeta k v = elAttr "meta" ("name" =: k <> "content" =: v) blank
