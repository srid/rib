{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HTML where

import Control.Monad
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Clay
import Reflex.Dom.Core

import Reflex.Dom.Pandoc.Document (elPandocDoc, elPandocInlines)
import qualified Reflex.Dom.Pandoc.SyntaxHighlighting as SyntaxHighlighting

import Rib.Types (Page (..), Post (..), PostCategory (..), getPostAttribute, getPostAttributeJson)

import qualified CSS

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
    elAttr "style" ("type" =: "text/css") $ text $ TL.toStrict $ Clay.render CSS.style
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
                  partition ((== Just Programming) . getPostAttributeJson "category") posts
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
    mapM_ elLinkGoogleFont [CSS.headerFont, CSS.contentFont, CSS.codeFont]
  where
    postList ps = divClass "ui relaxed divided list" $ forM_ ps $ \p ->
      divClass "item" $ do
        elAttr "a" ("class" =: "header" <> "href" =: _post_url p) $
          postTitle p
        el "small" $ maybe blank elPandocInlines $ getPostAttribute "description" p

    postTitle = maybe (text "Untitled") elPandocInlines . getPostAttribute "title"

    semUiCdn = "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
    elLinkGoogleFont name =
      elAttr "link" ("href" =: fontUrl <> "rel" =: "stylesheet" <> "type" =: "text/css") blank
      where
        fontUrl = "https://fonts.googleapis.com/css?family=" <> (T.replace " " "-" name)
    elMeta k v = elAttr "meta" ("name" =: k <> "content" =: v) blank
