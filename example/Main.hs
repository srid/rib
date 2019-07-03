{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (div, (**))

import Control.Monad
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Clay
import Reflex.Dom.Core hiding (display)

import Reflex.Dom.Pandoc.Document (elPandocDoc, elPandocInlines)
import qualified Reflex.Dom.Pandoc.SyntaxHighlighting as SyntaxHighlighting

import qualified Rib
import qualified Rib.App as App
import qualified Rib.Settings as S
import Rib.Types (Page (..), Post(..), getPostAttribute, getPostAttributeJson, PostCategory(..))

-- TODO: PostCategory and such should not be in library

-- | Configure this site here.
--
-- See `S.Settings` for the settings available.
siteSettings :: S.Settings x
siteSettings = Rib.defaultSiteSettings
  { S.pageWidget = pageWidget
  -- ^ How to render a page type
  }

main :: IO ()
main = App.run siteSettings

-- | Entrypoint suited for ghcid
--
-- TODO: Can we obviate this? Use withArgs from ghcid.
dev :: IO ()
dev = App.runWith siteSettings $ App.Serve 8080 True

googleFonts :: [Text]
googleFonts = [headerFont, contentFont, codeFont]

headerFont :: Text
headerFont = "Comfortaa"

contentFont :: Text
contentFont = "Open Sans"

codeFont :: Text
codeFont = "Roboto Mono"

-- | Main style for the site
pageStyle :: Css
pageStyle = body ? do
  div # "#thesite" ? do
    marginTop $ em 1
    marginBottom $ em 2
    fontFamily [contentFont] [sansSerif]
    forM_ [h1, h2, h3, h4, h5, h6, ".header"] $ \sel -> sel ?
      fontFamily [headerFont] [sansSerif]
    forM_ [pre, code, "tt"] $ \sel -> sel ?
      fontFamily [codeFont] [monospace]
    h1 ? textAlign center
    (article ** h2) ? color darkviolet
    (article ** img) ? do
      display block
      marginLeft auto
      marginRight auto
      width $ pct 50
    footer ? textAlign center

-- | HTML for page type
pageWidget :: DomBuilder t m => Page -> m ()
pageWidget page = elAttr "html" ("lang" =: "en") $ do
  el "head" $ do
    mapM_ (uncurry elMeta)
      [ ("charset", "UTF-8")
      , ("description", "Sridhar's notes")
      , ("author", "Sridhar Ratnakumar")
      , ("viewport", "width=device-width, initial-scale=1")
      ]
    el "title" pageTitle
    mapM_ elStyleClay [pageStyle, SyntaxHighlighting.style]
    elLinkStylesheet semanticUiCss

  el "body" $ do
    elAttr "div" ("class" =: "ui text container" <> "id" =: "thesite") $ do
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
        elAttr "a" ("class" =: "ui green right ribbon label" <> "href" =: "https://www.srid.ca") $
          text "Sridhar Ratnakumar"

    -- Load Google fonts at the very end for quicker page load.
    mapM_ elLinkGoogleFont googleFonts

  where
    pageTitle = case page of
      Page_Index _ -> text "Srid's notes"
      Page_Post post -> postTitle post

    -- Render the post title (Markdown supported)
    postTitle = maybe (text "Untitled") elPandocInlines . getPostAttribute "title"

    -- Render a list of posts
    postList xs = divClass "ui relaxed divided list" $ forM_ xs $ \x ->
      divClass "item" $ do
        elAttr "a" ("class" =: "header" <> "href" =: _post_url x) $
          postTitle x
        el "small" $ maybe blank elPandocInlines $ getPostAttribute "description" x

    semanticUiCss = "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"

    elMeta k v =
      elAttr "meta" ("name" =: k <> "content" =: v) blank
    elLinkStylesheet l =
      elAttr "link" ("href" =: l <> "rel" =: "stylesheet") blank
    elLinkGoogleFont f = elLinkStylesheet $
      "https://fonts.googleapis.com/css?family=" <> T.replace " " "-" f
    elStyleClay =
      elAttr "style" ("type" =: "text/css") . text . TL.toStrict . Clay.render
