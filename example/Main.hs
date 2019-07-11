{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (div, (**))

import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics

import Clay hiding (type_)
import Reflex.Dom.Core hiding (display)
import Lucid

import Reflex.Dom.Pandoc.Document (elPandocDoc, elPandocInlines)
import qualified Reflex.Dom.Pandoc.SyntaxHighlighting as SyntaxHighlighting

import qualified Rib
import qualified Rib.App as App
import qualified Rib.Settings as S
import Rib.Types
import Rib.Pandoc

data PostCategory
  = Programming
  | Other
  deriving (Generic, Show, Eq, Ord, FromJSON, ToJSON)

-- | Configure this site here.
--
-- See `S.Settings` for the settings available.
settings :: S.Settings x
settings = Rib.defaultSiteSettings
  { S.pageWidget = pageWidget
  -- ^ How to render a page type
  }

main :: IO ()
main = App.run settings

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

pageWidget :: Page -> Html ()
pageWidget page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [name_ "charset", content_ "utf-8"]
    meta_ [name_ "description", content_ "Sridhar's notes"]
    meta_ [name_ "author", content_ "Sridhar Ratnakumar"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ pageTitle
    style_ [type_ "text/css"] $ TL.toStrict $ Clay.render pageStyle
    style_ [type_ "text/css"] $ TL.toStrict $ Clay.render $ SyntaxHighlighting.style
    link_ [href_ semanticUiCss, rel_ "stylesheet"]

  body_ $ do
    with div_ [class_ "ui text container", id_ "thesite"] $ do
      with div_ [class_ "ui raised segment"] $ do
        with a_ [class_ "ui violet ribbon label", href_ "/"] "Srid's notes"
        -- Main content
        with h1_ [class_ "ui huge header"] pageTitle
        case page of
          Page_Index posts -> do
            let (progPosts, otherPosts) =
                  partition ((== Just Programming) . getPandocMetaJson "category" . _post_doc) posts
            with h2_ [class_ "ui header"] "Haskell & Nix notes"
            postList progPosts
            with h2_ [class_ "ui header"] "Other notes"
            postList otherPosts
          Page_Post post ->
            with article_ [class_ "post"] $ do
              toHtml . T.pack . show $ _post_doc post
        with a_ [class_ "ui green right ribbon label", href_ "https://www.srid.ca"] "Sridhar Ratnakumar"
    -- Load Google fonts at the very end for quicker page load.
    forM_ googleFonts $ \f ->
      link_ [href_ $ "https://fonts.googleapis.com/css?family=" <> T.replace " " "-" f, rel_ "stylesheet"]

  where
    semanticUiCss = "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"

    pageTitle = case page of
      Page_Index _ -> "Srid's notes"
      Page_Post post -> postTitle post

    -- Render the post title (Markdown supported)
    postTitle = maybe "Untitled" (toHtml . T.pack . show) . getPandocMetaInlines "title" . _post_doc

    -- Render a list of posts
    postList :: [Post] -> Html ()
    postList xs = with div_ [class_ "ui relaxed divided list"] $ forM_ xs $ \x ->
      with div_ [class_ "item"] $ do
        with a_ [class_ "header", href_ (_post_url x)] $
          postTitle x
        small_ $ maybe blank (toHtml . T.pack .show)  $ getPandocMetaInlines "description" $ _post_doc x


-- HTML for page type
pageWidget' :: DomBuilder t m => Page -> m ()
pageWidget' page = elAttr "html" ("lang" =: "en") $ do
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
                  partition ((== Just Programming) . getPandocMetaJson "category" . _post_doc) posts
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
    postTitle = maybe (text "Untitled") elPandocInlines . getPandocMetaInlines "title" . _post_doc

    -- Render a list of posts
    postList xs = divClass "ui relaxed divided list" $ forM_ xs $ \x ->
      divClass "item" $ do
        elAttr "a" ("class" =: "header" <> "href" =: _post_url x) $
          postTitle x
        el "small" $ maybe blank elPandocInlines $ getPandocMetaInlines "description" $ _post_doc x

    semanticUiCss = "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"

    elMeta k v =
      elAttr "meta" ("name" =: k <> "content" =: v) blank
    elLinkStylesheet l =
      elAttr "link" ("href" =: l <> "rel" =: "stylesheet") blank
    elLinkGoogleFont f = elLinkStylesheet $
      "https://fonts.googleapis.com/css?family=" <> T.replace " " "-" f
    elStyleClay =
      elAttr "style" ("type" =: "text/css") . text . TL.toStrict . Clay.render
