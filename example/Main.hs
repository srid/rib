{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (div, (**))

import Control.Monad
import Data.Maybe

import Clay hiding (type_)
import Development.Shake
import Lucid

import qualified Rib.App as App
import Rib.Pandoc (getMeta, highlightingCss, pandoc2Html)
import Rib.Server (getHTMLFileUrl)
import Rib.Simple (Page (..))
import qualified Rib.Simple as Simple

main :: IO ()
main = App.run buildAction

buildAction :: Action ()
buildAction = Simple.buildAction renderPage

renderPage :: Page -> Html ()
renderPage page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ pageTitle
    style_ [type_ "text/css"] $ Clay.render pageStyle
    style_ [type_ "text/css"] highlightingCss
  body_ $
    with div_ [id_ "thesite"] $ do
      -- Main content
      h1_ pageTitle
      case page of
        Page_Index posts ->
          div_ $ forM_ posts $ \(f, doc) -> div_ $ do
            with a_ [href_ (getHTMLFileUrl f)] $ postTitle doc
            maybe mempty small_ $ getMeta @(Html ()) "description" doc
        Page_Post (_, doc) -> do
          when (fromMaybe False $ getMeta @Bool "draft" doc) $
            div_ "This is a draft"
          with article_ [class_ "post"] $
            toHtmlRaw $ pandoc2Html doc
  where
    pageTitle = case page of
      Page_Index _ -> "My website!"
      Page_Post (_, doc) -> postTitle doc

    -- Render the post title (Markdown supported)
    postTitle = fromMaybe "Untitled" . getMeta @(Html ()) "title"

    -- | CSS
    pageStyle :: Css
    pageStyle = div # "#thesite" ? do
      marginLeft $ pct 20
      marginTop $ em 4
