{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (div, (**))

import Control.Monad

import Clay hiding (type_)
import Development.Shake
import Lucid

import qualified Rib.App as App
import Rib.Pandoc (getPandocMetaHTML, highlightingCss, pandoc2Html)
import Rib.Server (getHTMLFileUrl)
import Rib.Simple (Page (..), Post (..), isDraft)
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
          div_ $ forM_ posts $ \post -> div_ $ do
            with a_ [href_ (getHTMLFileUrl $ _post_srcPath post)] $ postTitle post
            small_ $ maybe mempty toHtmlRaw $ getPandocMetaHTML "description" $ _post_doc post
        Page_Post post -> do
          when (isDraft post) $
            div_ "This is a draft"
          with article_ [class_ "post"] $
            toHtmlRaw $ pandoc2Html $ _post_doc post
  where
    pageTitle = case page of
      Page_Index _ -> "My website!"
      Page_Post post -> postTitle post

    -- Render the post title (Markdown supported)
    postTitle = maybe "Untitled" toHtmlRaw . getPandocMetaHTML "title" . _post_doc

    -- | CSS
    pageStyle :: Css
    pageStyle = div # "#thesite" ? do
      marginLeft $ pct 20
      marginTop $ em 4
