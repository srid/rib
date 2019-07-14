{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (div, (**))

import Control.Monad
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T

import Clay hiding (type_)
import Lucid

import qualified Rib.App as App
import Rib.Pandoc (getPandocMetaHTML, getPandocMetaValue, highlightingCss, pandoc2Html)
import Rib.Simple (Page (..), Post (..), isDraft)
import qualified Rib.Simple as Simple

data PostCategory
  = Blog
  deriving (Eq, Ord, Show, Read)

main :: IO ()
main = App.run $ Simple.buildAction renderPage

renderPage :: Page -> Html ()
renderPage page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    meta_ [name_ "description", content_ "Rib - Haskell static site generator"]
    meta_ [name_ "author", content_ "Sridhar Ratnakumar"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ pageTitle
    style_ [type_ "text/css"] $ Clay.render pageStyle
    style_ [type_ "text/css"] highlightingCss
    link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"]
  body_ $ do
    with div_ [class_ "ui text container", id_ "thesite"] $
      with div_ [class_ "ui raised segment"] $ do
        with a_ [class_ "ui violet ribbon label", href_ "/"] "Rib"
        -- Main content
        with h1_ [class_ "ui huge header"] pageTitle
        case page of
          Page_Index posts -> do
            let (blogPosts, otherPosts) =
                  partition ((== Just Blog) . getPandocMetaValue "category" . _post_doc) posts
            postList otherPosts
            with h2_ [class_ "ui header"] "Blog"
            postList blogPosts
          Page_Post post -> do
            when (isDraft post) $
              with div_ [class_ "ui warning message"] "This is a draft"
            with article_ [class_ "post"] $
              toHtmlRaw $ pandoc2Html $ _post_doc post
        with a_ [class_ "ui green right ribbon label", href_ "https://github.com/srid/rib"] "Github"
    -- Load Google fonts at the very end for quicker page load.
    forM_ googleFonts $ \f ->
      link_ [href_ $ "https://fonts.googleapis.com/css?family=" <> T.replace " " "+" f, rel_ "stylesheet"]

  where
    pageTitle = case page of
      Page_Index _ -> "Rib - Haskell static site generator"
      Page_Post post -> postTitle post

    -- Render the post title (Markdown supported)
    postTitle = maybe "Untitled" toHtmlRaw . getPandocMetaHTML "title" . _post_doc

    -- Render a list of posts
    postList :: [Post] -> Html ()
    postList xs = with div_ [class_ "ui relaxed divided list"] $ forM_ xs $ \x ->
      with div_ [class_ "item"] $ do
        with a_ [class_ "header", href_ (_post_url x)] $
          postTitle x
        small_ $ maybe mempty toHtmlRaw $ getPandocMetaHTML "description" $ _post_doc x

    -- | CSS
    pageStyle :: Css
    pageStyle = div # "#thesite" ? do
      marginTop $ em 1
      marginBottom $ em 2
      fontFamily [contentFont] [sansSerif]
      forM_ [h1, h2, h3, h4, h5, h6, ".header"] $ \sel -> sel ?
        fontFamily [headerFont] [sansSerif]
      forM_ [pre, code, "tt"] $ \sel -> sel ? do
        fontFamily [codeFont] [monospace]
      "div.sourceCode" ? do
        sym padding $ em 1
        backgroundColor "#EBF5FB"
      h1 ? textAlign center
      (article ** h2) ? color darkviolet
      (article ** img) ? do
        display block
        marginLeft auto
        marginRight auto
        width $ pct 50
      footer ? textAlign center

    googleFonts :: [Text]
    googleFonts = [headerFont, contentFont, codeFont]

    headerFont :: Text
    headerFont = "Roboto"
    contentFont :: Text
    contentFont = "Literata"
    codeFont :: Text
    codeFont = "Inconsolata"
