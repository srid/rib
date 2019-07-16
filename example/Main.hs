{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (div, (**))

import Control.Monad
import Data.List (partition, sortOn)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Text.Encoding (encodeUtf8)


import Clay hiding (type_)
import Lucid
import Development.Shake
import Development.Shake.FilePath

import qualified Rib.App as App
import Rib.Pandoc (getPandocMetaHTML, getPandocMetaValue, highlightingCss, pandoc2Html, parsePandoc)
import Rib.Simple (Page (..), Post (..), isDraft)
import Rib.Server (getHTMLFileUrl)
import qualified Rib.Simple as Simple

data PostCategory
  = Blog
  deriving (Eq, Ord, Show, Read)

main :: IO ()
main = App.run buildAction

buildAction :: Action ()
buildAction = do
  toc <- guideToc
  Simple.buildAction $ renderPage toc

guideToc :: Action [Text]
guideToc = do
  x :: Maybe [Text] <- fmap (decode . BSL.fromStrict . encodeUtf8 . T.pack) $ readFile' $ App.ribInputDir </> "guide.json"
  pure $ fromMaybe (fail "Bad JSON") x

renderPage :: [Text] -> Page -> Html ()
renderPage toc page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    meta_ [name_ "description", content_ "Rib - Haskell static site generator"]
    meta_ [name_ "author", content_ "Sridhar Ratnakumar"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ $ maybe siteTitle (<> " - " <> siteTitle) pageTitle
    style_ [type_ "text/css"] $ Clay.render pageStyle
    style_ [type_ "text/css"] highlightingCss
    link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"]
  body_ $ do
    with div_ [class_ "ui text container", id_ "thesite"] $
      with div_ [class_ "ui raised segment"] $ do
        with a_ [class_ "ui violet ribbon label", href_ "/"] "Rib"
        -- Main content
        with h1_ [class_ "ui huge header"] $ fromMaybe siteTitle pageTitle
        pre_ $ toHtml $ show toc
        with div_ [class_ "ui note message"] $ pandoc2Html $ parsePandoc
          "Please note: Rib is still a **work in progress**. The API might change before the initial public release. The content you read here should be considered draft version of the upcoming documentation."
        case page of
          Page_Index posts -> do
            p_ "Rib is a static site generator written in Haskell that reuses existing tools (Shake, Lucid and Clay) and is thus non-monolithic."
            let (blogPosts, otherPosts) =
                  partition ((== Just Blog) . getPandocMetaValue "category" . _post_doc) $ sortPosts posts
            postList otherPosts
            unless (null blogPosts) $ do
              with h2_ [class_ "ui header"] "Blog"
              postList blogPosts
          Page_Post post -> do
            when (isDraft post) $
              with div_ [class_ "ui warning message"] "This is a draft"
            with article_ [class_ "post"] $
              pandoc2Html $ _post_doc post
        with a_ [class_ "ui green right ribbon label", href_ "https://github.com/srid/rib"] "Github"
    -- Load Google fonts at the very end for quicker page load.
    forM_ googleFonts $ \f ->
      link_ [href_ $ "https://fonts.googleapis.com/css?family=" <> T.replace " " "+" f, rel_ "stylesheet"]

  where
    sortPosts = sortOn (getPandocMetaValue "order" . _post_doc :: Post -> Maybe Int)

    siteTitle = "Rib - Haskell static site generator"
    pageTitle = case page of
      Page_Index _ -> Nothing
      Page_Post post -> Just $ postTitle post

    -- Render the post title (Markdown supported)
    postTitle = fromMaybe "Untitled" . getPandocMetaHTML "title" . _post_doc

    -- Render a list of posts
    postList :: [Post] -> Html ()
    postList xs = with div_ [class_ "ui relaxed divided list"] $ forM_ xs $ \x ->
      with div_ [class_ "item"] $ do
        with a_ [class_ "header", href_ (getHTMLFileUrl $ _post_srcPath x)] $
          postTitle x
        small_ $ fromMaybe mempty $ getPandocMetaHTML "description" $ _post_doc x

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
