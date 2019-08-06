{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (div, (**))

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Maybe
import Data.Profunctor (dimap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Clay hiding (filter, not, reverse, type_)
import Development.Shake
import Development.Shake.FilePath
import Lucid
import Text.Pandoc
import Text.Pandoc.Highlighting (styleToCss, tango)

import qualified Rib.App as App
import qualified Rib.Pandoc as Pandoc
import Rib.Server (getHTMLFileUrl)
import qualified Rib.Shake as Shake
import Rib.Simple (Page (..))

main :: IO ()
main = App.run buildAction

buildAction :: Action ()
buildAction = do
  void $ Shake.buildStaticFiles ["static//**"]

  toc <- guideToc
  posts <- applyGuide toc <$> Shake.readPandocMulti ("*.md", readMarkdown)

  void $ forP posts $ \x ->
    Shake.buildHtml (fst x -<.> "html") (renderPage $ Page_Post x)

  Shake.buildHtml "index.html" $ renderPage $ Page_Index posts

-- | Apply the guide metadata to a list of pages
applyGuide :: (Ord f, Show f) => [f] -> [(f, Pandoc)] -> [(f, Pandoc)]
applyGuide fs xs =
  flip mapWithAdj fsComplete $ \mprev (f, doc) mnext -> (f,) $
    setMetaMaybe "next" mnext $
    setMetaMaybe "prev" mprev doc
  where
    -- | Like `fmap` on lists but passes the previous and next element as well.
    mapWithAdj :: (Maybe a -> a -> Maybe a -> b) -> [a] -> [b]
    mapWithAdj f l = zipWith3 f (rshift1 l) l (shift1 l)
      where
        shift1 = (<> [Nothing]) . fmap Just . drop 1
        rshift1 = dimap reverse reverse shift1
    setMetaMaybe :: Show a => String -> Maybe a -> Pandoc -> Pandoc
    setMetaMaybe k mv doc = maybe doc (\v -> Pandoc.setMeta k v doc) mv
    -- Like `fs` but along with the associated Pandoc document (pulled from `xs`)
    fsComplete = fs <&> \f -> (f,) $ fromJust $ Map.lookup f xsMap
    xsMap = Map.fromList xs

guideToc :: Action [FilePath]
guideToc = do
  toc :: Maybe [FilePath] <- fmap (decode . BSL.fromStrict . encodeUtf8 . T.pack) $
    readFile' $ App.ribInputDir </> "guide.json"
  pure $ fromMaybe (fail "bad guide.json") toc

renderPage :: Page -> Html ()
renderPage page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    meta_ [name_ "description", content_ "Rib - Haskell static site generator"]
    meta_ [name_ "author", content_ "Sridhar Ratnakumar"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ $ maybe siteTitle (<> " - " <> siteTitle) pageTitle
    style_ [type_ "text/css"] $ Clay.render pageStyle
    style_ [type_ "text/css"] $ styleToCss tango
    link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"]
  body_ $ do
    with div_ [class_ "ui text container", id_ "thesite"] $
      with div_ [class_ "ui raised segment"] $ do
        with a_ [class_ "ui violet ribbon label", href_ "/"] "Rib"
        -- Main content
        with h1_ [class_ "ui huge header"] $ fromMaybe siteTitle pageTitle
        case page of
          Page_Index posts -> do
            p_ "Rib is a static site generator written in Haskell that reuses existing tools (Shake, Lucid and Clay) and is thus non-monolithic."
            with div_ [class_ "ui relaxed divided list"] $ forM_ posts $ \(f, doc) ->
              with div_ [class_ "item"] $ do
                with a_ [class_ "header", href_ (getHTMLFileUrl f)] $
                  postTitle doc
                maybe mempty small_ $ Pandoc.getMeta "description" doc
          Page_Post (_, doc) -> do
            when (fromMaybe False $ Pandoc.getMeta "draft" doc) $
              with div_ [class_ "ui warning message"] "This is a draft"
            postNav doc
            with article_ [class_ "post"] $
              Pandoc.render doc
        with a_ [class_ "ui green right ribbon label", href_ "https://github.com/srid/rib"] "Github"
    -- Load Google fonts at the very end for quicker page load.
    forM_ googleFonts $ \f ->
      link_ [href_ $ "https://fonts.googleapis.com/css?family=" <> T.replace " " "+" f, rel_ "stylesheet"]

  where
    siteTitle = "Rib - Haskell static site generator"
    pageTitle = case page of
      Page_Index _ -> Nothing
      Page_Post (_, doc) -> Just $ postTitle doc

    -- Render the post title (Markdown supported)
    postTitle = fromMaybe "Untitled" . Pandoc.getMeta @(Html ()) "title"

    -- Post navigation header
    postNav :: Pandoc -> Html ()
    postNav doc = with div_ [class_ "ui secondary segment"] $
      with div_ [class_ "ui grid"] $
        with div_ [class_ "four column row"] $
          forM_ [("prev", "Prev", "left"), ("next", "Next", "right")] $
            \(k, navLabel, navDir) -> with div_ [class_ $ navDir <> " floated column"] $
              case Pandoc.getMeta @(FilePath, Pandoc) k doc of
                Nothing -> mempty
                Just (f, otherDoc) -> strong_ $
                  with a_ [class_ "header", href_ (getHTMLFileUrl f)] $ do
                    navLabel <> ": "
                    postTitle otherDoc

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
