---
title: Rib
---

[Rib](https://github.com/srid/rib) is an easily composable Haskell static site generator library based on Shake.

## Compared to Hakyll

How does it compare to the popular static site generator Hakyll?

- Uses the [Shake](https://shakebuild.com/) build system at its core.
- Write HTML ([Lucid](https://chrisdone.com/posts/lucid2/)) & CSS ([Clay](http://fvisser.nl/clay/)) in Haskell.
- Built-in support for [Pandoc](https://pandoc.org/) and [MMark](https://github.com/mmark-md/mmark).
- Remain as simple as possible to use (see example below)
- Nix-based environment for reproducibility
- `ghcid` and fsnotify for "hot reload"

## Quick Preview

Here is how your code may look like if you were to generate your static site
using Rib:

```haskell
-- | Route corresponding to each generated static page.
--
-- The `a` parameter specifies the data (typically Markdown document) used to
-- generate the final page text.
data Route a where
  Route_Index :: Route [(Route Pandoc, Pandoc)]
  Route_Article :: FilePath -> Route Pandoc

-- | The `IsRoute` instance allows us to determine the target .html path for
-- each route. This affects what `routeUrl` will return.
instance IsRoute Route where
  routeFile = \case
    Route_Index ->
      pure "index.html"
    Route_Article srcPath ->
      pure $ "article" </> srcPath -<.> ".html"

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `content`, from which static files will be read.
-- 2. Directory `dest`, under which target files will be generated.
-- 3. Shake action to run.
--
-- In the shake action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = withUtf8 $ do
  Rib.run "content" "dest" generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles ["static/**"]
  let writeHtmlRoute :: Route a -> a -> Action ()
      writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r
  -- Build individual sources, generating .html for each.
  articles <-
    Rib.forEvery ["*.md"] $ \srcPath -> do
      let r = Route_Article srcPath
      doc <- Pandoc.parse Pandoc.readMarkdown srcPath
      writeHtmlRoute r doc
      pure (r, doc)
  writeHtmlRoute Route_Index articles

-- | Define your site HTML here
renderPage :: Route a -> a -> Html ()
renderPage route val = html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ routeTitle
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ $ do
    div_ [class_ "header"] $
      a_ [href_ "/"] "Back to Home"
    h1_ routeTitle
    case route of
      Route_Index ->
        div_ $ forM_ val $ \(r, src) ->
          li_ [class_ "pages"] $ do
            let meta = getMeta src
            b_ $ a_ [href_ (Rib.routeUrl r)] $ toHtml $ title meta
            renderMarkdown `mapM_` description meta
      Route_Article _ ->
        article_ $
          Pandoc.render val
  where
    routeTitle :: Html ()
    routeTitle = case route of
      Route_Index -> "Rib sample site"
      Route_Article _ -> toHtml $ title $ getMeta val
    renderMarkdown :: Text -> Html ()
    renderMarkdown =
      Pandoc.render . Pandoc.parsePure Pandoc.readMarkdown

-- | Define your site CSS here
pageStyle :: Css
pageStyle = C.body ? do
  C.margin (em 4) (pc 20) (em 1) (pc 20)
  ".header" ? do
    C.marginBottom $ em 2
  "li.pages" ? do
    C.listStyleType C.none
    C.marginTop $ em 1
    "b" ? C.fontSize (em 1.2)
    "p" ? sym C.margin (px 0)

-- | Metadata in our markdown sources
data SrcMeta
  = SrcMeta
      { title :: Text,
        -- | Description is optional, hence `Maybe`
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)
```

(View full [`Main.hs`](https://github.com/srid/rib-sample/blob/master/src/Main.hs) at rib-sample)

## Getting Started

The easiest way to get started with [Rib](/) is to [use the
template](https://help.github.com/en/articles/creating-a-repository-from-a-template)
repository, [**rib-sample**](https://github.com/srid/rib-sample), from Github.

## Next

* [2014301](z://concepts)
* [2014302](z://examples)

