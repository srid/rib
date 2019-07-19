---
title: "Getting Started"
description: Start using Rib to generate your own static website
---

We will use `Rib.Simple` to create the simplest static site possible---a site
with a list of posts and optional static files. First create the required
directory layout (content will be under directory `a`; and generated files will
be under directory `b`).


```bash
mkdir -p mysite/a mysite/b
cd mysite
```

Add some content by creating the file `a/first-post.md` with the following content:


```markdown
# Hello world

_This_ file is written in **Markdown**.

    Life is meant to be fun!
```


Finally, add the Haskell source `Main.hs` that wires everything together. Notice
the following:

- We use `Rib.Simple.buildAction` that wires together the necessary Shake
combinators (defined in `Rib.Shake`) for us---take a peek at that module if
you'd like to customize the behaviour of static site generation by writing your
own Shake action.

- `App.run` provides file monitoring and http serving on top of site
generation.

- Everything else in this file is your site specific HTML and CSS
written in Haskell DSL (via `Lucid` and `Clay`).

```haskell
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
import qualified Rib.Pandoc as Pandoc
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
  body_ $
    with div_ [id_ "thesite"] $ do
      -- Main content
      h1_ pageTitle
      case page of
        Page_Index posts ->
          div_ $ forM_ posts $ \(f, doc) -> div_ $ do
            with a_ [href_ (getHTMLFileUrl f)] $ postTitle doc
            maybe mempty small_ $ Pandoc.getMeta @(Html ()) "description" doc
        Page_Post (_, doc) -> do
          when (fromMaybe False $ Pandoc.getMeta @Bool "draft" doc) $
            div_ "This is a draft"
          with article_ [class_ "post"] $
            toHtmlRaw $ Pandoc.render doc
  where
    pageTitle = case page of
      Page_Index _ -> "My website!"
      Page_Post (_, doc) -> postTitle doc

    -- Render the post title (Markdown supported)
    postTitle = fromMaybe "Untitled" . Pandoc.getMeta @(Html ()) "title"

    -- | CSS
    pageStyle :: Css
    pageStyle = div # "#thesite" ? do
      marginLeft $ pct 20
      marginTop $ em 4
```

Include the `rib` library in your repo, install Nix and invoke the ghcid script:

```bash
git clone https://github.com/srid/rib rib
./rib/ghcid
```

This will launch the Rib app server running at http://localhost:8080. Watch it
regenerate the HTML files as you change the source (be they content or Haskell sources!).
