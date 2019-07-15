---
title: "Getting Started"
description: Start using Rib to generate your own static website
---

We will use `Rib.Simple` to create the simplest static site possible---a site
with a list of posts and optional static files. First create the required
directory layout (content will be under directory `a`; and generated files will
be under directory `b`).


```bash
mkdir -p mysite/a/static
cd mysite
```

Add some content by creating the file `a/first-post.md` with the following content:


```markdown
# Hello world

_This_ file is be written in *Markdown*.

    Life is meant to be fun!
```


Finally, add the Haskell source `Main.hs` that wires everything together. Notice
the following:

- We use `Rib.Simple` that does the necessary Shake machinery for us---take a
peek at that module if you'd like to customize the behaviour of static site
generation by writing your own Shake action---in addition to providing the
`Page` type that distinguishes between a `Post` file (i.e., the Markdown file
above) and the index file (`index.html`), which links to the list of posts.

- `App.run` provides file monitoring and http serving on top of site
generation.

- Everything else in this file is your site specific HTML and CSS
written in Haskell DSL (via `Lucid` and `Clay`).

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (div, (**))

import Control.Monad

import Clay hiding (type_)
import Lucid

import qualified Rib.App as App
import Rib.Pandoc (getPandocMetaHTML, highlightingCss, pandoc2Html)
import Rib.Simple (Page (..), Post (..), isDraft)
import qualified Rib.Simple as Simple

main :: IO ()
main = App.run $ Simple.buildAction renderPage

renderPage :: Page -> Html ()
renderPage page = with html_ [lang_ "en"] $ do
  head_ $ do
    title_ pageTitle
    style_ [type_ "text/css"] $ Clay.render pageStyle
    style_ [type_ "text/css"] highlightingCss
  body_ $ do
    with div_ [id_ "thesite"] $ do
      -- Main content
      h1_ pageTitle
      case page of
        Page_Index posts ->
          postList posts
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

    -- Render a list of posts
    postList :: [Post] -> Html ()
    postList xs = div_ $ forM_ xs $ \x -> div_ $ do
      with a_ [href_ (_post_url x)] $ postTitle x
      small_ $ maybe mempty toHtmlRaw $ getPandocMetaHTML "description" $ _post_doc x

    -- | CSS
    pageStyle :: Css
    pageStyle = div # "#thesite" ? do
      marginTop $ em 1
      marginBottom $ em 2
```

Include the `rib` library in your repo and invoke the ghcid script:

```bash
git clone https://github.com/srid/rib rib
./rib/ghcid
```

This will launch the Rib app server running at http://localhost:8080. Watch it
regenerate the HTML files as you change the source (be they content or Haskell sources!).
