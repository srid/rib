---
title: "Getting Started"
description: Start using Rib to generate your own static website
---

We will use `Rib.Simple` to create the simplest static site possible -- a site
with a list of posts and optional static files. First
create the required directory layout:


```bash
mkdir -p mysite/content/static 
cd mysite
```

Add some content by creating the file `content/first-post.md` with the following content:


```markdown
# Hello world

_This_ file is be written in *Markdown*.
```


Finally add the Haskell source `Main.hs` that wires everything together. Notice
how we use `Rib.Simple` that does the necessary Shake machinary for us. The
`App.run` here provides file monitoring and http serving on top of shake generation.

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

data PostCategory
  = Blog
  deriving (Eq, Ord, Show, Read)

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
