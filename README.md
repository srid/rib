![Logo](https://raw.githubusercontent.com/srid/rib/master/assets/rib.png)

# rib

[![BSD3](https://img.shields.io/badge/License-BSD-blue.svg)](https://en.wikipedia.org/wiki/BSD_License)
[![Hackage](https://img.shields.io/hackage/v/rib.svg)](https://hackage.haskell.org/package/rib)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

Rib is a Haskell library for writing your own **static site generator**.

How does it compare to Hakyll?

- Use the [Shake](https://shakebuild.com/) build system
- Builtin support for using Haskell DSL to define the HTML
  ([Lucid](https://chrisdone.com/posts/lucid2/)) & CSS
  ([Clay](http://fvisser.nl/clay/)) of your site 
  - Like Hakyll, Rib uses [Pandoc](https://pandoc.org/) for parsing the source
    documents. It also supports [MMark](https://github.com/mmark-md/mmark) if you need a lightweight alternative.
- Remain as simple as possible to use (see example below)
- Optional Nix based workflow for easily reproducible environment

Rib prioritizes the use of *existing* tools over reinventing them, and enables
the user to compose them as they wish instead of having to write code to fit a
custom framework.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [rib](#rib)
    - [Quick Preview](#quick-preview)
    - [Getting Started](#getting-started)
    - [Concepts](#concepts)
        - [Directory structure](#directory-structure)
        - [Run the site](#run-the-site)
        - [How Rib works](#how-rib-works)
        - [Editing workflow](#editing-workflow)
        - [What's next?](#whats-next)
    - [Examples](#examples)

<!-- markdown-toc end -->

## Quick Preview

Here is how your code may look like if you were to generate your static site
using Rib:

``` haskell
-- First we shall define two datatypes to represent our pages. One, the page
-- itself. Second, the metadata associated with each document.

-- | A generated page is either an index of documents, or an individual document.
--
-- The `Document` type takes two type variables:
-- 1. The first type variable specifies the parser to use: MMark or Pandoc
-- 2. The second type variable should be your metadata record
data Page
  = Page_Index [Document MMark DocMeta]
  | Page_Doc (Document MMark DocMeta)

-- | Type representing the metadata in our Markdown documents
--
-- Note that if a field is not optional (i.e., not Maybe) it must be present.
data DocMeta
  = DocMeta
      { title :: Text,
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `a`, from which static files will be read.
-- 2. Directory `b`, under which target files will be generated.
-- 3. Shake build action to run.
--
-- In the shake build action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = Rib.run [reldir|a|] [reldir|b|] $ do
  -- Copy over the static files
  Rib.buildStaticFiles [[relfile|static/**|]]
  -- Build individual markdown files, generating .html for each.
  posts <- Rib.buildHtmlMulti [relfile|*.md|] (renderPage . Page_Doc)
  -- Build an index.html linking to the aforementioned files.
  Rib.buildHtml [relfile|index.html|]
    $ renderPage
    $ Page_Index posts
  where
    -- Define your site HTML here
    renderPage :: Page -> Html ()
    renderPage page = with html_ [lang_ "en"] $ do
      head_ $ do
        meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
        title_ $ case page of
          Page_Index _ -> "My website!"
          Page_Doc doc -> toHtml $ title $ Rib.documentMeta doc
        style_ [type_ "text/css"] $ Clay.render pageStyle
      body_
        $ with div_ [id_ "thesite"]
        $ do
          with a_ [href_ "/"] "Back to Home"
          hr_ []
          case page of
            Page_Index docs ->
              div_ $ forM_ docs $ \doc -> with li_ [class_ "links"] $ do
                let meta = Rib.documentMeta doc
                b_ $ with a_ [href_ (Rib.documentUrl doc)] $ toHtml $ title meta
                maybe mempty Rib.renderMarkdown $
                  description meta
            Page_Doc doc ->
              with article_ [class_ "post"] $ do
                h1_ $ toHtml $ title $ Rib.documentMeta doc
                Rib.documentHtml doc
    -- Define your site CSS here
    pageStyle :: Css
    pageStyle = "div#thesite" ? do
      margin (em 4) (pc 20) (em 1) (pc 20)
      "li.links" ? do
        listStyleType none
        marginTop $ em 1
        "b" ? fontSize (em 1.2)
        "p" ? sym margin (px 0)
```

(View full [`Main.hs`](https://github.com/srid/rib-sample/blob/master/Main.hs) at rib-sample)

## Getting Started

The easiest way to get started with [Rib](/) is to [use the
template](https://help.github.com/en/articles/creating-a-repository-from-a-template)
repository, [**rib-sample**](https://github.com/srid/rib-sample), from Github.

## Concepts

### Directory structure

Let's look at what's in the template repository:

```shell
$ git clone https://github.com/srid/rib-sample.git mysite
...
$ cd mysite
$ ls -F
a/  default.nix  Main.hs  README.md  rib-sample.cabal
```

The three key items here are:

1. `Main.hs`: Haskell source containing the DSL of the HTML/CSS of your site.
1. `a/`: The source content (eg: Markdown sources and static files)
1. `b/`: The target directory, excluded from the git repository, will contain
   _generated_ content (i.e., the HTML files, and copied over static content)
   
The template repository comes with a few sample posts under `a/`, and a basic
HTML layout and CSS style defined in `Main.hs`. 

### Run the site

Now let's run them all. 

Clone the sample repository locally, install [Nix](https://nixos.org/nix/) and
run your site as follows:

```shell
nix-shell --run 'ghcid -T main'
```

(Note even though the author recommends it Nix is strictly not required; you may
simply run `ghcid -T main` instead of the above command if you do not wish to
use Nix.)

Running this command gives you a local HTTP server at http://localhost:8080/
(serving the generated files) that automatically reloads when either the content
(`a/`) or the HTML/CSS/build-actions (`Main.hs`) changes. Hot reload, in other
words.

### How Rib works

How does the aforementioned nix-shell command work?

1. `nix-shell` will run the given command in a shell environment with all of our
dependencies (notably the Haskell ones including the `rib` library itself)
installed. 

1. [`ghcid`](https://github.com/ndmitchell/ghcid) will compile your `Main.hs`
   and run its `main` function.

1. `Main.hs:main` in turn calls `Rib.App.run` which takes as argument your custom Shake action that will build the static site.

1. `Rib.App.run`: this parses the CLI arguments and runs the rib CLI "app" which
   can be run in one of a few modes --- generating static files, watching the
   `a/` directory for changes, starting HTTP server for the `b/` directory. By
   default---without any explicit arguments---this will run the Shake build
   action passed as argument on every file change and spin up a HTTP server.
   
Run that command, and visit http://localhost:8080 to view your site.

### Editing workflow

Now try making some changes to the content, say `a/first-post.md`. You should
see it reflected when you refresh the page. Or change the HTML or CSS of your
site in `Main.hs`; this will trigger `ghcid` to rebuild the Haskell source and
restart the server.

### What's next?

Great, by now you should have your static site generator ready and running! What
more can you do? Surely you may have specific needs; and this usually translates
to running custom Shake actions during the build. Rib provides helper functions in `Rib.Shake` to make this easier. 

Rib recommends writing your Shake actions in the style of being 
[forward-defined](http://hackage.haskell.org/package/shake-0.18.3/docs/Development-Shake-Forward.html)
which adds to the simplicity of the entire thing.

## Examples

* [rib-sample](https://github.com/srid/rib-sample): Use this to get started with
  your own site.

* Author's own website. Live at https://www.srid.ca/ 
