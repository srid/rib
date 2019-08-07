---
title: "Getting Started"
description: Start using Rib to generate your own static website
---

The easiest way to get started with [Rib](/) is to [use the
template](https://help.github.com/en/articles/creating-a-repository-from-a-template)
repository, [**rib-sample**](https://github.com/srid/rib-sample), from Github.

## Directory structure

Let's look at what's in the template repository:

```shell
$ git clone https://github.com/srid/rib-sample.git mysite
...
$ cd mysite
$ ls -F
a/  b/  default.nix  Main.hs  README.md  rib-sample.cabal
```

The three key items here are:

1. `Main.hs`: Haskell source containing the DSL of the HTML/CSS of your site.
1. `a/`: The source content (eg: Markdown sources and static files)
1. `b/`: The target directory, initially empty, will contain _generated_ content
   (i.e., the HTML files, and copied over static content)
   
The template repository comes with a few sample posts under `a/`, and a basic
HTML layout and CSS style defined in `Main.hs`. 

## Run the site

Now let's run them all. 

Clone the sample repository locally, install [Nix](https://nixos.org/nix/) and
run your site as follows:

```shell
nix-shell --run 'ghcid -T main'
```

Running this command gives you a local HTTP server at http://localhost:8080/
(serving the generated files) that automatically reloads when either the content
(`a/`) or the HTML/CSS/build-actions (`Main.hs`) changes. Hot reload, in other
words.

## How Rib works

How does the aforementioned nix-shell command work?

1. `nix-shell` will run the given command in a shell environment with all of our
dependencies (notably the Haskell ones including the `rib` library itself)
installed. 

1. [`ghcid`](https://github.com/ndmitchell/ghcid) will compile your `Main.hs`
   and run its `main` function.

1. `Main.hs:main` in turn calls the Shake build action (via `Rib.App.run`)
   defined in `Rib.Simple.buildAction` passing it your function `renderPage`.

There is quite a bit going on in that step 3! Let's break it down:

1. `Rib.App.run`: this parses the CLI arguments and runs the rib CLI "app" which
   can be run in one of a few modes --- generating static files, watching the
   `a/` directory for changes, starting HTTP server for the `b/` directory. By
   default---without any explicit arguments---this will run the Shake build
   action passed as argument on every file change and spin up a HTTP server.
   
1. `Rib.Simple.buildAction`: The `run` function takes a Shake build action to
   run on file change. `Rib.Simple` provides a very simple build action for
   generating the most simple static site --- a list of posts with static assets
   --- which the sample repository uses.
   
Run that command, and visit http://localhost:8080 to view your site.

## Editing workflow

Now try making some changes to the content, say `a/first-post.md`. You should
see it reflected when you refresh the page. Or change the HTML or CSS of your
site in `Main.hs`; this will trigger `ghcid` to rebuild the Haskell source and
restart the server.

## What's next?

Great, by now you should have your static site generator ready and running! What
more can you do? Surely you may have specific needs; and this usually translates
to running custom Shake actions during the build.

Rib provides helper functions in `Rib.Shake` and `Rib.Pandoc` to make this
easier. Indeed the `Rib.Simple.buildAction` function which the sample project
readily uses makes use of these functions.

In order to customize your site's build actions,

1. Copy the source for `buildAction` from the
[`Rib.Simple`](https://github.com/srid/rib/blob/master/src/Rib/Simple.hs) module
to your `Main.hs`

1. Make any customizations you want in *your* `buildAction` function.

1. Use that as the argument to the `Rib.App.run` function in your `main`

Notice how Rib's builtin `buildAction` is 
[forward-defined](http://hackage.haskell.org/package/shake-0.18.3/docs/Development-Shake-Forward.html)
which adds to the simplicity of the entire thing.
