---
title: "Getting Started"
description: Start using Rib to generate your own static website
---

The easiest way to get started with your new site is start from the `rib-sample`
repository. To do this go to the Github repository for
[`rib-sample`](https://github.com/srid/rib-sample) and click **Use this
template** and clone your new repo locally.

Follow the sample's README.md and install [Nix](https://nixos.org/nix/) before
running the dev server:

```
nix-shell --run 'ghcid -T man'
```

Nix is a purely functional package manger (a tutorial for Haskellers is
[available here](https://notes.srid.ca/haskell-nix)) that is used here to create
the desired environment for running Rib. `ghcid` is used to launch the dev
server; it automatically rebuilds and restarts it when the source file for your
static site (i.e., `Main.hs`) changes.

Go to http://localhost:8080 to access your site. A few points to note:

- Content is available in the `./a` directory. 
- HTML is generated in the `./b` directory
- Editing either the content or the HTML/CSS (in `Main.css`) will restart the
  dev server automatically in addition to Shake regenerating changed files.
  

## What's in `Main.hs`

Take a look at
[`Main.hs`](https://github.com/srid/rib-sample/blob/master/Main.hs) of
`rib-sample`; you'll notice that most of the contents in that file is about
defining the HTML and CSS of your site. The key part is in the `main` function
invoking Rib's `Simple.buildAction` function.

Rib comes with a builtin Shake build action `Rib.Simple.buildAction`
([forward-defined](http://hackage.haskell.org/package/shake-0.18.3/docs/Development-Shake-Forward.html)
for simplicity) for use
with the most simple static sites. If at any point you need to customize the
behaviour of your static site build don't hesitate to write your build action,
based on the template at
[`Rib.Simple`](https://github.com/srid/rib/blob/master/src/Rib/Simple.hs) and
using the helper functions from `Rib.Pandoc` and `Rib.Shake`.
