# rib

<!--
Credit for this image: https://www.svgrepo.com/svg/24439/ribs
-->
<img src="https://raw.githubusercontent.com/srid/rib/master/doc/a/static/ribs.svg?sanitize=true" width="150" />

**Rib** is a static site generator written in Haskell with the goal of reusing
existing tools (`Shake`, `Lucid` and `Clay`) instead of re-inventing them.

Rib enables you to write your site content, including HTML & CSS, in Haskell,
while providing necessary Shake combinators and Pandoc functions to wire
everything together in a simple fashion (see `Rib.Simple`).

## Examples

These examples illustrate how the `Rib` package can be used to write your own
static site generator in a few lines of Haskell code that includes the site's
HTML and CSS.

* [rib-sample](https://github.com/srid/rib-sample): Use this to get started with
  your own site.

* `./doc`: Rib's own documentation site. Live at: https://rib.srid.ca/

* Author's own website. Live at https://notes.srid.ca/ 

## Getting Started

To get a new site quickly up and running, go to
https://github.com/srid/rib-sample and click "Use this template", clone your new
repo and then run:

```bash
nix-shell --run 'ghcid -T main'
```

This will:

- Drop into a nix-shell with needed Haskell dependencies (including `rib`)
- Compile `Main.hs` through ghcid
  - Whenever `Main.hs` changes ghcid reloads them
- Run `Main.hs:main`
- This does the following:
  1. Convert sources in `./a` into `./b` using Shake
  2. Listens for changes to `./a`, and re-generate them
  3. Start a HTTP server serving the `./b` directory

Thus, by running that one command one gets a production-quality web server
serving the statically generated HTML files which automatically get regenerated
when the source content changes. What's more, we may change the Haskell sources
such as `Main.hs` and ghcid will recompile and relaunch the whole thing.

With `rib` we get hot reload for free.
