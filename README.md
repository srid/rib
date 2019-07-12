# rib

<!--
Credit for this image: https://www.svgrepo.com/svg/24439/ribs
-->
<img src="https://raw.githubusercontent.com/srid/rib/master/example/content/static/ribs.svg?sanitize=true" width="150" />

Rib is a static site generator written in Haskell using sensible technologies
like `Shake`, `Lucid` and `Clay`. It is nearly done but still a work in progress
and will soon be ready for general use.

## Example

See `./example` (author's actual website in fact) to see how the `Rib` library
can be used to write your own static site generator in a few lines of code which
includes the HTML and CSS of the site:

```
$ cloc --by-file example/Main.hs
[...]
-------------------------------------------------------------------------------
File                             blank        comment           code
-------------------------------------------------------------------------------
example/Main.hs                     15              6             90
-------------------------------------------------------------------------------
```

With Rib you do not have to deal with the less powerful template engines or
write raw HTML/CSS by hand. Do everything in Haskell, and concisely at that!

To get the example site up and running run:

```bash
cd ./example
nix-shell ../default.nix --run "ghcid -c 'ghci -Wall -i../src Main.hs' -T 'Rib.App.dev Main.settings' --reload=Main.hs"
```

The above command does the following:

- Drop into a nix-shell with needed Haskell dependencies
- Compile the `rib` library and `example` through ghcid
  - Whenever Haskell sources change ghcid reloads them
- Run `example/Main.hs` which in turn runs `Rib.App.run`
- This does the following:
  1. Generate ./content into ./content.generated using Shake
  2. Listens for changes to ./content, and re-generate them
  3. Start a HTTP server serving the ./content.generated directory

Thus, by running that one command you get a production-quality web server
serving your statically generated HTML files that automatically get regenerated
when the source content changes. What's more, you may change the Haskell sources
such as `Main.hs` and ghcid will recompile and relaunch the whole thing. With
`rib` you get hot reload for free.
