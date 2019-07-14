# rib

<!--
Credit for this image: https://www.svgrepo.com/svg/24439/ribs
-->
<img src="https://raw.githubusercontent.com/srid/rib/master/example/content/static/ribs.svg?sanitize=true" width="150" />

Rib is a static site generator written in Haskell that reuses existing tools (`Shake`, `Lucid` and `Clay`) and is thus non-monolithic. It is nearly done but still a work in progress and will soon be ready for general use.

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
example/Main.hs                     14              5             86
-------------------------------------------------------------------------------
```

(See `Rib.Simple` if you need further customization.)

With Rib you do not have to deal with less powerful template engines or
write raw HTML/CSS by hand. Do everything in Haskell, and concisely at that!

To get the example site up and running run:

```bash
cd ./example
../ghcid
```

This will:

- Drop into a nix-shell with needed Haskell dependencies
- Compile the `rib` library and `example/Main.hs` through ghcid
  - Whenever Haskell sources change ghcid reloads them
- Run `example/Main.hs:main` with `serve -w` CLI arguments
- This does the following:
  1. Generate ./content into ./content.generated using Shake
  2. Listens for changes to ./content, and re-generate them (the `-w` argument)
  3. Start a HTTP server serving the ./content.generated directory (the `serve` part)

Thus, by running that one command you get a production-quality web server
serving your statically generated HTML files that automatically get regenerated
when the source content changes. What's more, you may change the Haskell sources
such as `Main.hs` and ghcid will recompile and relaunch the whole thing. With
`rib` you get hot reload for free.
