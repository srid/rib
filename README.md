# rib

<!--
Credit for this image: https://www.svgrepo.com/svg/24439/ribs
-->
<img src="https://raw.githubusercontent.com/srid/rib/master/example/content/images/ribs.svg?sanitize=true" width="150" />

Rib is a static site generator written in Haskell using sensible technologies
like `Shake`, `Reflex` and `Clay`. It is still a work in progress but will soon
be ready for general use.

## Example

See `./example` (author's actual website in fact) to see how the `Rib` library
can be used to write your own static site generator in a few lines of code which
includes the HTML and CSS of the site:

```
$ cloc --by-file example/*.hs
[...]
-------------------------------------------------------------------------------
File                             blank        comment           code
-------------------------------------------------------------------------------
example/Main.hs                     23             16            102
-------------------------------------------------------------------------------
```

With Rib you do not have to deal with the less powerful template engines or
write raw HTML/CSS by hand. Do everything in Haskell, and concisely at that!

## Local server when editing only content

```bash
nix-build -A ghc.rib
cd example
../result/bin/rib-example serve --watch
```

## ... when hacking on Haskell sources

```bash
nix-shell -A shells.ghc --run "ghcid -c 'cabal new-repl rib-example' -T 'System.Directory.withCurrentDirectory \"example\" \$ Main.dev'"
```

## TODO

- [ ] Use my own jsonCache' and drop dependency on Slick
- [ ] Combine multiple files into one `Site.hs`
- [ ] Hakyll-like programming interface:
  - Shake buildActions extension in Settings, with default implementation
    `buildActions :: Settings x -> Action ()`
    Make staticFilePatterns and postFilePatterns part of the default impl.
- [ ] One file dev?
  - User creates Site.hs, then content/Hello.md and that's it.
  - A shebang in Site.hs will allow them to build it straight!

### Article Ideas

- Github CI for OSS haskell projects
- Lens and friends
- mtl
- string types
- personal nix cache
