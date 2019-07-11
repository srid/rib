# rib

<!--
Credit for this image: https://www.svgrepo.com/svg/24439/ribs
-->
<img src="https://raw.githubusercontent.com/srid/rib/master/example/content/static/ribs.svg?sanitize=true" width="150" />

Rib is a static site generator written in Haskell using sensible technologies
like `Shake`, `Lucid` and `Clay`. It is still a work in progress but will soon
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
nix-build
cd example
../result/bin/rib-example serve --watch
```

Or simply (no cabal file needed),

```bash

nix-shell ../default.nix --run "ghcid -c 'ghci -Wall -i../rib/src Main.hs' -T 'Rib.App.dev Main.settings' --reload=Main.hs"
```



## ... when hacking on Haskell sources

```bash
nix-shell --run "ghcid -c 'cabal new-repl rib-example' -T
'System.Directory.withCurrentDirectory \"example\" \$ Rib.App.dev Main.settings'"
```

### Article Ideas

- Github CI for OSS haskell projects
- Lens and friends
- mtl
- string types
- personal nix cache
