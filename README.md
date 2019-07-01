# rib

<!--
Credit for this image: https://www.svgrepo.com/svg/24439/ribs
-->
<img src="https://raw.githubusercontent.com/srid/rib/master/site/images/ribs.svg?sanitize=true" width="150" />

Rib is a static site generator written in Haskell using sensible technologies
like `Shake` and `Reflex`. See `./example` to see how the library can be used;
it contains the author's actual website. It is still a work in progress but will
soon be ready for general use.

## Local server when editing only content

```bash
nix-build -A ghc.rib
./result/bin/rib serve --watch
```

## ... when hacking on Haskell sources

```bash
nix-shell -A shells.ghc --run "ghcid -c 'cabal new-repl rib-example' -T 'System.Directory.withCurrentDirectory \"example\" \$ Main.dev'"
```

## TODO

- Use my own jsonCache' and drop dependency on Slick

### Article Ideas

- Github CI for OSS haskell projects
- Lens and friends
- mtl
- string types
- personal nix cache
