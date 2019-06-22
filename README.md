# notes.srid.ca

My public notes. Kind of a blog and wiki.

## TODO

- Migrate from gitbook
- Publish first Haskell article (nix tutorial)
- Write default.nix that automates everything (including autoreload)

## Article Ideas

- Github CI for OSS haskell projects
- Lens and friends
- mtl
- string types
- personal nix cache

## Running

```
nix-build
./result/bin/notessridca site
nix-shell -p nodePackages.serve --run 'serve dist'
```
