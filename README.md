# notes.srid.ca

My public notes. Kind of a blog and wiki.

## TODO

- Write default.nix that automates everything (including autoreload)
  - Or revamp this whole thing, using Reflex.

## Article Ideas

- Github CI for OSS haskell projects
- Lens and friends
- mtl
- string types
- personal nix cache

## Running

```bash
nix-build
./result/bin/notessridca site
nix-shell -p nodePackages.serve --run 'serve dist'
```

Using ghcid:

```bash
nix-shell --run "hpack; ghcid"
```
