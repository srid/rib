# notes.srid.ca

My public notes. Kind of a blog and wiki.

## TODO

- Reflex based
  - What I'd like to do *first*: write a server that serves static files which
    gets auto-generated on file change. Basically combine `warp` with `fsnotify`.
    - [ ] Use `warp` to serve the current dist directory.
    - [ ] Add `fsnotify` to re-run Shake on file modificaiton (warp server
          should serve the new files automatically)
          - Shake API to do this: https://hackage.haskell.org/package/shake-0.18.2/docs/Development-Shake-Database.html

## Article Ideas

- Github CI for OSS haskell projects
- Lens and friends
- mtl
- string types
- personal nix cache

## Running

```bash
nix-build -A ghc.notessridca
./result/bin/notessridca
nix-shell -p nodePackages.serve --run 'serve dist'
```

Using ghcid:

```bash
nix-shell -A shells.ghc --run ghcid
```
