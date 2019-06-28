# notes.srid.ca

My public notes. Kind of a blog and wiki.

## Local server when editing content

```bash
nix-build -A ghc.notessridca
./result/bin/notessridca serve --watch
```

## Hacking on Main.hs

```bash
nix-shell -A shells.ghc --run "ghcid -T 'Main.runApp (Main.Serve 8080 True)'"
```

## TODO

- Reflex based
  - What I'd like to do *first*: write a server that serves static files which
    gets auto-generated on file change. Basically combine `warp` with `fsnotify`.
    - [ ] Use `warp` to serve the current dist directory.
    - [ ] Add `fsnotify` to re-run Shake on file modificaiton (warp server
          should serve the new files automatically)
          - Shake API to do this: https://hackage.haskell.org/package/shake-0.18.2/docs/Development-Shake-Database.html
- Literate haskell
  - Include rendered Main.lhs as a post in notes.srid.ca

### Article Ideas

- Github CI for OSS haskell projects
- Lens and friends
- mtl
- string types
- personal nix cache
