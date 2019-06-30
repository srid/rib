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

- Use my own jsonCache' and drop dependency on Slick
- Literate haskell
  - Include rendered Main.lhs as a post in notes.srid.ca

### Article Ideas

- Github CI for OSS haskell projects
- Lens and friends
- mtl
- string types
- personal nix cache
