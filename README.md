# notes.srid.ca

My public notes. Kind of a blog and wiki. This will soon be a generic static
site generator based on Shake and Reflex.

## Local server when editing only content

```bash
nix-build -A ghc.notessridca
./result/bin/notessridca serve --watch
```

## ... when hacking on Haskell sources

```bash
nix-shell -A shells.ghc --run "ghcid -T Main.dev"
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
