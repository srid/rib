# Explaining Haskell

Source code for my planned "Explaining Haskell" documentation project wherein I
would explain the various Haskell concepts that I have come to understand since
2018.

## TODO

- [x] Think of first article to write
- [ ] DOING: Write it
- [ ] DOING: Static site generation (Slick)
- [ ] Deploy to production
- [ ] Share and get feedback on first article

## Article Ideas

See site/drafts/ for existing stubs. In addition, consider these ideas:

- Github CI for OSS haskell projects
-

## Running

```
nix-build
./result/bin/ExplainingHaskell-exe site
nix-shell -p nodePackages.serve --run 'serve dist'
```
