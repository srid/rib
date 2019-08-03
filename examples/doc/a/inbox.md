---
title: Random stuff related to Rib
draft: 'True'
---

## Tasks

Are these blog-worthy? Write articles in doc/?

- [X] Drafts
- [X] Simplify `Settings` (seems gobbled together)
- [X] Rib.Shake combinators
- [X] Pandoc metadata refactoring (using type class)
- [X] Metadata file (eg: `foo.rst.yaml`)

### Milestone: public announcement

- Rib as library
  - [X] Finish invoice generator example (as simple as possible)
  - [ ] Publish to Hackage
  - [ ] Add haddock before next publishing
  - [ ] Add default.nix to each example, pulling rib from Github, and using
        ghcid on it.
  - [ ] Create Github project template to get started easily
    - Emphasize the prequisitive of Nix!
  - [ ] Finalize examples
    - [ ] Include a demo version of them running somewhere.

### Rib, for journaling

- [X] Support orgmode format
- [ ] Render Seinfeld diary view of good/bad/etc.

### [ ] Rib, for invoice generation

- Values in Main.hs top-level
- Lucid to render (where Rib comes into play)
- Use google-chrome --headless to directly convert to PDF

- No need for serve/watch; just generate. Make ./ghcid generic enough for this?

### Rib, for SlowNews

Drop obelisk; generate HTML statically.

- Generate only once a week
- Implement emailing as someone requested
- a/config.json => a/index.html?
