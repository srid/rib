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
- [ ] Tasks

### Rib, for documentation

- [X] MVP - rib.srid.ca
- [X] getting-started: instruct cp example/Main.hs and create a separate page
      explaining Main.hs (literate haskell if possible;)

### Rib, for journaling

Start ./examples/journal and use example data, while a private version to use
the notes in my Dropbox.

- [ ] Render Seinfeld diary view of good/bad/etc.

- [ ] Make it easy to add entries (some cli script)
  - As simple as getting today's date for the file name
  - Goal: collect years of journal data, so I can look back say 4 years
  
- [ ] Search 
  - If I have years of data, it makes sense to provide search feature.
    - Just use doom-emacs project search!?
    
- [ ] Just use org-mode!

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
