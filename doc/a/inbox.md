---
title: Random stuff related to Rib
draft: 'True'
description: This stuff is hidden if not private
---

## Tasks

- [X] Drafts
- [X] Simplify `Settings` (seems gobbled together)
- [ ] Tasks
- [ ] Pandoc metadata to/from ADT (kind of like FromJSON/ToJSON)
  - Use `flattenMeta` from Slick

### Rib, for documentation

- [X] MVP - rib.srid.ca
- [ ] Navigation (next <> title <> prev)
  - Define post slugs (/inbox.md => "/inbox.md")
    - Make a `map` of posts keyed by their slug
    - Have getPageUrl use this
  - Define hierarchy in Main.hs:
    ```
    postHierarchy = [ "/introduction", "/getting-started" ]
    ```


### Rib, for journaling

- [ ] Render Seinfeld diary view of good/bad/etc.

### [ ] Rib, for invoice generation

- Lucid to render
- Use google-chrome --headless to convert to PDF
