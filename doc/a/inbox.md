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
  - [X] Post slugs and `guide.json`
  - [X] Pandoc metadata system (at least, as simple as `setPandocMetaValue`)
  - [ ] Inject next/prev keys into Pandoc metadata
    - [X] Patch `DocPage` in the render function passed to `buildHtmlMulti`
      - Need to do a lookup guide.json each time, which is okay.
    - [ ] Include title from yet-unparsed next/prev docs. How? :-S


### Rib, for journaling

- [ ] Render Seinfeld diary view of good/bad/etc.

### [ ] Rib, for invoice generation

- Lucid to render
- Use google-chrome --headless to convert to PDF
