---
title: "Getting Started"
description: Start using Rib to generate your own static website
---

We will use `Rib.Simple` to create the simplest static site possible---a site
with a list of posts and optional static files. First create the required
directory layout (content will be under directory `a`; and generated files will
be under directory `b`).


```bash
mkdir -p mysite/a mysite/b
cd mysite
```

Add some content by creating the file `a/first-post.md` with the following content:


```markdown
# Hello world

_This_ file is written in **Markdown**.

    Life is meant to be fun!
```


Finally, add the Haskell source `Main.hs` that wires everything together. Notice
the following:

- We use `Rib.Simple.buildAction` that wires together the necessary Shake
combinators (defined in `Rib.Shake`) for us---take a peek at that module if
you'd like to customize the behaviour of static site generation by writing your
own Shake action.

- `App.run` provides file monitoring and http serving on top of site
generation.

- Everything else in this file is your site specific HTML and CSS
written in Haskell DSL (via `Lucid` and `Clay`).

```{.haskell include=../example/Main.hs}
```

Include the `rib` library in your repo, install Nix and invoke the ghcid script:

```bash
git clone https://github.com/srid/rib rib
./rib/ghcid
```

This will launch the Rib app server running at http://localhost:8080. Watch it
regenerate the HTML files as you change the source (be they content or Haskell sources!).
