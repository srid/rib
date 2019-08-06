---
title: Introduction
description: What is, and *why* use, Rib?
---

Rib is a Haskell library for writing your own **static site generator**.

Rib's unique advantages are:

- Use Haskell DSL to define the HTML & CSS of your site
- Use the [Shake](https://shakebuild.com/) build system
- Remain as simple as possible to use

Rib was designed to use *existing* tools instead of reinventing them while also
giving the user full permission to compose them as they wish, instead being
forced to use any particular framework.

Read the [next section](/getting-started) to understand how to write your own
static site generator using Rib.

## Powered by

Rib makes use of the following technologies.

### Shake

Like Makefile but in Haskell:
https://shakebuild.com/

### Pandoc

Parse documents in various formats such as Markdown, reST, org, etc.: 
https://pandoc.org/

### Lucid

Write HTML in Haskell:
https://chrisdone.com/posts/lucid2/

### Clay

Write CSS in Haskell: 
http://fvisser.nl/clay/

### Nix

Rib is best used with [nix](https://nixos.org/nix/); Nix makes it dead-easy
to create reproducible dev environments---a Haskell one in our case. You do not
necessarily have to use Nix for working with Rib sites, but rib provides
necessary Nix expressions to make your job easier. See
[here](https://notes.srid.ca/haskell-nix) for a Nix tutorial for Haskellers.
