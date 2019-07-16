---
title: Introduction
description: What is, and *why* use, Rib?
order: 0
next: getting-started.md
---

Rib was designed to use *existing* tools instead of reinventing them while giving the
user full permission to compose them as they wish, instead of forcing any
particular framework.

## Powered by

### Shake

https://shakebuild.com/

### Lucid

https://chrisdone.com/posts/lucid2/

### Clay

http://fvisser.nl/clay/

### Nix

Rib is best used with [nix](https://nixos.org/nix/); Nix makes it dead-easy
to create reproducible dev environments---a Haskell one in our case. The
included `ghcid` script uses Nix to install everything including GHC itself for
your Haskell program to build and run.
