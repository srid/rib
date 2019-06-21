---
title: "Nix tutorial for Haskellers"
author: Sridhar Ratnakumar
date: Jul 19, 2019
tags: [nix]
description: How to develop Haskell projects using Nix
---

The goal of this article is to get you comfortable managing simple Haskell
programs and projects using the **Nix** package manager.

## Why Nix?

TODO

## Prerequisitives

Before proceeding make sure that Nix is installed in your environment by
following the [installation instructions here](https://nixos.org/nix/). If you
are feeling adventurous enough to try a new Linux distribution that is based on
the principles of Nix, you may alternatively install [NixOS](https://nixos.org/).

## Hello World

Let's start with the simplest scenario of all--running a single Haskell source
file with no library dependencies.

```haskell
$ cat > HelloWorld.hs
module Main where

main :: IO ()
main = do
  putStrLn "Hello World"
^D
$
```

To compile and run this module let's use `runhaskell` (which in turn runs
[`runghc`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runghc.html)),
which is part of the `ghc` Nix package. This raises a question: how do we
install a Nix package? The `nix-env -i ghc` will do just that. However we do not
*have* to install packages in the global user enviroment just to be able to use
them (this is one of benefits of using Nix over other package managers). If
`runhaskell` is only needed to run this HelloWorld module, instead we can enter
a bash shell with the `ghc` package installed and activated within it (and only
it). The `nix-shell`, command with its `-p` argument, does exactly that:

```
# This drops us in a shell with ghc package installed
$ nix-shell -p ghc
...
# Now let's run our module
[nix-shell:~] runhaskell HelloWorld.hs
Hello World

# When you exit the shell the ghc package is no longer in scope (although still
cached in the local nix store for future use)
[nix-shell:~] ^D
```


## Package dependencies

What if our `HelloWorld.hs` used an external library dependency?

```
TODO
```

This version of the module imports `Text.Pandoc` which comes from the `pandoc`
Haskell package. Nix allows us to install individual Haskell packages as well.
To enter a shell with the pandoc package installed:

```
# TODO: But why ghcWithPackages
$ nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [pandoc])"
...
[nix-shell:~] runhaskell HelloWorld.hs
```

## Cabal project

Being able to develop with a single source file is the easiest path to
developing in Haskell, however any non-trivial program will end up using the
project facilities provided by tools like Cabal. Nix can integrate quite well
with Cabal, thanks to its `callCabal2nix`.

To illustrate this you may use your existing cabal project, or create one from
scratch:

```
$ cabal init
...
```

Nix's development tools (`nix-build`, `nix-shell`) by default use a filename
called `default.nix`. And the nix package store (`nixpkgs`) provides us with a
helper function called `haskellPackages.developPackage` to make Haskell
developer easier. Let's use that for our newly created Cabal project:

```
$ cat > default.nix
(import <nixpkgs> { }).haskellPackages.developPackage {
  root = ./.;
}
```

Here, we tell `developPackage` that the root of our Cabal project is the same
directory where default.nix lives. That's all is required to tell Nix about our
Cabal project.


## Adding build tools

TODO: can we use `addBuildTools`
