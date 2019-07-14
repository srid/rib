---
title: "Nix tutorial for Haskellers"
description: How to develop *Haskell* projects using *Nix*
category: 'Programming'
---

The goal of this article is to get you comfortable managing simple Haskell
programs and projects using the **Nix** package manager.

## Why Nix?

TODO

## Prerequisites

If you are on Linux or macOS follow the [installation instructions
here](https://nixos.org/nix/) to install Nix. Alternatively, if you
are feeling adventurous enough, install [NixOS](https://nixos.org/) (a Linux
distribution based on Nix).

## Hello World

We will begin with using Nix to run the simplest of Haskell code, a single
module printing "Hello World".

```haskell
-- HelloWorld.hs
module Main where

main :: IO ()
main = putStrLn "Hello World"
```

In order to compile and run this program we must install the `ghc` Nix package.
[Nix manual](https://nixos.org/nix/manual/#chap-package-management) informs us
to use `nix-env -i ghc` to do this. A better approach would be to create a
shell environment containing the requested package (instead of installing them
in a global scope) using the `nix-shell -p ghc` command.

```bash
# This drops us in a bash shell with ghc package installed and $PATH updated.
$ nix-shell -p ghc
...
# Now let's run our module.
[nix-shell:~] runhaskell HelloWorld.hs
Hello World
```

Running `nix-shell -p ghc` will be slower the first time, as Nix would be
downloading ghc and its dependencies. The subsequent runs will be near
instantaneous as packages will be cached in the Nix store.

## Package dependencies

Let us extend our code to work with
[brick](http://hackage.haskell.org/package/brick) UI library.

```haskell
module Main where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui
```

We need to tell Nix of the new package dependency. Packages in Nix come from the
official channel called [nixpkgs](https://github.com/NixOS/nixpkgs), which
provides a function called `ghcWithPackages` that can be used to install the
`ghc` package along with a custom list of dependencies.

```
$ nix-shell \
    -p "haskellPackages.ghcWithPackages (ps: with ps; [pandoc])" \
    --run "runhaskell HelloWorld.hs"
```

Note how we directly ran the command instead of being dropped in shell. This
allows us to treat our Haskell source file as a runnable script using the
shebang.

## Cabal project

Nix can be used to create a reproducible development environment for your Cabal
projects. First create a file called `default.nix` in your project root. This
file is by default used by commands like `nix-build` and `nix-shell`.

```nix
(import <nixpkgs> { }).haskellPackages.developPackage {
  root = ./.;
}
```

Now if you run `nix-shell` it will you drop in a shell with all Haskell
dependencies installed; from here you can run your `cabal` commands, and
everything will be function as expected.

```bash
$ nix-shell
...
[nix-shell:~] cabal new-build
..
```

## Overriding dependencies

Your project may depend on a library that is not on Hackage, or it may depend on
a forked version of an existing library. In Nix overriding packages is rather
straightforward, and the `developPackage` function exposes it via the
`source-overrides` attribute. Suppose your cabal project depends on the
aeson-gadt-th package at this particular git revision, then you would modify
your `default.nix` to look like:

```nix
let
  pkgs = import <nixpkgs> { };
  compilerVersion = "ghc844"; 
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
compiler.developPackage {
  root = ./.;
  source-overrides = {
    aeson-gadt-th = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "aeson-gadt-th";
      rev = "e40c293901a9cb9be4b0748109f4bc6806bfdb79";
      sha256 = "08iqyzd4240g7af2lwgxmqbfglyxxii43i2zi01xmk1kg8inzs3v";
    };
  };
}
```

In order to compute the `sha256` hash you can use `nix-prefetch-git`:

```bash
nix-prefetch-git \
  --url https://github.com/obsidiansystems/aeson-gadt-th.git \
  --rev e40c293901a9cb9be4b0748109f4bc6806bfdb79
...
hash is 08iqyzd4240g7af2lwgxmqbfglyxxii43i2zi01xmk1kg8inzs3v
{
  "url": "https://github.com/obsidiansystems/aeson-gadt-th.git",
  "rev": "e40c293901a9cb9be4b0748109f4bc6806bfdb79",
  "date": "2019-03-28T17:00:03-04:00",
  "sha256": "08iqyzd4240g7af2lwgxmqbfglyxxii43i2zi01xmk1kg8inzs3v",
  "fetchSubmodules": false
}
```
