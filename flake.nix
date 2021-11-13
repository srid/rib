{
  description = "Rib, a static site generator based on Shake.";
  inputs = {
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    # Pin nixpkgs to nixos-unstable as of 2021-11-12
    nixpkgs = { url = "github:NixOS/nixpkgs/92204897f3d80e7034168d883741dffae99ede5c"; };
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, ... }@inputs: {
    packages.x86_64-linux.rib =
      let pkgs = import nixpkgs { system = "x86_64-linux"; config = { allowBroken = true; }; };
      in pkgs.haskellPackages.developPackage {
        root = ./rib;
        name = "rib";
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
            [
              cabal-install cabal-fmt pkgs.nixpkgs-fmt ghcid
              ormolu haskell-language-server zlib
            ]);
        overrides = self: super: with pkgs.haskell.lib; {
          mmark-ext = dontCheck super.mmark-ext;
          mmark = dontCheck super.mmark;
          pandoc-types = dontCheck super.pandoc-types;
          aeson = dontCheck super.aeson;
          lucid = dontCheck super.lucid;
        };
        source-overrides = {
          # rib = ./rib;
          rib-core = ./rib-core;
        };
      };
    packages.x86_64-linux.rib-core =
      let pkgs = import nixpkgs { system = "x86_64-linux"; };
      in pkgs.haskellPackages.developPackage {
        root = ./rib-core;
        name = "rib-core";
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
            [
              cabal-install cabal-fmt pkgs.nixpkgs-fmt
              ghcid ormolu haskell-language-server zlib
            ]);
        overrides = self: super: with pkgs.haskell.lib; {
          mmark-ext = dontCheck super.mmark-ext;
          mmark = dontCheck super.mmark;
          pandoc-types = dontCheck super.pandoc-types;
          aeson = dontCheck super.aeson;
          lucid = dontCheck super.lucid;
        };
        source-overrides = {
          rib = ./rib;
          # rib-core = ./rib-core;
        };
      };
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.rib;
    devShell = self.packages.x86_64-linux.rib;
  };
}
