{
  description = "Rib, a static site generator based on Shake.";
  inputs = {
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    # rib = { url = "."; flake = false; };
    # Pin nixpkgs
    # nixpkgs = { url = "github:NixOS/nixpkgs/5272327b81ed355bbed5659b8d303cf2979b6953"; flake = false; }; # 20.03
    # nixpkgs = { url = "github:NixOS/nixpkgs/cd63096d6d887d689543a0b97743d28995bc9bc3"; flake = false; }; # 20.09
    # nixpkgs = { url = "github:NixOS/nixpkgs/7e9b0dff974c89e070da1ad85713ff3c20b0ca97"; flake = false; }; # 21.05
    nixpkgs = { url = "github:NixOS/nixpkgs"; flake = false; }; # Master
  };
  outputs = { self, nixpkgs, ... }@inputs: {
    packages.x86_64-linux.rib =
      with import nixpkgs { system = "x86_64-linux"; config = { allowBroken = true; doCheck = false; }; };
      pkgs.haskellPackages.developPackage {
        root = ./rib;
        name = "rib";
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
            [
              cabal-install
              cabal-fmt
              pkgs.nixpkgs-fmt
              ghcid
              ormolu
              haskell-language-server
              zlib
            ]);
        overrides = self: super: {
          mmark-ext = pkgs.haskell.lib.dontCheck super.mmark-ext;
          mmark = pkgs.haskell.lib.dontCheck super.mmark;
          pandoc-types = pkgs.haskell.lib.dontCheck super.pandoc-types;
          aeson = pkgs.haskell.lib.dontCheck super.aeson;
          lucid = pkgs.haskell.lib.dontCheck super.lucid;
        };
        source-overrides = {
          rib = ./rib;
          rib-core = ./rib-core;
        };
      };
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.rib;
    devShell = true;
  };
}
