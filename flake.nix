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
        root = ./.;
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
              lua51Packages.lpeg
            ]);
        overrides = self: super: {
          mmark-ext = pkgs.haskell.lib.dontCheck super.mmark-ext;
          mmark = pkgs.haskell.lib.dontCheck super.mmark;
          pandoc-types = pkgs.haskell.lib.dontCheck super.pandoc-types;
          aeson = pkgs.haskell.lib.dontCheck super.aeson;
          lucid = pkgs.haskell.lib.dontCheck super.lucid;
        };
        source-overrides = {

          # hashable = pkgs.fetchFromGitHub { owner = "haskell-unordered-containers";
          #                                   repo = "hashable";
          #                                   rev = "10c9b57dba7a168d4e7ba8d601e6047a1a3bd47f";
          #                                   sha256 = "JiU/82LLl61ofJklGtT74I4KXBNWdjnKH5Hc5r29NO0=";
          #                                 };
          # with-utf8 = pkgs.fetchFromGitHub { owner = "serokell";
          #                                    repo = "haskell-with-utf8";
          #                                    rev = "b60991303f9cf3f2891479c581549c2d54f45a0e";
          #                                    sha256 = "GRYyW8oi4A8xGcZ4FeZMhlSDOy4LIVEC/Vch8DvSgAw=";
          #                                  };

        };
      };
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.rib;
    devShell = true;
  };
}
