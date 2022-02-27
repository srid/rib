let
  # Use https://status.nixos.org// to find the next hash to update nixpkgs to.
  # Look for the "Last updated" commit hash for the entry `nixpkgs-unstable`
  nixpkgsRev = "53c97404221a9956165bc876b5c476d97ee3d074";
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = "0228c7zdg073k2dvvvbw6ysx6gnwnlz1364rif14k08k75avm49h";
  };

  gitignoreSrc = builtins.fetchTarball { 
    url = "https://github.com/hercules-ci/gitignore/archive/c4662e6.tar.gz";
    sha256 = "1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import gitignoreSrc { }) gitignoreSource;

  ribRoot = gitignoreSource ./.;
in { 
  pkgs ? import nixpkgsSrc {}
, compiler ? pkgs.haskellPackages
, root ? (ribRoot + "/rib")
, name ? "rib"
, source-overrides ? {}
, overrides ? self: super: {}
, additional-packages ? _: []
, ...
}:
let
  optionals = pkgs.lib.lists.optionals;
  myMmark = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.mmark;
in
compiler.developPackage {
  inherit root name;
  source-overrides = {
    rib = ribRoot + "/rib";
    rib-core = ribRoot + "/rib-core";
    lucid = pkgs.fetchFromGitHub {
      owner = "chrisdone";
      repo = "lucid";
      rev = "090fd7dbc6c10269a50cc0f37a1bb40effed730e";
      sha256 = "EUthRB53XrqkWMJhGZpqhJOtwGKMK1UzAE6QZ2sDuI0=";
    };
  } // source-overrides;
  overrides = self: super: with pkgs.haskell.lib; {
    mmark = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.mmark;
    mmark-ext = pkgs.haskellPackages.callCabal2nix "mmark-ext" (pkgs.fetchFromGitHub {
         owner = "mmark-md";
         repo = "mmark-ext";
         rev = "bb7f26f8c2cf98a54472372601ce5f6d5cb9df2d";
         sha256 = "AoXLb2/nCNgJGUeLHaZA9nfIw0b249USNEwvYn0A3uE=";
    });
  } // (overrides self super);
  modifier = with pkgs.haskell.lib;
    let
      addRibDeps = drv:
        addBuildTools drv (with pkgs.haskellPackages;
          [ cabal-install
            ghcid
            haskell-language-server
          ] 
          # Additional packages would be available in `nix-build` as well, only
          # as long as the built executable references it. When using as a
          # Haskell library, however, you will have to override the package and
          # add it to propagateBuildInputs (see neuron for an example).
          ++ additional-packages pkgs
          # Shake recommends fsatrace, but it requires system configuration on
          # macOS.
          ++ optionals (builtins.currentSystem == "x86_64-linux") [pkgs.fsatrace]
        );
    in drv: addRibDeps (dontHaddock drv);
}
