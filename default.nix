let
  # Use https://status.nixos.org// to find the next hash to update nixpkgs to.
  # Look for the "Last updated" commit hash for the entry `nixpkgs-unstable`
  nixpkgsRev = "05f0934825c2";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
  ribRoot = gitignoreSource ./.;
in { 
  pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz") {}
, compiler ? pkgs.haskellPackages
, root ? ribRoot
, name ? "rib"
, source-overrides ? {}
, overrides ? self: super: {}
, additional-packages ? _: []
, ...
}:
let
  pipe = pkgs.lib.trivial.pipe;
  optionals = pkgs.lib.lists.optionals;
in
compiler.developPackage {
  inherit root name;
  source-overrides = {
    rib = ribRoot;
  } // source-overrides;
  overrides = self: super: with pkgs.haskell.lib; {
  } // (overrides self super);
  modifier = with pkgs.haskell.lib;
    let
      addRibDeps = drv:
        addBuildTools drv (with pkgs.haskellPackages;
          [ cabal-install
            ghcid
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
    in drv: pipe drv [
      addRibDeps
      dontHaddock
    ];
}
