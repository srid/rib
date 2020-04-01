let
  # Use https://howoldis.herokuapp.com/ to find the next hash to update nixpkgs to.
  # Look for the "Last updated" commit hash for the entry `nixpkgs-unstable`
  nixpkgsRev = "24c765c744b";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
  ribRoot = gitignoreSource ./.;
in { 
  pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz") {}
, compiler ? "default"
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
  githubRepo = fq: rev:
    builtins.fetchTarball ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
  dhallSrc = githubRepo "dhall-lang/dhall-haskell" "8761fab";
  dsumSrc = githubRepo "mokus0/dependent-sum" "5ab6d81";
  prettyprinterSrc = githubRepo "quchen/prettyprinter" "320538b";
in
pkgs.haskellPackages.developPackage {
  inherit root name;
  source-overrides = {
    rib = ribRoot;
    clay =
      githubRepo "sebastiaanvisser/clay" "cc7729b";
    megaparsec =
      githubRepo "mrkkrp/megaparsec" "90b4608";
    hspec-megaparsec =
      githubRepo "mrkkrp/hspec-megaparsec" "365a277";
    mmark =
      githubRepo "mmark-md/mmark" "9a6a649";
    mmark-ext =
      githubRepo "mmark-md/mmark-ext" "4d1c40e";
    pandoc-include-code =
      githubRepo "owickstrom/pandoc-include-code" "bc0430f";
    path =
      githubRepo "commercialhaskell/path" "4ceb12b";
    path-io =
      githubRepo "mrkkrp/path-io" "236825b";
    relude =
      githubRepo "kowainik/relude" "ee509c8";
    shake 
      = githubRepo "ndmitchell/shake" "6936aae";
    # For dhall
    dhall = dhallSrc + "/dhall";
    generic-random = githubRepo "Lysxia/generic-random" "1a091b6";
    prettyprinter = pkgs.runCommand "prettyprinter" {}
    ''
      mkdir $out
      cp -r -L ${prettyprinterSrc}/prettyprinter/* $out/
    '';
    # dependent-sum
    dependent-sum = dsumSrc + "/dependent-sum";
    dependent-sum-template = dsumSrc + "/dependent-sum-template";
    some = githubRepo "phadej/some" "7e2a9ef5352097954a3a416a5ef12bc35b0d53db"; # "1998df3";
  } // source-overrides;
  overrides = self: super: with pkgs.haskell.lib; {
    clay = dontCheck super.clay;    # Might fail on Windows Subsystem for Linux
    shake = dontCheck super.shake;  # Tests fail on 0.18.5
    dhall = dontCheck super.dhall;  # Until https://github.com/srid/rib/issues/100
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
