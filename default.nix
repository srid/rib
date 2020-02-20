let
  # Use https://howoldis.herokuapp.com/ to find the next hash to update nixpkgs to.
  # Look for the "Last updated" hash for the entry `nixpkgs-unstable`
  nixpkgsRev = "d5bf8b23592";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
  ribRoot = gitignoreSource ./.;
in { 
  pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz") {}
, compiler ? "default"
, root ? ribRoot
, name ? "rib"
, source-overrides ? {}
, ...
}:
let
  pipe = pkgs.lib.trivial.pipe;
  optionals = pkgs.lib.lists.optionals;
  githubRepo = fq: rev:
    builtins.fetchTarball ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
  dhallSrc = githubRepo "dhall-lang/dhall-haskell" "3ed23be15682fab70c1b3e78f262a822cddb6ae1";
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
      githubRepo "owickstrom/pandoc-include-code" "7e4d9d9";
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
      cp -r -L ${prettyprinterSrc}/prettyprinter $out
    '';
  } // source-overrides;
  overrides = self: super: with pkgs.haskell.lib; {
    shake = dontCheck super.shake;  # Tests fail on 0.18.5
    dhall = dontCheck super.dhall;  # Until https://github.com/srid/rib/issues/100
  };
  modifier = with pkgs.haskell.lib;
    let
      addRibDeps = drv:
        addBuildTools drv (with pkgs.haskellPackages;
          [ cabal-install
            ghcid
          ] 
          # Shake recommends fsatrace, but it requires system configuration on
          # macOS.
          ++ optionals (builtins.currentSystem == "x86_64-linux") [pkgs.fsatrace]
        );
    in drv: pipe drv [
      addRibDeps
      dontHaddock
    ];
}
