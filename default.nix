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
  } // source-overrides;
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
