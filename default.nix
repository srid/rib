let
  # Use https://howoldis.herokuapp.com/ to find the next hash to update nixpkgs to.
  # Look for the "Last updated" hash for the entry `nixpkgs-unstable`
  nixpkgsRev = "c438ce12a85";
in { 
  pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz") {}
, compiler ? "default"
, root ? ./.
, name ? "rib"
, source-overrides ? {}
, ...
}:
let
  pipe = pkgs.lib.trivial.pipe;
  githubRepo = fq: rev:
    builtins.fetchTarball ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");
in
pkgs.haskellPackages.developPackage {
  inherit root name;
  source-overrides = {
    rib = ./.;
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
  } // source-overrides;
  modifier = with pkgs.haskell.lib;
    let
      platformSpecificDeps =
        if builtins.currentSystem == "x86_64-linux"
        # Shake recommends fsatrace, but it requires system configuration on macOS.
        then [pkgs.fsatrace]
        else [];
      addExtraDeps = drv:
        addBuildTools drv (with pkgs.haskellPackages;
          [ cabal-install
            ghcid
          ] ++ platformSpecificDeps
        );
    in drv: pipe drv [
      addExtraDeps
      dontHaddock
    ];
}
