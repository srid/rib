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

    # Override haskell packages here:
    clay =
      githubRepo "sebastiaanvisser/clay" "cc7729b";
    megaparsec =
      githubRepo "mrkkrp/megaparsec" "90b4608";
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

    # The dependencies below are not used in rib; but useful to have for users
    # of the library.
    # TODO: Move these to rib-sample, as a commented out example.
    dependent-sum =
      let dsum = githubRepo "mokus0/dependent-sum" "5ab6d81"; 
      in "${dsum}/dependent-sum";
    some = githubRepo "phadej/some" "7e2a9ef5352097954a3a416a5ef12bc35b0d53db";  # 1.0.0.3
    # TOML parser
    tomland = githubRepo "kowainik/tomland" "d9b7a1d";
  } // source-overrides;
  overrides = self: super: with pkgs.haskell.lib; {
    mmark = dontCheck super.mmark;  # Test deps use wrong megaparsec version
    modern-uri = dontCheck super.modern-uri;  # Test deps use wrong megaparsec version

    some = doJailbreak super.some;
    tomland = dontCheck super.tomland;
  };
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
