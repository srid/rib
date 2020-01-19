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
  haskellPackages =
    if compiler == "default"
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
  t = pkgs.lib.trivial;
  h = pkgs.haskell.lib;
  githubRepo = fq: rev:
    builtins.fetchTarball ("https://github.com/" + fq + "/archive/" + rev + ".tar.gz");

  justBuild = p: h.dontHaddock (h.dontCheck p);
in
haskellPackages.developPackage {
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
  overrides = self: super: {
    rib = justBuild super.rib;
    clay = h.dontCheck super.clay;
    mmark = h.dontCheck super.mmark;
    modern-uri = h.dontCheck super.modern-uri;
    tomland = h.dontCheck super.tomland;
    path = h.dontCheck super.path;
    relude = h.dontCheck super.relude;
    some = h.doJailbreak super.some;
  };
  modifier =
    let
      platformSpecificDeps =
        if builtins.currentSystem == "x86_64-linux"
        then [pkgs.fsatrace]
        else []; # fsatrace disabled on macOS, and requires system configuration.
      addExtraDeps =
        (t.flip h.addBuildTools) (with haskellPackages;
          [ cabal-install
            ghcid
          ] ++ platformSpecificDeps
        );
    in (t.flip t.pipe) [
      addExtraDeps
      h.dontHaddock
    ];
}
