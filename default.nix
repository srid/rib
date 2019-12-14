# Use https://howoldis.herokuapp.com/ to find the next hash to update nixpkgs to.
{ pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/58fb23f72ad.tar.gz") {}
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
in
haskellPackages.developPackage {
  root = root;
  name = name;
  source-overrides = {
    rib = ./.;
    # Override haskell packages here:
    clay =
      githubRepo "sebastiaanvisser/clay" "54dc9ea";
    mmark =
      githubRepo "mmark-md/mmark" "8f5534d";
    mmark-ext =
      githubRepo "mmark-md/mmark-ext" "4d1c40e";
    named =
      githubRepo "monadfix/named" "e684a00";
    pandoc-include-code =
      githubRepo "owickstrom/pandoc-include-code" "7e4d9d9";
    path =
      githubRepo "commercialhaskell/path" "1d8bdf0";
    path-io =
      githubRepo "mrkkrp/path-io" "84ce6a2";
    relude =
      githubRepo "kowainik/relude" "bfb5f60";
    dependent-sum =
      let dsum = githubRepo "mokus0/dependent-sum" "5ab6d81"; in "${dsum}/dependent-sum";
  } // source-overrides;
  overrides = self: super: {
    clay = h.dontCheck super.clay;
    path = h.dontCheck super.path;
    path-io = h.doJailbreak super.path-io;  # Override hardcoded dependency on path ==0.6.*
    relude = h.dontCheck super.relude;
  };
  modifier =
    let
      addExtraDeps =
        (t.flip h.addBuildTools) (with haskellPackages;
          [ cabal-install
            ghcid
          ]);
    in (t.flip t.pipe) [
      addExtraDeps
      h.dontHaddock
    ];
}
