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
    relude =
      githubRepo "kowainik/relude" "bfb5f60";
  } // source-overrides;
  overrides = self: super: with pkgs.haskell.lib; {
    clay = dontCheck super.clay;
    relude = dontCheck super.relude;
  };
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = with haskellPackages;
      (attrs.buildTools or []) ++ [cabal-install ghcid] ;
  });
}
