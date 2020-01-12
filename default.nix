let
  # Use https://howoldis.herokuapp.com/ to find the next hash to update nixpkgs to.
  # Look for the "Last updated" hash for the entry `nixpkgs-unstable`
  nixpkgsRev = "7e8454fb856";
in { pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz") {}
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

  pp = githubRepo "quchen/prettyprinter" "v1.5.1";
  ppUnpackSymlinks = hp: pkgs.haskell.lib.overrideCabal hp (drv: {
    postUnpack = ''
      cp --remove-destination ${pp}/prettyprinter/misc/version-compatibility-macros.h $sourceRoot/misc/
      cp --remove-destination ${pp}/prettyprinter/LICENSE.md $sourceRoot/
      cp --remove-destination ${pp}/prettyprinter/README.md $sourceRoot/
    '';
  });
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
      githubRepo "commercialhaskell/path" "1d8bdf0";
    path-io =
      githubRepo "mrkkrp/path-io" "84ce6a2";
    relude =
      githubRepo "kowainik/relude" "bfb5f60";

    # The dependencies below are not used in rib; but useful to have for users
    # of the library.
    # TODO: Provide a mechanism for the user to override these.
    dependent-sum =
      let dsum = githubRepo "mokus0/dependent-sum" "5ab6d81"; in "${dsum}/dependent-sum";
    some = githubRepo "phadej/some" "7e2a9ef5352097954a3a416a5ef12bc35b0d53db";  # 1.0.0.3

    # Dhall, and its dependency overrides
    dhall =
      let dhallHaskell = githubRepo "dhall-lang/dhall-haskell" "1.28.0";
      in "${dhallHaskell}/dhall";
    atomic-write = githubRepo "stackbuilders/atomic-write" "v0.2.0.7";
    generic-random = githubRepo "lysxia/generic-random" "1.3.0.0";
    prettyprinter = "${pp}/prettyprinter";

    # TOML parser
    tomland = githubRepo "kowainik/tomland" "d9b7a1d";
  } // source-overrides;
  overrides = self: super: {
    clay = h.dontCheck super.clay;
    mmark = h.dontCheck super.mmark;
    modern-uri = h.dontCheck super.modern-uri;
    megaparsec = h.doJailbreak (h.dontCheck super.megaparsec);  # For tomland
    tomland = h.dontCheck super.tomland;
    path = h.dontCheck super.path;
    path-io = h.doJailbreak super.path-io;  # Override hardcoded dependency on path ==0.6.*
    some = h.doJailbreak super.some;
    relude = h.dontCheck super.relude;
    prettyprinter = h.dontCheck (ppUnpackSymlinks super.prettyprinter);
    dhall = h.dontCheck super.dhall;
    rib = justBuild super.rib;
  };
  modifier =
    let
      platformSpecificDeps =
        if builtins.currentSystem == "x86_64-linux"
        then [pkgs.fsatrace]
        else []; # fsatrace disabled on macOS, and requires sytem configuration.
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
