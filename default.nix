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
  fetchGH = fqrepo: rev: builtins.fetchTarball ("https://github.com/" + fqrepo + "/archive/" + rev + ".tar.gz");
  pandocSrc = fetchGH "jgm/pandoc" "a946424";
  sky = fetchGH "jgm/skylighting" "0.8.2.3";
  haddock = fetchGH "haskell/haddock" "2a5fc0a";
in
haskellPackages.developPackage {
  root = root;
  name = name;
  source-overrides = {
    rib = ./.;
    clay = fetchGH "sebastiaanvisser/clay" "54dc9ea";
    pandoc-include-code = fetchGH "srid/pandoc-include-code" "bb025b8";

    # Use newer pandoc, to use the well-functioning `toTableOfContents`
    # TODO: Move these to nix/pandoc.nix
    pandoc = pandocSrc;
    doclayout = fetchGH "jgm/doclayout" "0.2.0.1";
    doctemplates = fetchGH "jgm/doctemplates" "0.7.1";
    pandoc-types = fetchGH "jgm/pandoc-types" "1.20";
    skylighting  = "${sky}/skylighting";
    skylighting-core = "${sky}/skylighting-core";
    texmath = fetchGH "jgm/texmath" "0.12";
    regex-pcre = fetchGH "haskell-hvr/regex-pcre" "v0.95.0.0";
    regex-pcre-builtin = fetchGH "audreyt/regex-pcre-builtin" "0124444";
    regex-base = fetchGH "haskell-hvr/regex-base" "v0.94.0.0";
    regex-posix = fetchGH "haskell-hvr/regex-posix" "v0.96.0.0";
    HsYAML = fetchGH "haskell-hvr/HsYAML" "v0.2.1.0";
    haddock-library = "${haddock}/haddock-library";
  } // source-overrides;

  overrides = self: super: with pkgs.haskell.lib;
    let
      skylighting-core = overrideCabal super.skylighting-core (drv: {
        isExecutable = true;
        isLibrary = true;
        configureFlags = [ "-fexecutable" ];  # We need the CLI tool later.
      });
    in
  {
    clay = dontCheck super.clay;
    haddock-library = doJailbreak super.haddock-library;
    pandoc = dontCheck super.pandoc;
    pandoc-types = dontCheck super.pandoc-types;
    Glob = dontCheck super.Glob;
    skylighting-core = overrideCabal super.skylighting-core (drv: {
      preConfigure = ''
        rm -f changelog.md; touch changelog.md  # Workaround failing symlink access
      '';
    });
    skylighting = overrideCabal super.skylighting (drv: {
      preConfigure = ''
        ${skylighting-core}/bin/skylighting-extract ${skylighting-core}/xml/*.xml
        rm -f changelog.md; touch changelog.md  # Workaround failing symlink access
      '';
      isExecutable = true;
      isLibrary = true;
    });
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = with haskellPackages; (attrs.buildTools or []) ++ [cabal-install ghcid] ;
  });
}
