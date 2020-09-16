let
  # Use https://status.nixos.org// to find the next hash to update nixpkgs to.
  # Look for the "Last updated" commit hash for the entry `nixpkgs-unstable`
  nixpkgsRev = "6d4b93323e7f";
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = "0g2j41cx2w2an5d9kkqvgmada7ssdxqz1zvjd7hi5vif8ag0v5la";
  };

  gitignoreSrc = builtins.fetchTarball { 
    url = "https://github.com/hercules-ci/gitignore/archive/c4662e6.tar.gz";
    sha256 = "1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import gitignoreSrc { }) gitignoreSource;

  ribRoot = gitignoreSource ./.;
in { 
  pkgs ? import nixpkgsSrc {}
, compiler ? pkgs.haskellPackages
, root ? (ribRoot + "/rib")
, name ? "rib"
, source-overrides ? {}
, overrides ? self: super: {}
, additional-packages ? _: []
, ...
}:
let
  optionals = pkgs.lib.lists.optionals;
in
compiler.developPackage {
  inherit root name;
  source-overrides = {
    rib = ribRoot + "/rib";
    rib-core = ribRoot + "/rib-core";
  } // source-overrides;
  overrides = self: super: with pkgs.haskell.lib; {
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
    in drv: addRibDeps (dontHaddock drv);
}
