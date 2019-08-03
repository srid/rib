{ pkgs ? import <nixpkgs> {}
, compiler ? "default"
, root ? ./.
, source-overrides ? {}
, ...
}:
let
  haskellPackages =
    if compiler == "default"
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
in
haskellPackages.developPackage {
  root = root;
  source-overrides = {
    clay = pkgs.fetchFromGitHub {
      owner = "sebastiaanvisser";
      repo = "clay";
      rev = "54dc9eaf0abd180ef9e35d97313062d99a02ee75";
      sha256 = "0y38hyd2gvr7lrbxkrjwg4h0077a54m7gxlvm9s4kk0995z1ncax";
    };
    pandoc-include-code = pkgs.fetchFromGitHub {
      owner = "owickstrom";
      repo = "pandoc-include-code";
      rev = "7e4d9d967ff3e3855a7eae48408c43b3400ae6f4";
      sha256 = "0wvml63hkhgmmkdd2ml5a3g7cb69hxwdsjmdhdzjbqbrwkmc20rd";
    };
    rib = ./.;
  } // source-overrides;

  overrides = self: super: with pkgs.haskell.lib; {
    clay = dontCheck super.clay;
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [haskellPackages.ghcid] ;
  });
}
