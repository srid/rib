{ reflex-platform ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "716879f16d53c93766e7ed9af17416fccb2edfe1";
    sha256 = "1mngxa24cfpvxxq4hgh77nw36vny4abl5wi2xmlwpkk25wzm0h0x";
  }) {}
}:

reflex-platform.project ({ pkgs, hackGet, ... }: {
  packages = {
    rib = ./.;
    pandoc = hackGet ./dep/pandoc;
    hslua-module-system = hackGet ./dep/hslua-module-system;
    hslua-module-text = hackGet ./dep/hslua-module-text;
    ipynb = hackGet ./dep/ipynb;
    tasty-lua = hackGet ./dep/tasty-lua;
    hslua = hackGet ./dep/hslua;
    skylighting = (hackGet ./dep/skylighting) + /skylighting;
    skylighting-core = (hackGet ./dep/skylighting) + /skylighting-core;
    cmark-gfm = hackGet ./dep/cmark-gfm-hs;
    texmath = hackGet ./dep/texmath;
  };

  overrides = self: super: with pkgs.haskell.lib; {
    hslua-module-system = dontCheck super.hslua-module-system;
    hslua-module-text = dontCheck super.hslua-module-text;
  };

  shells = {
    ghc = ["rib"];
  };
})
