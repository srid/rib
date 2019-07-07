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
    pandoc-types = hackGet ./dep/pandoc-types;
    hslua-module-system = hackGet ./dep/hslua-module-system;
    hslua-module-text = hackGet ./dep/hslua-module-text;
    ipynb = hackGet ./dep/ipynb;
    tasty-lua = hackGet ./dep/tasty-lua;
    hslua = hackGet ./dep/hslua;
    skylighting = (hackGet ./dep/skylighting) + /skylighting;
    skylighting-core = (hackGet ./dep/skylighting) + /skylighting-core;
    cmark-gfm = hackGet ./dep/cmark-gfm-hs;
    texmath = hackGet ./dep/texmath;
    haddock-library = (hackGet ./dep/haddock) + /haddock-library;
    haddock-api = (hackGet ./dep/haddock) + /haddock-api;
    # haskell-ide-engine = ./dep/haskell-ide-engine;
  };

  overrides = self: super: with pkgs.haskell.lib;
  let 
    skylighting-core = overrideCabal super.skylighting-core (drv: {
      isExecutable = true;
      isLibrary = true;
      configureFlags = [ "-fexecutable" ];  # We need the CLI tool later.
    });
  in
  {
    hslua-module-system = dontCheck super.hslua-module-system;
    hslua-module-text = dontCheck super.hslua-module-text;
    skylighting-core = skylighting-core;
    skylighting = overrideCabal super.skylighting (drv: {
      preConfigure = ''
        ${skylighting-core}/bin/skylighting-extract ${skylighting-core}/xml/*.xml
        rm -f changelog.md; touch changelog.md  # Workaround failing symlink access
      '';
      isExecutable = true;
      isLibrary = true;
    });
    # pandoc = doJailbreak super.pandoc;  # Remove the version lock on `haddock-library`
  };

  shells = {
    ghc = ["rib"];
  };
})
