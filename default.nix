{ reflex-platform ? import ./reflex-platform.nix {} }:

reflex-platform.project ({ pkgs, ... }: {
  packages = {
    notessridca = ./.;
  };

  shells = {
    ghc = ["notessridca"];
  };
})
