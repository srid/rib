{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "ca.srid.ExplainingHaskell";
  android.displayName = "Explaining Haskell";
  ios.bundleIdentifier = "ca.srid.ExplainingHaskell";
  ios.bundleName = "Explaining Haskell";

  packages = {
    # Newer mmark requires newer version of certain dependencies as well
    mmark = hackGet ./dep/mmark;
    mmark-ext = hackGet ./dep/mmark-ext;
    megaparsec = hackGet ./dep/megaparsec;
    hspec-megaparsec = hackGet ./dep/hspec-megaparsec;
    modern-uri = hackGet ./dep/modern-uri;

    # Use a version of neat-interpolation that works with the megaparsec version
    # used by mmark above.
    neat-interpolation = hackGet ./dep/neat-interpolation;
  };

  overrides = self: super: with pkgs.haskell.lib; {
    mmark = dontCheck super.mmark;
    mmark-ext = dontCheck super.mmark-ext;
    megaparsec = dontCheck super.megaparsec;
  };
})
