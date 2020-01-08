# Developing rib

Use ghcid for quicker compilation cycles:

```
nix-shell --run ghcid
```

To test your changes, clone [rib-sample](https://github.com/srid/rib-sample) and run it using your local rib checkout:

```
cd ..
git clone https://github.com/srid/rib-sample.git
cd rib-sample
nix-shell --arg rib ../rib --run 'ghcid -T main'
```

## Release steps

1. Create a `release-x.y` branch
1. Finalize ChangeLog.md
1. Run `cabal haddock` and sanity check the haddocks
1. Commit all changes, and push a release PR.
1. Generated sdist using `cabal sdist`
1. [Upload a package candidate](https://hackage.haskell.org/packages/candidates/upload)
1. Sanity check the upload, and then "publish" it publicly.
1. Run `cabal haddock --haddock-for-hackage` to generated haddocks for hackage.
1. Run `cabal upload -d --publish $PATH_TO_TARBALL` to update haddocks on the release.
1. Squash merge the PR.
1. [Draft a new release](https://github.com/srid/rib/releases) on Github. Copy paste the change log. This will automatically create and push the new git tag.

### Post-release

1. Increment cabal version in .cabal file
1. Plan, as first task, updating of nixpkgs and package dependencies.
