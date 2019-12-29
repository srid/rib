# Change Log for rib

## 0.6.0.0 - UNRELEASED

- API: Allow multiple parsers in the same static site
  - Changes API of `Rib.buildHtmlMulti`, to use `Data.Some`
  - Changes the `documentVal` of `Document` type, to use `Data.Dependent.DSum`
- API: Expose `ribInputDir` and `ribOutputDir` for use in custom Shake actions
- Fix #63: create intermediate directories when generating post HTML
- Advance nixpkgs; require Shake >=0.18.4
- API: Remove `Markup` type, and adjust Shake API to allow specifying it via `Proxy` (This allow the user to add their own custom markup type without forking rib)

## 0.5.0.0

This release comes with a major API refactor. Key changes:

- Added MMark support, as an alternative to Pandoc
- Allows using arbitrary records to load metadata
  - This replaces the previous complex metadata API
- Added `Document` type that uses the custom metadata record
- Add top-level `Rib` import namespace for ease of use
- Remove the following:
  - JSON cache
  - `Rib.Simple`
- Support for Table of Contents via MMark

Other changes:

- Use type-safe path types using the [path](http://hackage.haskell.org/package/path) library.
- Fix #40: Gracefully handle rendering/ parsing errors, without dying.
- Misc error reporting improvements

## 0.4.1.0

- `Rib.Pandoc`: 
  - Export `render'` and `renderInlines'` (the non-Lucid versions)
  - Re-export `Text.Pandoc.Readers` so the library user does not have to directly depend on `pandoc` only to render its documents.
- `Rib.App`: The `run` funtion now takes two more arguments, specifying the input and output directory, which are no longer hardcoded.
- `Rib.Simple`: add LaTeX to default list of readers
- `Rib.Server`: Remove ".html" detection magic from URLs

## 0.3.0.0

- Rename `Rib.App.Watch` to `Rib.App.WatchAndGenerate`

## 0.2.0.0

- Initial release.
