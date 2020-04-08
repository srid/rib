# Change Log for rib

## 0.9.0.0 (UNRELEASED, DEV)

- Misc changes
  - #145: CLI arguments have been revamped
    - Added `--input-dir/--output-dir` to override these paths
    - Exposed `Rib.Shake.getCliConfig` to get full CLI configuration
  - #141: Allow quiet logging (useful when rib is used as a library)

## 0.8.0.0

- Dependency upgrades
  - GHC 8.8
  - pandoc-include-code: 0.5.0.0
  - pandoc-types: 1.20
  - dhall: 1.30
  - clay: 0.13.3 (This is a downgrade, as 0.14 is not released yet)
- New features:
  - API exposes the CLI parser (`optparse-applicative`) for user-level composition
  - Add `Rib.Parser.Pandoc.getToC` returning rendered Table of contents for a Pandoc document
  - Add `Rib.Parser.MMark.getFirstParagraphText`
  - Add `Rib.Extra.OpenGraph` for Open Graph protocol
  - Add to `Rib.Extra.CSS`, `googleFonts` and `stylesheet`
- Bug fixes and misc changes:
  - `routeUrl`: Fix incorrect substitution of "foo-index.html" with "foo-"
  - Lucid rendering functions (like `MMark.render`) are now polymorphic in their monad.
  - #122: Fix Pandoc parser never returning metadata
  - #127: Rib's HTTP server now binds to `127.0.0.1`.
  - Allow directory listings in HTTP server
  - #130: Prevent unnecessary re-running of Shake action by debouncing fsnotify events
  - #136: Move `.shake` database directory under `ribInputDir`
  - default.nix: Takes `overrides` and `additional-packages` as extra arguments

## 0.7.0.0

- Dependency upgrades
  - mmark: 0.0.7.2
  - megaparsec: 0.8
  - clay: 0.14
  - shake: 0.8.15
- New features:
  - Added Dhall parser, `Rib.Parser.Dhall`
  - Add `Rib.Extra` containing useful but non-essential features
- MMark, extensions removed:
  - `ghcSyntaxHighlighter`: we already have `skylighting` (which supports more parsers than Haskell)
  - `obfuscateEmail`: requires JS, which is not documented.
- API changes:
  - Introduced `Route` functionality for simpler management of static routes.
    - Removed `buildHtmlMulti`, `buildHtml`, `readSource` functions and `Source` type.
  - Introduced `Rib.Shake.forEvery` to run a Shake action over a pattern of files when they change.
  - Exposed `Rib.Shake.writeFileCached`
  - `MMark.parse` and `Pandoc.parse` now automatically append path to `ribInputDir` and do not return Either.
  - Added `MMark.parseWith` (and `parsePureWith`), to specify a custom list of mmark extensions
- Bug fixes
  - #95: Fix Shake error `resource busy (file is locked)`
  - #97: Fix Shake error `AsyncCancelled` when server thread crashes
  - #96 & #108: Drop problematic use of Shake `cacheActionWith`

## 0.6.0.0

- Advance nixpkgs; require Shake >=0.18.4
- Major API simplication: no more type class!
  - Allow user to specify their own source parser as a Haskell function
  - Removed types `Document` and `Markup` in favour of `Source`
  - Expose `ribInputDir` and `ribOutputDir` for use in custom Shake actions
- Bug fixes:
  - #63: create intermediate directories when generating post HTML
  - #70: Don't crash on Shake errors
  - Fix unnecessary rebuild of all files when only one file changed
    - #66: Use caching (via Shake's `cacheActionWith`), to avoid writing HTML to disk until it has changed.

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
