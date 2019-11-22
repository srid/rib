# Change Log for rib

## 0.5.0.0 (UNRELEASED)

This release comes with major API refactor. Key changes:

- Support for both Pandoc and MMark parsers
- Add `Rib.Markup.Markup` type class to polymorphically select the parser
- Add top-level `Rib` import namespace to ease of use
- Drop support for JSON cache
- Replace complex metadata handling using straightforward aeson `Value` parsing

## 0.4.1.0

- `Rib.Pandoc`: 
  - Export `render'` and `renderInlines'` (the non-Lucid versions)
  - Re-export `Text.Pandoc.Readers` so the library user does not have to directly depend on `pandoc` only to render its documents.
- `Rib.App`: The `run` funtion now takes two more arguments, specifying the input and output directory, which are no longer hardcoded.
- `Rib.Simple`: add LaTeX to default list of readers
- `Rib.Server`: Remove ".html" detection magic from URLs
- Remove `Rib.Simple`

## 0.3.0.0

- Rename `Rib.App.Watch` to `Rib.App.WatchAndGenerate`

## 0.2.0.0

- Initial release.

