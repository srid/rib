# rib

<!--
Credit for this image: https://www.svgrepo.com/svg/24439/ribs
-->
<img src="https://raw.githubusercontent.com/srid/rib/master/examples/doc/a/static/ribs.svg?sanitize=true" width="150" />

Rib is a static site generator written in Haskell that reuses existing tools
(`Shake`, `Lucid` and `Clay`) and is thus non-monolithic. It is nearly done but
still a work in progress and will soon be ready for general use.

## Example

See `./examples/hello-world` to see how the `Rib` library can be used to write your own
static site generator in a few lines of code which includes the HTML and CSS of
the site.

See `./examples/doc` for a real-world example---Rib's own documentation site.

(Refer to `Rib.Simple` if you need further customization of the Shake action.)

With Rib you do not have to deal with less powerful template engines or
write raw HTML/CSS by hand. Do everything in Haskell, and concisely at that!

---

To get an example site up and running run:

```bash
cd ./examples/hello-world
nix-shell --run 'ghcid -T main'
```

This will:

- Drop into a nix-shell with needed Haskell dependencies (including `rib`)
- Compile `Main.hs` through ghcid
  - Whenever `Main.hs` changes ghcid reloads them
- Run `Main.hs:main`
- This does the following:
  1. Convert sources in `./a` into `./b` using Shake
  2. Listens for changes to `./a`, and re-generate them
  3. Start a HTTP server serving the `./b` directory

Thus, by running that one command one gets a production-quality web server
serving the statically generated HTML files which automatically get regenerated
when the source content changes. What's more, we may change the Haskell sources
such as `Main.hs` and ghcid will recompile and relaunch the whole thing.

With `rib` we get hot reload for free.
