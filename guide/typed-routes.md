---
tags:
    - guide
---

# Typed Routes

Rib includes an optional route system based on
[GADT](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html)s than
can be used to define safe and structured routes for your site. The sample repo
used in [[tutorial]] already uses routes, and you can see the entire code in
[[preview]] to see all of this would fit together in a static site
generator.


## Definining Routes

To define a route, create a GADT type called `Route` with a type parameter `a`:


```haskell
data Route a where
  Route_Home :: Route ()
  Route_Article :: FilePath -> Route Pandoc
```

Why use GADTs instead of regular ADTs? There are two reasons:

1. GADTs provides us with a mechanism to specify an unique type (the type
   parameter `a`) for *each* of its constructor. This type, `a`, is generally
   the type of the value used to *generate* the static content (typically HTML)
   of that route.

2. The value used to generate a route is not necessary to create the route

For example, this is how we create the article route (typically inside the
[`forEvery`](http://hackage.haskell.org/package/rib-0.8.0.0/docs/Rib-Shake.html#v:forEvery) block):

```haskell
let r = Route_Article "hello.md"
```

Note that we are able to create a route without the value used to generate it,
i.e., the Markdown content (point 2). This route is of type `Route Pandoc`,
which means `a ~ Pandoc`. Therefore, wherever we pass this route - when we pass
the value associated with it, it must be of the type `Pandoc` (point 1). The
`renderPage` function which renders the HTML of a route is defined to take both
the route and its value as an argument:


```haskell
renderPage :: Route a -> a -> Html ()
...
```

The `renderPage` function is thus polymorphic in the route it handles. It can
render a common HTML layout, and `case` on the route value passed to render
content to specific to routes. The author finds this way of rendering HTML to be
much more ergonomic and pleasant compared to the various templating systems of
other static site generators.

## Customizing route paths

Once your route type is defined, you may specify the file paths for each of
them. To do this, simply derive an instance for the `IsRoute` class:

```haskell
instance IsRoute Route where
  routeFile = \case
    Route_Home ->
      pure "index.html"
    Route_Article srcPath ->
      pure $ "article" </> srcPath -<.> ".html"
```

Notice how in order to calculate the file path of a route, you only need the route
(`Route a`), but not the data (`a`) used to generate it. 

The arguments your route constructors take are meant to be used in the
calculation of this file path. For example, the `FilePath` argument of the
`Route_Article` constructor specifies the source path of the Markdown document,
from which we compute the target (generated) path by simply replacing the
extension with ".html" (in addition to putting it in a sub directory "article").

## Generating a route

`Rib.Route.writeRoute` takes a route, a string and writes that string to the
file path associated with the route. You will use this in your Shake `Action`
monad, typically inside a `forEvery` block.

## Route URLs

Once routes are fully defined as above, it becomes very straightforward to use
them when linking in your HTML. The `Rib.routeUrl` function takes a route
(`Route a`) and returns the URL to that route. 

You will pass your routes to whichever function (`renderPage` is principle among
them) that needs to know the URL to your generated files, as long as they remain
polymorphic in the route type.

## See also

- [obelisk-route](https://old.reddit.com/r/haskell/comments/fsgqd6/monthly_hask_anything_april_2020/fm6esky/?context=3):
  the library that inspired `Rib.Route`.
