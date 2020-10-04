---
title: Enable Syntax Highlighting
tags:
    - guide
---

Use Pandoc to add syntax highlighting support to your rib site.

1. Import the desired style from Pandoc

    ```haskell
    import Text.Pandoc.Highlighting (styleToCss, tango)
    ```

2. Add it to the `<head>` section of your HTML:

    ```haskell
    style_ [type_ "text/css"] $ styleToCss tango
    ```

3. Make sure that your Markdown files specify the language in their fenced code
   blocks. See [Github
   documentation](https://help.github.com/en/github/writing-on-github/creating-and-highlighting-code-blocks#syntax-highlighting)
   for details.
