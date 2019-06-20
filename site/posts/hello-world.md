---
title: "Hello World"
author: Sridhar Ratnakumar
date: Jul 19, 2017
tags: [haskell, programming]
description: How to write a Hello World blog post
image: battleship.jpg
---

Functions don't evaluate their arguments. This means that programs can compose
together very well, with the ability to write control constructs (such as
if/else) just by writing normal functions. The purity of Haskell code makes it
easy to fuse chains of functions together, allowing for performance benefits.

```haskell
when p m = if p then m else return ()
main = do args <- getArgs
          when (null args)
               (putStrLn "No args specified!") 
```
