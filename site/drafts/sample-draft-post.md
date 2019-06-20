---
title: "Flux Monoid: Efficient change counting"
author: John
date: Jun 20, 2018
tags: [programming, haskell]
description: Description and implementation of a Monoid to effeciently count element equality changes across data structures
image: flux-monoid/flux.jpg
---

This is going to be a post about monoids, finger trees, efficiently splitting up text, and the unending change
of the world that surrounds us; but mostly the monoid bit.

# Monoids

If you're entirely unfamiliar with the concept of monoids, or just need a
refresher, it would be a good idea to get a solid grounding there first;
[here's a good place to
start](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour).
