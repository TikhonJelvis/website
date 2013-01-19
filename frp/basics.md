---
title: FRP Intro
author: Tikhon Jelvis
---

<div class="content">

# Programming UIs

Writing responsive, interactive UIs with modern tools and frameworks is relatively tricky. Most frameworks present an event-oriented interface: UI logic is written using callbacks that respond to events coupled with mutable models of both the "business logic" and the actual onscreen controls. Such code ends up non-linear and hard to follow. Everything being written with callbacks naturally obscures the underlying meaning and flow of the code. Ultimately, these UI frameworks are at too low a level: implementation details dominate the actual meaning of the code.

Implementing UI logic would be much simpler---and correspondingly more productive---with a more declarative approach: a model abstracting away the *how* in favor of the *what*. The actual code should reflect its high-level meaning instead of the relatively complex control flow actually required to couple it with GUI elements.

Another problem is that the standard method is staunchly imperative. It fits well into imperative, object-oriented languages but feels rather jarring in more functional settings. A declarative approach would work well with functional code.

## A Game of Life

To help illustrate these concepts, we're going to write a simple program with FRP. I will cover exactly how to write the *core* part of the application---enough to make it usable--and leave some additional features as exercises. 

We will implement a simple version of [John Conway's Game of Life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life). If you are not familiar with this "game", be sure to read about it: it's a fascinating subject all on its own and the favorite of hackers everywhere. In fact, the informal but rather popular ["hacker emblem"](http://www.catb.org/hacker-emblem/), which you may have seen in other contexts, comes from the Game of Life.

Since this article is entirely about the UI, I have implemented the rules of the game of life as [Game.hs](Game.hs). This implementation is based on some [slide](http://illustratedhaskell.org/index.php/2011/09/24/conways-game-of-life-with-repa/) available online; if you are interested, it is certainly a short and healthy Haskell exercise to implement this yourself.

If you just want the code, you can find it on [GitHub](https://github.com/TikhonJelvis/Reactive-Life). The default "minimalist" branch only has the core UI while the "master" branch has a bunch of additional features.

</div>

<div class="content">

# Setting Forth

<!-- Maybe switch everything to wxWidgets 2.9? That sounds like a good idea... -->

We are going to be using a Haskell library called Reactive Banana. The first step is to install the [Haskell Platform](http://www.haskell.org/platform/) if you don't have it already. After this, you will need to install Reactive Banana:

    cabal install reactive-banana
    
Assuming that worked, you will also need the reactive-banana-wx package which provides bindings for the FRP library to wxWidgets, a cross-platform GUI toolkit. Before building this, make sure you have the development version of wxWidgets installed---this is a package like `wxGTK-dev`.
