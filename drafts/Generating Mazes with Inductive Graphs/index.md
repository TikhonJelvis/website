---
title: Generating Mazes with Inductive Graphs
author: Tikhon Jelvis
---

A few years ago---back in high school---I spent a little while writing programs to automatically generate mazes. It was a fun exercise and helped me come to grips with recursion: the first time I implemented it (in Java), I couldn't get the recursive version to work properly so ended up using a `while` loop with an explicit stack! I later wrote a [version in JavaScript](/maze) too; you can play around with it and read the code on [GitHub](https://github.com/TikhonJelvis/maze).

Making random mazes is actually a really good programming exercise: it's relatively simple, produces cool pictures and does a good job of covering graph algorithms. It's especially interesting for functional programming because it relies on **graphs** and **randomness**, two things generally viewed as tricky in a functional style.

So lets look at how to implement a maze generator in Haskell using **monad transformers** for randomness and **inductive graphs** for our graph traversal. Here's what we're aiming for:

![A simple maze built from a grid.](simple-maze.png)

## The Algorithm

We can generate a **perfect** maze by starting with a graph representing a grid and generating a random **spanning tree**. A perfect maze has exactly one path between any two points---no cycles or walled-off areas. A spanning tree of a graph is a tree that connects every single node in the graph.

There are multiple algorithms we can use to generate such a tree. Let's focus on the simplest one which is just a randomized depth first search (DFS):

  1. start with a grid that has every possible wall filled in
  2. choose a cell to begin
  3. from your current cell, choose a random neighbor *that you haven't visited yet*
  4. move to the chosen neighbor, knocking down the wall between it
  5. if there are no unvisited neighbors, backtrack to the previous cell you were in and repeat
  6. otherwise, repeat from your new cell

<div class="figure">
  <img src="grid.png" alt="A grid with all the walls enabled." />
  <img src="small-maze.png" alt="A maze built by deleting walls in the grid." />
  <p> Our algorithm starts with a grid and produces a maze by deleting walls. </p>
</div>

## Inductive Data Types

To write our DFS, we need some way to represent a graph. Unfortunately, graphs are often a bit awkward in functional languages: standard representations like adjacency matrices or adjacency lists are designed for imperative computations. While you can certainly use them in Haskell, the resulting code would be relatively awkward.

But what sorts of structures does Haskell handle really well? Trees and lists come to mind: we can write very natural code by pattern matching. A very common pattern is to inspect a list as a head element and a tail, recursing on the tail:

    foo (x:xs) = bar x : foo xs

exactly the same pattern is useful for trees where we recurse on the children of a node:

    foo (Node x children) = Node (bar x) (map foo children)

This pattern of choosing a "first" element and then recursing on the "rest" of the structure is extremely powerful.

We can decompose lists and trees like this very naturally because that's exactly how they're constructed in the first place: pattern matching is just the inverse of using the constructor normally. Even the syntax is the same! The pattern `(x:xs)` decomposes the result of the expression `(x:xs)`. And since we're just following the inherent structure of the type, there is always exactly one way to break the type apart.

These sorts of types are called **inductive data types** by analogy to mathematical induction. Generally, they have two branches: a base case and a recursive case---just like a proof by induction! Consider a `List` type:

    data List a = Empty           -- base case
                | Cons a (List a) -- recursive case

Pretty straightforward. Unfortunately, graphs don't have this same structure. We can't break a graph down into pieces in any canonical sort of way, which means we can't pattern match on a graph, which means our code is awkward.

## Inductive Graphs

**Inductive graphs** are a way to represent graphs so that we can *view* them as if they were inductive. We can split a graph up and recurse over it, but this isn't a structural operation like it would be for lists and, more importantly, it is *not* canonical: at any point, many different graph decompositions can all make sense.

The core idea is that we can always view a graph as either *empty* or as some node with its edges and the rest of the graph. A node together with its incoming and outgoing edges is called a *context*; the idea is that we can split a graph up into a context and everything else, just like we can split a list up into its head element and everything else.

Conceptually, this is as if we defined a graph using two constructors, `Empty` and `:&` (in infix syntax):

    data Graph = Empty
               | (Context [Edge] Node [Edge]) :& Graph

The actual type also supports both node and edge labels, which I've left out for simplicity.

The problem with using this definition directly is that equivalent graphs could be built up in different ways---the order we attach contexts does not matter. So instead the actual graph type is *abstract*, and we can just *view* it using contexts like above.

In particular, instead of pattern matching directly on a graph, we need to use a function that takes a graph and returns a context decomposition like above (or fails on an empty graph). One problem is that there is no "natural" first node to return---a graph could be built up in any order, after all. This is why the simplest matching function `matchAny` returns an *arbitrary* (implementation defined) node decomposition:

    matchAny :: Graph -> (Context, Graph)

With `ViewPatterns`, we can actually use this function directly inside a pattern for nice syntax. Here's the moral equivalent of `head` for graphs:

    $ghead$

We can also direct our graph traversal by trying to match against a *specific* node with the `match` function:

    match :: Node -> Graph -> (Maybe Context, Graph)

If the node is not in the graph, we get a `Nothing` for our context. This function can also be used as a view pattern:

    foo (match node -> (Just context, graph)) = ...

This makes for some pretty slick graph-manipulating code!

## FGL
