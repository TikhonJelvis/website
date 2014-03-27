---
title: Generating Mazes with Inductive Graphs
author: Tikhon Jelvis
---

A few years ago---back in high school---I spent a little while writing programs to automatically generate mazes. It was a fun exercise and helped me come to grips with recursion: the first time I implemented it (in Java), I couldn't get the recursive version to work properly so ended up using a `while` loop with an explicit stack! I later wrote a [version in JavaScript](/maze) too; you can play around with it and read the code on [GitHub](https://github.com/TikhonJelvis/maze).

Making random mazes is actually a really good programming exercise: it's relatively simple, produces cool pictures and does a good job of covering graph algorithms. It's especially interesting for functional programming because it relies on **graphs** and **randomness**, two things generally viewed as tricky in a functional style.

So lets look at how to implement a maze generator in Haskell using **monad transformers** for randomness and **inductive graphs** for our graph traversal. 

## The Algorithm

We can generate a **perfect** maze by starting with a graph representing a grid and generating a random **spanning tree**. A perfect maze has exactly one path between any two points---no cycles or walled-off areas. A spanning tree of a graph is a tree that connects every single node in the graph.

There are multiple algorithms we can use to generate such a tree. Let's focus on the simplest one which is just a randomized depth first search (DFS). 