---
title: Generating Mazes with Inductive Graphs
author: Tikhon Jelvis
---

A few years ago---back in high school---I spent a little while writing programs to automatically generate mazes. It was a fun exercise and helped me come to grips with recursion: the first time I implemented it (in Java), I couldn't get the recursive version to work properly so ended up using a `while` loop with an explicit stack! I later wrote a [version in JavaScript](/maze) too; you can play around with it and read the code on [GitHub](https://github.com/TikhonJelvis/maze).

Making random mazes is actually a really good programming exercise: it's relatively simple, produces cool pictures and does a good job of covering graph algorithms. It's especially interesting for functional programming because it relies on **graphs** and **randomness**, two things generally viewed as tricky in a functional style.

So lets look at how to implement a maze generator in Haskell using **inductive graphs** for our graph traversal. Inductive graphs are provided by Haskell's "Functional Graph Library" [`fgl`][fgl].

[fgl]: http://hackage.haskell.org/package/fgl

Here's what we're aiming for:

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

</div>
<div class="content">

# Inductive Data Types

To write our DFS, we need some way to represent a graph. Unfortunately, graphs are often inconvenient functional languages: standard representations like adjacency matrices or adjacency lists were designed with an imperative mindset. While you can certainly use them in Haskell, the resulting code would be relatively awkward.

But what sorts of structures does Haskell handle really well? Trees and lists come to mind: we can write very natural code by pattern matching. A very common pattern is to inspect a list as a head element and a tail, recursing on the tail:

```haskell
foo (x:xs) = bar x : foo xs
```

Exactly the same pattern is useful for trees where we recurse on the children of a node:

```haskell
foo (Node x children) = Node (bar x) (map foo children)
```

This pattern of choosing a "first" element and then recursing on the "rest" of the structure is extremely powerful. I like to think of it as "layering" your computation on the "shape" of the data structure.

We can decompose lists and trees like this very naturally because that's exactly how they're constructed in the first place: pattern matching is just the inverse of using the constructor normally. Even the syntax is the same! The pattern `(x:xs)` decomposes the result of the expression `(x:xs)`. And since we're just following the inherent structure of the type, there is always exactly one way to break the type apart.

These sorts of types are called **inductive data types** by analogy to mathematical induction. Generally, they have two branches: a base case and a recursive case---just like a proof by induction! Consider a `List` type:

```haskell
data List a = Empty           -- base case
            | Cons a (List a) -- inductive case
```

Pretty straightforward.

Unfortunately, graphs don't have this same structure. A graph is defined by its set of nodes and edges---the nodes and edges do not have any particular order. We can't build a graph in a unique way by adding on nodes and edges because any given graph could be built up in multiple different ways. And so we can't break the graph up in a unique way. We can't pattern match on the graph. Our code is awkward.

## Inductive Graphs

**Inductive graphs** are graphs that we can *view* as if they were a normal inductive data type. We can split a graph up and recurse over it, but this isn't a structural operation like it would be for lists and, more importantly, it is *not* canonical: at any point, many different graph decompositions might make sense.

We can always view a graph as either *empty* or as some node, its edges and the rest of the graph. A node together with its incoming and outgoing edges is called a *context*; the idea is that we can split a graph up into a context and everything else, just like we can split a list up into its head element and everything else.

Conceptually, this is as if we defined a graph using two constructors, `Empty` and `:&` (in infix syntax):

```haskell
data Graph = Empty
           | (Context [Edge] Node [Edge]) :& Graph
```

The [actual graph view][graph-views] in `fgl` is a bit different and supports node and edge labels, which I've left out for simplicity. The fundamental ideas are the same.

Consider a small example graph:

[graph-views]: http://hackage.haskell.org/package/fgl-5.4.2.4/docs/Data-Graph-Inductive-Graph.html#g:3

![Just a random graph.](example.png)

We could decompose this graph into the node `1`, its context and the rest of the graph:

![`(Context [4,5,6] 1 []) :& graph` our graph decomposed into the node `1`, its edges to `4`, `5` and `6` as well as the rest of the graph.](match1.png)

We can't use this definition directly as that equivalent graphs could be built up in different ways---the order we attach contexts does not matter. For example, we could just as easily decompose the same example graph into node `2` and the rest:

![`(Context [5,6,7] 2 []) :& graph` another equally valid way to decompose the same graph.](match2.png)

So instead the actual graph type is *abstract*, and we can just *view* it using contexts like above. Unlike normal pattern matching, viewing an abstract type is *not* necessarily the inverse of constructing it.

We accomplish this by using a matching function that takes a graph and returns a context decomposition like above. Since there is no "natural" first node to return, the simplest matching function `matchAny` returns an *arbitrary* (implementation defined) decomposition:
                                    
```haskell
matchAny :: Graph -> (Context, Graph)
```

In `fgl`, `Context` is just a tuple with four elements: incoming edges, the node, the node label and outgoing edges. This is what functions like `matchAny` return.

With [`ViewPatterns`][views] we can actually use `matchAny` directly inside a pattern with nice syntax. Here's the moral equivalent of `head` for graphs:

[views]: https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/pattern-and-guard-extensions#viewpatterns

```haskell
ghead :: Graph -> Node
ghead graph | Graph.isEmpty graph             = error "Empty graph!"
ghead (matchAny -> ((_, node, _, _,), graph)) = node
```

Of course, it's different from normal `head` in that the exact node it returns is arbitrary and implementation defined. To overcome this, we can direct our graph traversal by trying to match against a *specific* node with the `match` function:

```haskell
match :: Node -> Graph -> (Maybe Context, Graph)
```

If the node is not in the graph, we get a `Nothing` for our context. This function can also be used as a view pattern:

```haskell
foo (match node -> (Just context, graph)) = ...
```

This makes it easy to do *directed* traversals of the graph: we can "travel" to the node of our choice. 

</div>
<div class="content">

# A Real Example

All functions in `fgl` are actually specified against a [`Graph`][graph-class] typeclasses rather than a concrete implementation. This typeclass mechanism is great since it allows multiple implementations of inductive graphs. Unfortunately, it also breaks type inference in ways that are sometimes hard to track down so, for simplicity, we'll just the implementation type provided: `Gr`. `Gr n e` is a graph that has nodes labeled with `n` and edges labeled with `e`.

[graph-class]: http://hackage.haskell.org/package/fgl-5.4.2.4/docs/Data-Graph-Inductive-Graph.html#t:Graph

## Map

The "Hello, World!" of recursive list functions is `map`, so lets start by looking at a version of `map` for graphs. The idea is to apply a function to every node label in the graph.

For reference, here's list `map`:

```haskell
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
```

First, we have a base case for the empty list. Then we decompose the list, apply the function to the single element and recurse on the remainder.

The map function for graph nodes looks very similar:

```haskell
mapNodes :: (n -> n') -> Gr n e -> Gr n' e
mapNodes _ g | Graph.isEmpty g = Graph.empty
mapNodes f (matchAny -> ((in', node, label, out), g)) =
  (in', node, f label, out) & mapNodes f g
```

The base case is almost exactly the same. For the recursive case, we use `matchAny` to decompose the graph into *some* node and the rest of the graph. For the node, we actually get a `Context` which contains the incoming edges (`in'`), outgoing edges (`out`), the node itself (`node`) as well as the label (`label`). We just want to apply a function to the label, so we pass the rest of the context through unchanged. Finally, we recombine the graph with the `&` function, which is the graph equivalent of `:` for lists. (Although note that it is *not* a constructor but just a function!)

Since we used `matchAny`, the exact order we map over the graph is not defined! Apart from that, the code feels very similar to programming against normal Haskell data types and characterizes `fgl` in general pretty well.

## DFS

Our maze algorithm is going to be a randomized depth-first search. We can first write a simple, non-random DFS and then go from that to our maze algorithm. This is actually going to be pretty similar to `mapNodes` except that we're going to build up a list of visited nodes as we go along instead of building a new graph. It is also a *directed* traversal unlike the undirected map.

The first version of our DFS will take a graph and traverse it starting from an arbitrary node, returning a list of the nodes visited in order. Here's the code:

```haskell
dfs :: Gr a b -> [Node]
dfs (matchAny -> ((_, start, _, _), graph) = go [start] graph
  where go [] _                            = []
        go _ g | Graph.isEmpty g           = []
        go (n:ns) (match v -> (Just c, g)) = n : go (suc' c ++ ns) g
        go (_:ns)                          = go ns g
```

All of the interesting logic is in the helper `go` function. It takes two arguments: a list, which is the stack of nodes to visit and the remainder of the graph.

The first two lines of `go` are the base cases. If we either don't have any more nodes on the stack or we've run out of nodes in the graph, we're done.

The recursive case is more interesting. First, we get the node we want to visit (`n`) from our stack. Then, we use that to *direct* our match with the `match n` function. If this succeeds, we add `n` to our result list and push every successor node of `n` to the stack. If the match failed, it means we visited that node already, so we just ignore it and recurse on the rest of the stack.

We find the successors of a node using the `suc'` function which returns the successor nodes of a particular context. In `fgl`, functions named with a `'` typically act on contexts like this.

The important idea here is that we don't need to explicitly keep track of which nodes we've visited since, after we visit a node for the first time, we always recurse on the remainder of the graph that does not contain it. This is common to a whole bunch of different graph algorithms, which makes this a very useful pattern.

## EDFS

`dfs` gives us a list of nodes in the order that they were visited. But for mazes, we really care about the *edges* we followed rather than just the nodes. So lets modify our `dfs` into an `edfs` which returns a list of edges rather than a list of nodes. In `fgl`, an edge is just a tuple of two nodes: `(Node, Node)`.

The modifications from our original `dfs` are actually quite slight: we keep a stack of edges instead of a stack of nodes. This requires modifying our starting condition:

```haskell
edfs (matchAny -> ((_, start, _, _), graph)) =
          drop 1 $$ go [(start, start)] graph
```

I cheated a bit here: to make the starting condition nicer, I push an extra edge onto the stack and then discard it with the `drop 1`. The other change was for the recursive case, where we push edges onto the stack instead of nodes:

```haskell
go ((p, n):ns) (match n -> (Just c, g)) =
          (p, n) : go (map (n,) (suc' c) ++ ns) g
```

We still get the successor nodes, but now we turn them into an edge from the current node. The `map (n,)` syntax takes advantage of the [`TupleSections`][tuple-sections] extension.

[tuple-sections]: http://www.haskell.org/ghc/docs/7.0.3/html/users_guide/syntax-extns.html#tuple-sections

## Randomness

The final change to our `dfs` code to generate a maze is to add randomness. All we really want to do is shuffle the list of successors before putting it on the stack. We're going to use the `MonadRandom` class, which is compatible with a bunch of other monads like `IO`. I used a naïve O(n²) shuffle:

```haskell
shuffle :: MonadRandom m => [a] -> m [a]
```

Given this, we just need to modify `edfs` to use it which requires lifting everything into the monad:

```haskell
edfsR :: MonadRandom m => Gr n e -> m [(Node, Node)]
edfsR (matchAny -> ((_, start, _, _), graph)) =
      liftM (drop 1) $$ go [(start, start)] graph
  where go [] _                                 = return []
        go _ g | Graph.isEmpty g                = return []
        go ((p, n):ns) (match n -> (Just c, g)) = do
          edges <- shuffle $$ map (n,) (suc' c)
          liftM ((p, n) :) $$ go (edges ++ ns) g
        go (_:ns) g                             = go ns g
```

The differences are largely simple and very type-directed: you have to add some calls to `return` and `liftM`, but you get some nice type errors that tell you *where* you need them. The only other change is using `shuffle`, which is straightforward with do-notation.

For something that's supposed to be awkward in functional programming, I think the code is actually pretty neat and easy to follow!

</div>
<div class="content">

# Mazes

Right now, we have a random DFS that gives us a list of edges. This is basically the whole maze generation algorithm in a nutshell. However, it's difficult to go from a set of edges to drawing a maze. The final piece of the puzzle is labeling the edges in a way that's convenient to draw as well as generating the graph for the initial grid.

This is the first place where we're going to use edge labels. Each edge represents a wall and we want enough information to draw the wall. To do this we need to know the walls *location* and its *orientation* (either horizontal and vertical). For simplicity, we will walls by the location of the cell either below or to the right of the wall. Here's our label type:

```haskell
data Orientation = Horizontal | Vertical deriving (Show, Eq)

data Wall = Wall (Int, Int) Orientation deriving (Show, Eq)

type Maze = Gr () Wall -- () means no node labels needed
```

Now we just need to make a graph representing the grid we start with. After that, we just pass it into our `edfsR` and get a list of walls *not* to draw. We just draw the walls for all the other edges an we're set!

We can assemble the graph with the `mkGraph` function which takes a list of nodes and a list of edges. Remember that we want to label each edge with its location and orientation. There's likely a better way to do it, but for now I take advantage of the fact that `Node` is just an alias for `Int`:

```haskell
grid width height = Graph.mkGraph nodes edges
  where nodes = [(node, ()) | node <- [0..width * height - 1]]
        edges = [(n, n', wall n Vertical) |
                 (n, _) <- nodes,
                 (n, _) <- nodes,
                 n - n' == 1 && n `mod` width /= 0 ]
             ++ [(n, n', wall n Horizontal) |
                 (n,_) <- nodes,
                 (n',_) <- nodes,
                 n - n' == width ]
        wall n = let (y, x) = n `divMod` width in Wall (x, y)
```

