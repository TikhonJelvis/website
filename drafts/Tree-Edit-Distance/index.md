---
title: Tree Edit Distance
author: Tikhon Jelvis
---

# Trees

Now that we have a framework for thinking about dynamic programming as well as a nice example, let's extend these ideas to a slightly more complex problem---tree edit distance.

I'm going to be using the [Zhang-Shasha][zs-algorithm] algorithm, a generalization of the string edit distance algorithm described previously. In fact, the string algorithm emerges as a special case of Zhang-Shasha applied to a tree with no branching. We can think of this algorithm as a version of the string edit distance algorithm which additionally keeps track of the tree structure (ie parent and children nodes) explicitly instead of assuming a purely linear configuration.

## Edit Actions

We still consider the same three edit actions (add, remove, modify), extended to trees.

Modifying doesn't change the structure of the tree, it just relabels a node:

![Modifying node \(x\) to \(y\).](tree-modify.png)

Removing a node moves all of its children up a level. Here, removing \(x\) moves its children up to \(x\)'s old parent \(1\).

![Removing node \(x\).](tree-remove.png)

I find adding nodes hard to think about. Adding a node moves its children down---but how do we know which nodes are the children? It's easier to realize that adding is just the mirror image of removing a node, just like in the string case.

So adding is really just removing a node from the target tree instead of the source tree.

![Adding node \(x\).](tree-add.png)

## Basic Trees

For string edit distance, we took our inputs as lists. For trees, we're going to use "rose trees" which have a value at each node and any number of branches. This type is provided by `Data.Tree` but, for simplicity, I'm going to redefine it here:

```haskell
data Tree a = Node a (Forest a)

type Forest a = [Tree a]
```

At each node, we have a value and a list of sub-trees. For this algorithm, having a separate `Forest` type turns out to be particularly useful because much of the code works with forests rather than single trees.

Since this is a dynamic programming problem, we're going to use integers to index into sub-problems. For the linear string case, these integers could just be the position in the list. But what do we do for trees?

We can traverse the tree in some set order and index into nodes based on their position in this traversal. For this algorithm, a post-order traversal turns out to be particularly useful. In a post order-order traversal, we first traverse all the sub-trees in order and then finally visit the root:

```haskell
postOrder :: Tree a -> Tree (Int, a)
postOrder node = evalState (go node) 0
  where go (Node v cs) = do
          cs' <- mapM go cs
          n   <- get <* modify (+ 1)
          return $$ Node (n, v) cs'
```

I think this is also a great example of using [`State`][State] to make the bookkeeping simpler. While the actual `State` type is a bit complicated---largely because it's actually defined using a monad transformer---we don't have to worry about any of those details at all. We just use [`evalState`][evalState] and let type inference handle everything.

[zs-algorithm]: http://grantjenks.com/wiki/_media/ideas/simple_fast_algorithms_for_the_editing_distance_between_tree_and_related_problems.pdf
[State]: https://hackage.haskell.org/package/mtl-2.0.1.0/docs/Control-Monad-State-Lazy.html#t:State
[evalState]: https://hackage.haskell.org/package/mtl-2.0.1.0/docs/Control-Monad-State-Lazy.html#v:evalState