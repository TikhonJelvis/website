---
title: Tree Edit Distance
author: Tikhon Jelvis
---

I recently got back to an old project of mine which requires me to compute diffs between trees efficiently, a problem generally called **tree edit distance**. This algorithm is a great example of how to do **dynamic programming** in Haskell, and a good case study of implementing nontrivial algorithms in a functional style.

The final goal is to take two trees and produce a summary of the *differences* between them: what nodes do we need to add, delete or modify to go from one to the other? This is a more general version of a common dynamic programming problem called **string edit distance**, so I'm going to start by implementing that and then changing the code to work for trees.

<!-- add tree diff image here -->

<!--more-->

## Semantic Version Control

A few years ago, I started working on a project for **semantic version control**. The idea is to compare code *as* code, not as plain text. We would do the diffs and merges directly on syntax trees, enabling a higher level of analysis.

The first version was hacked together in 18 hours at a hackathon and did not work very well. Later, [Ankur Dave][ankur] and I rewrote the whole system for a class project. We named it [cow][cow] because, well, we're bad with names.  While that version worked properly, it did not scale beyond short code snippets: I implemented the core algorithm for diffing trees incorrectly. After that, I got stuck and the project went on hiatus for a couple of years.

I've been meaning to get back to it for a while and implement the algorithm correctly, but just haven't gotten around to it. I was recently contacted by [Joe Nelson][joe] who is going on an "open source pilgrimage" and wanted to pair with me on a project. This was the push I needed to get back to the project, and I spent a [great day][pairing] working with Joe on it.

Right now, I'm just going to talk about the tree edit distance algorithm I need for the project; I'll cover other parts of the whole system later.

[ankur]: http://ankurdave.com
[cow]: /cow
[joe]: http://begriffs.com/
[pairing]: http://blog.begriffs.com/2014/04/pilgrimage-report-structural-merging.html

</div>
<div class="content">

# Dynamic Programming and Memoization

**Dynamic programming** is one of the core techniques for writing efficient algorithms. The idea is to break a problem into *smaller subproblems* and then save the result of each subproblem so that it is only ever calculated once. Dynamic programming involves two parts: first, we have to restate the problem in terms of **overlapping subproblems**, then we memoize calculating those subproblems.

Memoization in general is a rich topic in Haskell. There are some very interesting approaches for memoizing functions over different sorts of inputs like Conal Elliott's [elegant memoization][elegant] or Luke Palmer's [memo combinators][combinators].

The general idea is to take advantage of laziness and create a large data structure like a list or a tree that stores the results of the function. This data structure is defined circularly: recursive calls are replaced with references to other parts of the data structure. Pieces of the data structure only get evaluated as need and at most once---the actual memoization emerges naturally from the evaluation rules. A very illustrative example (but slightly cliché) example is the memoized version of the Fibonacci function:

```haskell
fib n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)
```

The `fib` function just indexes into the `fibs` list. This list is defined *in terms of itself*: instead of recursively calling `fib`, we just make newer elements of `fibs` depends on older ones by passing `fibs` and `(drop 1 fibs)` into `zipWith (+)`. It helps to visualize what this list looks like as more and more elements get evaluated:

<div id="fibs-animation" class="figure">
<ul class="animation">
  <li> <img src="fib-frames/frame0.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames/frame1.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames/frame2.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames/frame3.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames/frame4.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames/frame5.png" alt="The fibs list in memory." />
  </li>
</ul>
  <style type="text/css">
  #fibs-animation ul li {
    text-align : left;
  }
  </style>
  <script type="text/javascript">
    animate("#fibs-animation");
  </script>
</div>

However, memoization in dynamic programming algorithms always tends to have the same structure---you create an array and the inputs to the algorithm are turned into indices into the array. Since we wouldn't need most of the structure of the more general memoization approaches, we're just going to look at writing dynamic programming problems out in terms of **lazy arrays**.

Dynamic programming algorithms are often presented in a distinctly imperative style: you initialize a large array with some value and then manually update it as you go along. You have to do some explicit bookkeeping at each step to save your result and there is nothing preventing you from accidentally reading in part of the array you haven't written to yet. Moreover, this style is also quite awkward to replicate directly in Haskell: you'd have to either use a mutable array or thread an immutable array through as an argument. The first version ends up pretty ugly in Haskell and the second version---not so great itself---is not very efficient.

Instead of replicating the imperative approach directly, we're going to take advantage of Haskell's laziness to define an array *that depends on itself*. This way, the logic of calculating each value once and then caching it is handled behind the scenes by Haskell's evaluation strategy. We compute the subproblems at most once in the order that we need but the array is always used *as if* it was fully filled out: we can never accidentally forget to save a result or access the array before that result has been calculated.

[elegant]: http://conal.net/blog/posts/elegant-memoization-with-functional-memo-tries
[combinators]: http://lukepalmer.wordpress.com/2008/10/14/data-memocombinators/