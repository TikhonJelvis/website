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

The general idea is to take advantage of laziness and create a large data structure like a list or a tree that stores the results of the function. This data structure is defined circularly: recursive calls are replaced with references to other parts of the data structure. Pieces of the data structure only get evaluated as needed and at most once---the actual memoization emerges naturally from the evaluation rules. A very illustrative (but slightly cliche) example is the memoized version of the Fibonacci function:

```haskell
fib n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)
```

The `fib` function just indexes into the `fibs` list. This list is defined *in terms of itself*: instead of recursively calling `fib`, we just make newer elements of `fibs` depend on older ones by passing `fibs` and `(drop 1 fibs)` into `zipWith (+)`. It helps to visualize what this list looks like as more and more elements get evaluated:

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
  <p> `zipWith f` applies `f` to the first elements of both lists then recurses on their tails. In this case, the two lists are actually just pointers into the same list!</p>
</div>

Note how we only ever need the last two elements of the list. Since we don't have any other references to the `fibs` list, GHC's garbage collector can reclaim unused list elements as soon as we're done with them. So with GC, the actual execution looks more like this:

<div id="fibs-gc-animation" class="figure">
<ul class="animation">
  <li> <img src="fib-frames-gc/frame0.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames-gc/frame1.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames-gc/frame2.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames-gc/frame3.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames-gc/frame4.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames-gc/frame5.png" alt="The fibs list in memory." />
  </li>
</ul>
  <style type="text/css">
  #fibs-gc-animation ul li {
    text-align : left;
  }
  </style>
  <script type="text/javascript">
    animate("#fibs-gc-animation");
  </script>
  <p> More memory efficient: we only ever store a constant number of past results.</p>
</div>

## Lazy Arrays

Dynamic programming algorithms tend to have a very specific memoization style---sub-problems are put into an array and the inputs to the algorithm are transformed into array indices.  These algorithms are often presented in a distinctly imperative fashion: you initialize a large array with some empty value and then manually update it as you go along. You have to do some explicit bookkeeping at each step to save your result and there is nothing preventing you from accidentally reading in part of the array you haven't set yet.

This imperative-style updating is awkward to represent in Haskell. We could do it by either passing around an immutable array as an argument or using a mutable array internally, but both of these options are unpleasant to use and the former is not very efficient. 

Instead of replicating the imperative approach directly, we're going to take advantage of Haskell's laziness to define an array *that depends on itself*. The trick is to have the recursive call in the function to index into the array, and each array cell contain a call back to the function. This way, the logic of calculating each value once and then caching it is handled behind the scenes by Haskell's evaluation strategy. We compute the subproblems at most once in the order that we need and the array is always used *as if* it was fully filled out: we can never accidentally forget to save a result or access the array before that result has been calculated.

At its heart, this is the same idea as having a `fibs` list that depends on itself, just with an array instead of a list. An array just fits many dynamic programming problems better than a list or some other data structure.

We can rewrite our `fib` function to use this style of memoization. Note that this approach is actually strictly *worse* for Fibonacci numbers; this is just to illustrate how it works.

```haskell
fib' max = go max
  where go 0 = 0
        go 1 = 1
        go n = fibs ! (n - 1) + fibs ! (n - 2)
        fibs = Array.listArray (0, max) [go x | x <- [0..max]]
```

The actual recursion is done by a helper function: we need this so that our memoization array (`fibs`) is only defined *once* in a call to `fib'` rather than redefined at each recursive call!

For calculating `fib' 5`, `fibs` would be an array of 6 thunks each containing a call to `go`. The final result is the thunk with `go 5`, which depends on `go 4` and `go 3`; `go 4` depends on `go 3` and `go 2` and so on until we get to the entries for `go 1` and `go 0` which are the base cases `1` and `0`.

![The array of sub-problems for `fib 5`.](fib-array.png)

The nice thing is that this tangle of pointers and dependencies is all taken care of by laziness. We can't really mess it up or access parts of the array incorrectly because those details are *below our level of abstraction*. Filling out, updating and reading the array is all a result of forcing the thunks in the cells, not something we implemented explicitly in Haskell.

[elegant]: http://conal.net/blog/posts/elegant-memoization-with-functional-memo-tries
[combinators]: http://lukepalmer.wordpress.com/2008/10/14/data-memocombinators/

</div>
<div class="content">

# String Edit Distance

Now that we have a technique for doing dynamic programming neatly with lazy arrays, let's apply it to a real dynamic programming problem: **string edit distance**. This is one of the most common problems used to introduce dynamic programming in algorithms classes and a good first step towards implementing tree edit distance.

The **edit distance** between two strings is a measure of how *different* the strings are: it's the number of steps needed to go from one to the other where each step can either add, remove or modify a single character. The actual sequence of steps needed is called an **edit script**. For example:

  * `"brother"` → `"bother" `\ \ \ \ remove `'r'`
  * `"bother"`\ \  → `"brother"`\ \ add `'r'`
  * `"sitting"` → `"fitting"`\ \ modify `'s'` to `'f'`

The distance between strings \(a\) and \(b\) is always the same as the distance between \(b\) and \(a\). We go between the two edit scripts by inverting the actions: turning adds into removes, removes into adds and flipping the characters being modified.

The [Wagner-Fischer algorithm][wf-algorithm] is the basic approach for computing the edit distance between two strings. The core idea is to go through the two strings character by character, trying all three possible actions (adding, removing or modifying).

For example, to get the distance between `"kitten"` and `"sitting"`, we would start with the first two characters `k` and `s`. As these are different, we need to try the three possible edit actions and find them smallest distance. So we would compute the distances between `"itten"` and `"sitting"` for a delete, `"kitten"` and `"itting"` for an insert and `"itten"` and `"itting"` for a modify, and choose the smallest result.

This is where the branching factor comes from---each time the strings differ, we have to solve *three* recursive sub-problems to see which action is optimal at the given step.

We can express this as a recurrence relation. Given two strings \(a\) and \(b\), \(d_{ij}\) is the distance between their suffixes of length \(i\) and \(j\) respectively. So, for `"kitten"` and `"sitting"`, \(d_{6,7}\) would be the whole distance while \(d_{5,6}\) would be between `"itten"` and `"itting"`. 

  \[ \begin{align}
       d_{i0} & = i & \text{ for } 0 \le i \le m & \\
       d_{0j} & = j & \text{ for } 0 \le j \le n & \\
       d_{ij} & = d_{i-1,j-1}\ & \text{if } a_i = b_j & \\
       d_{ij} & = \min \begin{cases}
         d_{i-1,j} + 1\ \ \ \ (\text{delete}) \\
         d_{i,j-1} + 1\ \ \ \ (\text{insert}) \\
         d_{i-1,j-1} + 1\ (\text{modify}) \\
       \end{cases} & \text{if } a_i \ne b_j
     \end{align}
  \]

The base cases \(d_{i0}\) and \(d_{0j}\) arise when we've gone through all of the characters in one of the strings, since the distance is just based on the characters remaining in the other string. The recursive case has us try the three possible actions, compute the distance for the three results and return the best one.

We can transcribe this almost directly to Haskell:

```haskell
naive a b = d (length a) (length b)
  where d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) ==  b !! (j - 1) = d (i - 1) (j - 1)
          | otherwise = minimum [ d (i - 1) j       + 1
                                , d i (j - 1)       + 1
                                , d (i - 1) (j - 1) + 1
                                ]
```

And, for small examples, this code actually works! You can try it on `"kitten"` and `"sitting"` to get `3`. Of course, it runs in exponential time, which makes it freeze on larger inputs---even just `"aaaaaaaaaa"` and `"bbbbbbbbbb"` already take a while! The practical version of this algorithm relies on dynamic programming, caching each value \(d_{ij}\) into a two-dimensional array so that we only calculate it at most once.

We can do this transformation in much the same way we used a `fibs` array: we define `ds` as an array with a bunch of calls to `d i j` and we replace our recursive calls `d i j` with indexing into the array `ds ! (i, j)`.

```haskell
basic a b = d m n
  where (m, n) = (length a, length b)
        d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) ==  b !! (j - 1) = ds ! (i - 1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]

        ds = Array.listArray bounds
               [d i j | (i, j) <- Array.range bounds]
        bounds = ((0, 0), (m, n))
```

This code is really not that different from the naive version, but it's *far* faster.

## Lists as Loops

One thing that immediately jumps out from the above code is using `!!` for indexing into lists. Since lists are not a good data structure for random accesses, the `!!` is often a bit of a code smell. And, indeed, using lists causes problems for comparing longer strings.

We can solve this by converting `a` and `b` into arrays and then actually diffing those. (We can also make the arrays 1-indexed, simplifying the arithmetic a bit.)

```haskell
better a b = d m n
  where (m, n) = (length a, length b)
        a'     = Array.listArray (1, m) a
        b'     = Array.listArray (1, n) b

        d i 0 = i
        d 0 j = j
        d i j
          | a' ! i ==  b' ! j = ds ! (i - 1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]

        ds = Array.listArray bounds
               [d i j | (i, j) <- Array.range bounds]
        bounds = ((0, 0), (m, n))
```

The only difference here is defining `a'` and `b'` and then using `!` instead of `!!`. In practice, this is much faster than the `basic` version.

Now, it might seem a little odd to take lists as arguments just to immediately convert them into arrays. Why don't we just ask for arrays directly?

Partly, people in Haskell just don't use arrays very much. They would look odd in an API. People use a large set of sequential data types like lists, sequences, text, bytestrings, vectors, REPA... Chances are they would have to convert whatever they have to an array to use our function.

And how would they convert it? They'd probably go through an intermediate list! Just like us, they'd actually construct the array with something like `Array.listArray`. This seems wasteful in the same way: why create an intermediate list just to turn it into an array?

The real insight is that lists in Haskell are lazy and really behave more like loops than data structures. The list never has to completely exist in memory: just like with the `fibs` example, we only evaluate the list items as we need them, and the GC can collect old elements as soon as we're done with them.

So if we start with a `Sequence`, convert it to a list and feed that list into `Array.listArray`, we actually just get a loop that traverses the sequence and *safely* constructs the array. We can think of a list argument like this as a hole where you can plug in a loop rather than a normal argument.

With this in mind, our signature `Eq a => [a] -> [a] -> Distance` is ultimately the most general way to write this function: it accepts two *traversals* of some data structure and just diffs those by internally writing the traversal to a string. In fact, the `[a]` function more like iterators except much nicer: they're first-class data structures that we can pattern-match and they can be mapped over, filtered, folded and read however we like without any problems. 

[wf-algorithm]: http://en.wikipedia.org/wiki/Edit_distance#Basic_algorithm