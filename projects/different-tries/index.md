---
title: Patricia Tries
author: Tikhon Jelvis
---

<div class="content">
# Patricia Tries

Patricia tries are an efficient data structure for mapping integer keys to values. They work well as immutable, persistent data structures and provide the asymptotics of hashtables with additional benefits like efficient ordered traversal. In practice the constant factors of Patricia tries are somewhat worse than hashtables but they are still good enough for everyday use.

Haskell's [`Data.IntMap`][intmap] data structure is a *big-endian binary Patricia trie* based on Chris Okasaki's paper ["Fast Mergeable Integer Maps"][okasaki].

![A binary trie for a three-bit key. *Figure from [Okasaki's paper][okasaki]*.](img/trie.png)

As a first step to understanding the data structure and to get practice benchmarking and optimizing Haskell, I reimplemented the core operations as described in Okasaki's paper, resulting in code virtually identical to `Data.IntMap`.

</div>
<div class="content">

One interesting tradeoff with Patricia tries is the branching factor or "span" of the tree. The span determines the number of bits from the key that the tree branches on. Haskell's IntMap, and Okasaki' paper, both default to a span of 1—they read the key in a bit at a time and each internal node has two children.

To experiment with different spans, I implemented a Patricia trie with a configurable span. A trie with a span of `s` (specified as a type-level natural number) looks at `s` bits at a time and has arrays of `2ˢ` child pointers at each internal node. This deviates significantly from both the paper and the estandard implementation, with a substantial performance penalty.

Currently, I'm trying to optimize it so that practical performance of my trie with a span of 1 is similar to the hard-coded binary variant. It's a fun crash course in benchmarking, profiling and optimizing real Haskell code.

The end-goal of the whole project is to implement an [adaptive radix tree][art] library, which is a novel variant of Patricia tries that should be more efficient both in speed of operations and in memory usage as compared to the binary Patricia trie we currently use. I'd then like to plug this into the [functional graph library][fgl] to see if we can get some real world performance improvements for purely functional graph algorithms.

</div>

[intmap]: hackage.haskell.org/package/containers/docs/Data-IntMap.html
[okasaki]: http://ittc.ku.edu/~andygill/papers/IntMap98.pdf
[fgl]: https://hackage.haskell.org/package/fgl
