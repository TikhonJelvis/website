---
title: F-Algebras
author: Tikhon Jelvis
---

Abstract math, as the name implies, is a wonderful source of abstractions---something we value deeply as programmers. It's a toolbox of ideas that are simple, well-defined and universal.

A particularly prolific source of mathematical abstractions useful to programmers is abstract algebra, the study of **algebraic structures**. These come up in everything from cryptography to computer graphics to parsing and can be used to express interesting structure in your own code.

We can go further and generalize all the different algebraic structures we care about into F-algebras. Apart from unifying a bunch of abstractions we already find useful, F-algebras also come up as a natural way to structure generic data transformations in functional programming, making them a useful part of functional programmer's arsenal.

![The Haskell `diagrams` library expresses operations over shapes in terms of monoids, a specific sort of algebraic structure, giving it a simple consistent interface throughout.][diagrams-example]

<!--more-->

## Algebraic Structures

An **algebraic structure** is some set \(S\) with some operations *closed* over \(S\). A "closed operation" is a function that takes some number of elements in \(S\) to another element in \(S\). Additionally, algebraic structures can have "identity elements" which are specific elements from the set that have some sort of relationship with the defined operations.

Since we're talking about Haskell, we're going to replace sets with types, but the rest of the idea is the same: an algebraic structure is some type `τ` with some number of operations and identities. All these functions take some number of arguments of type `τ` and produce a single result of type `τ`. (We don't really have to worry about closure because any function with the right type will be closed over that type by definition.)

Haskell has a natural way of associating functions (and values) with types: typeclasses. A good example structure to start with is the **monoid** which has a single binary operation and an identity. It's actually defined in the standard library module `Data.Monoid` as follows:

```haskell
class Monoid m where
  mempty :: m            -- identity
  mappend :: m -> m -> m -- binary operation
```

`mappend` is an awkward name for a function, so I use the provided infix synonym `<>`.

Monoids have a few additional restrictions called the monoid laws which are invariants that ust hold for every valid instance of the structure. In particular, the binary operation has to be associative and the "empty" value has to be an identity for the operation:

```haskell
mempty <> a == a == a <> mempty
a <> b <> c == (a <> b) <> c == a <> (b <> c)
```

This is very important for using monoids or other specific algebraic structures, but something we will broadly ignore as we generalize them to F-algebras. We *could* extend our abstraction to include these---and people have---but it's not necessary for most functional programming uses.

