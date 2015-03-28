---
title: Fun with Algebraic Structures
author: Tikhon Jelvis
---

## Monoids

So, what is an algebraic structure? Conventionally, it's a set with some number of operations *closed* over the set: functions that take a number of elements from the set to another element in the set. In the context of Haskell we prefer to talk about types rather than sets, but the idea remains the same: an algebraic structure is a type coupled with certain operations. The operations and any relationships between them are what defines the sort of algebraic structure we're working with---it's a signature for algebraic structures.

In Haskell, we have a natural way of representing a type with some attached operations: typeclasses. With the typeclass approach, a specific algebraic structure is a type that's an instance of the appropriate class. This lets us define our first algebraic structure which is present in the standard library, the **monoid**.

A monoid is a type with two operations: a binary operation for combining two values and producing a third and an value representing something empty. There are two additional restrictions. The first is that the empty value acts as an identity for the binary operation: combining anything with the identity returns the original thing, unchanged. The second is that the binary operation is **associative**: informally, you can add or remove parentheses however you like when you combine a bunch of things with the binary operation.

Here's the class as defined in the standard library:

```haskell
class Monoid m where
  mempty :: m                -- identity value
  mappend a b :: m -> m -> m -- binary operation
```

The names of the two operations try to evoke lists because a list with `++` as the binary operation and `[]` as the identity forms a monoid. However, I think this naming is a bit misleading: the binary operation and identity can be *anything* that satisfies the type and restrictions above and does not have to behave like a list or container at all. I usually use the `<>` operator for the binary operation, which is also provided in `Data.Monoid`.

With this in mind, we can write out the two restrictions (called "monoid laws") in a slightly more formal notation. The following two conditions must hold for every `a`, `b` and `c` in the underlying type:

```haskell
mempty <> a == a == a <> mempty
a <> b <> c == (a <> b) <> c == a <> (b <> c)
```

