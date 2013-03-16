---
title: Algebraic Data Types
author: Tikhon Jelvis
---

<div class="content">

# Algebraic Data Types

<!-- really need to edit this paragraph! -->
Algebraic data types (ADTs) are a simple way of representing custom data, usually associated with functional programming languages like Haskell and OCaml. However, there is nothing inherently functional about them and you could just as easily use them in an imperative programming language as well.

I'm going to use OCaml syntax throughout this article, but all the ideas transcend any particular language. 

Most languages provide some basic, primitive types like ints, floats, doubles and chars. ADTs are simply a way to combine these primitive types to represent values in the domain of your program.

## Products

There are two fundamental ways to combine types. The first, usually called a **product**, simply combines multiple types by having a value of each type. So the product of `char` and `int` would have values that each carry *both* a `char` *and* an `int`. These are also known as tuples (like Python), structs (like C) or records. In OCaml, we would write this type as `char * int` and a value would like `('t', 42)`. 

The simplest example of a useful product type would be a 2D point:

```ocaml
type point = int * int
```

This shows an obvious but important property of product types: the product of a type with itself is a new type.

We can understand the name by allusion to math: a product type corresponds to a Cartesian product of sets. Another way of thinking about it is by counting the elements of the types: if `char` has `x` possible elements and `int` has `y`, then there will be `x * y` possible pairs.

## Sums

The second way to combine types is called a **sum** and represents *alternation*. Where a product of `char` and `int` has both a `char` *and* an `int`, a sum of these would have *either* an `int` *or* a `char`. The simplest possible way to do this would be with a union type, like C; however, this is a bit limited. In particular, you can obviously get the product of a type with itself where the union of a type with itself would not do anything. 

We can solve this by assigning a tag to each alternative. In the sum of say `int` and `int`, the tag will let use differentiate between the first `int` and the second `int`. These tags are called *constructors* because they are used to construct values of the type. Here is what the sum of `int` and `int` could look like if we called the tags `Left` and `Right`:

```ocaml
type ints = Left of int
          | Right of int
```

So a sum type is just a union where each component type has a unique tag--they are often called "tagged unions". They are also sometimes called "variants" and "disjoint unions" (by allusion to math).

## Combining

Algebraic data types are just types made up of sums and products. They represent a choice between combinations of values. 