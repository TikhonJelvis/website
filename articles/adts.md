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

We can also imagine an *empty* tuple type: a product of *no* types. This gives us the type usually called `unit` containing a single value: `()` (the empty tuple).

## Sums

The second way to combine types is called a **sum** and represents *alternation*. Where a product of `char` and `int` has both a `char` *and* an `int`, a sum of these would have *either* an `int` *or* a `char`. The simplest possible way to do this would be with a union type, like C; however, this is a bit limited. In particular, you can obviously get the product of a type with itself where the union of a type with itself would not do anything. 

We can solve this by assigning a tag to each alternative. In the sum of say `int` and `int`, the tag will let use differentiate between the first `int` and the second `int`. These tags are called *constructors* because they are used to construct values of the type. Here is what the sum of `int` and `int` could look like if we called the tags `Left` and `Right`:

```ocaml
type ints = Left of int
          | Right of int
```

Individual values of this type would be written like `Left 42` or `Right 42`. These two are *distinct* values despite having the same number---they are not equal to each other.

So a sum type is just a union where each component type has a unique tag; they are often called "tagged unions". They are also sometimes called "variants" and "disjoint unions" (by allusion to math).

We could have a product type containing of *no* types, which was called `unit` and contained a single value: `()`, the empty tuple. Similarly, we can have a sum type containing *no* alternatives; this type has *no* values. It is usually called `void` and can be written like this:

```ocaml
type void
```

Having no values, it is not terribly useful in practice but makes for a nice symmetry with product types and is very useful from a theoretical standpoint.

## Putting It All Together

Algebraic data types are just types made up of sums and products. They represent a choice between combinations of values. As a contrived example, we can have a type representing different shapes:

```ocaml
type shapes = Circle of int * int
            | Triangle of point * point * point
            | Rectangle of point * point
```

Sometimes, we want to have a tag without *any* associated values. This is conceptually equivalent to a tag of `union`. Since `union` only has one possible value, it carries no additional information. This is very similar to the value of an enum in a language like `Java`; a type that is just a sum of tags like this behaves exactly like an enum. We can use this to define a boolean type:

```ocaml
type bool = True | False
```

This type declaration is very simple to read: a `bool` is either `True` or `False`. 

## Recursive Types

Algebraic data types can also be *recursive*: that is, an algebraic data type can have a field of its own type. This property makes them very good for representing abstract syntax trees. Let's imagine a very trivial language: we have numbers, variables, addition and multiplication. The type for expressions in this language would be:

```ocaml
type expression = Variable of string
                | Number of int
                | Add of expression * expression
                | Multiply of expression * expression
```

We can now write expressions in this language. For example, `1 + (a * 2)` would look like this:

```ocaml
Add (Number 1, Multiply (Variable "a", Number 2))
```

## Polymorphism

So far, all our types have been very concrete. We always know exactly what type everything is; when we defined a `point` type, it always had two `int`s. In reality, however, we really want to be able to define a `point` type that could take values of *any* types. We can do this by making the type definition **polymorphic**, which just means that we define it in terms of type *variables*---which can be *any* type---instead of concrete types like `int` or `char`. This is usually called **parametric** polymorphism, because the type variables act as parameters to the whole type.

It makes sense to redefine our point type to accept pairs of *any* two types rather than just `int` and `int`. Here is how we can do it by introducing two type variables `'a` and `'b`':

```ocaml
type ('a, 'b) point = 'a * 'b
```

Now we have defined a type that can be used for two `int`s: `(int, int) point`, two `char`s: `(char, char) point`, a combination of the two: `(int, char) point` or anything you would like: `((int, char) point, int) point`.