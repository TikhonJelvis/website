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

The syntax for writing values of this type is also a combination of the syntax for products and sums. For example, lets take the rectangle. A value of `point` looks like: `(1, 2)`. So `point * point` would look like `((1, 2), (3, 4))`. We combine this with the `Rectangle` tag to get the whole value:

```ocaml
Rectangle ((1, 2), (3, 4))
```

Sometimes, we want to have a tag without *any* associated values. This is conceptually equivalent to a tag of `union`. Since `union` only has one possible value, it carries no additional information. This is very similar to the value of an enum in a language like `Java`; a type that is just a sum of tags like this behaves exactly like an enum. We can use this to define a boolean type:

```ocaml
type bool = True | False
```

This type declaration is very simple to read: a `bool` is either `True` or `False`. 

## Pattern Matching

We now know how to define algebraic data types. We also know how to write their values. But how do we actually use them? We need some way of unpacking products: from `int * int`, we want to be able to access the first `int` and the second `int` separated. For sum types, we need to know which variant we have: how to we differentiate between a `Left` and a `Right` value?

The answer to this is **pattern matching**. A pattern lets us give names to sub-parts of a value. It looks just like the value, with variables for the parts we want named. For example, if we want to break a `Rectangle` value into `x`, `y`, `width` and `height`, we would use this pattern:

```ocaml
Rectangle ((x, y), (width, height))
```

We could also break it up into `position` and `size` instead:

```ocaml
Rectangle (position, size)
```

If we don't care about some particular part of a pattern, it is customary to call it `_` instead of giving it a real name.

We can use patterns like this in a function definition. We simply use multiple patterns to handle all the possible cases. Here is the function for finding the area of a shape:

```ocaml
let area = function Circle (_, radius) -> pi * radius * radius
                  | Rectangle (_, (width, height)) -> width * height
```

Note how I didn't provide the area of a triangle. Getting the area from three points is a bit tedious! Unfortunately, this means that if somebody actually tried passing a triangle into this function, they would get an error at runtime. However, the compiler actually has enough information to know that this is possible, so I would get a warning when I compiled this function: 

    Warning 8: this pattern-matching is not exhaustive.
    Here is an example of a value that is not matched:
    Triangle (_, _, _)

The compiler even knows enough to tell me exactly which case I forgot and what sort of values would cause an error. This is very useful for preventing the very common mistake of forgetting about an alternative.

Pattern matching makes it very easy to unpack product types and choose between different variants of sum types. It also makes for very visual code---a pattern looks just like the data it matches.

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

We can combine polymorphism and recursive types to define some very common data structures. The most common data structure in functional programming is the linked list. A node of a linked list has two options: either it's the end of the list or it has a value and the rest of the list. We can transcribe this into an ADT directly:

```ocaml
type 'a list = End | Value of 'a * 'a list
```

We can use this to very easily encode even more complicated types like binary trees where every node is either a leaf or has two children:

```ocaml
type 'a binary_tree = Leaf of 'a | Node of 'a * 'a binary_tree * 'a binary_tree
```
