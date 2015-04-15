---
title: F-Algebras
author: Tikhon Jelvis
---

Abstract math, as the name implies, is a wonderful source of abstractions---something deeply valuable to programmers. It's a toolbox of ideas that are simple, well-defined and universal.

A particularly prolific source of useful mathematical abstractions is abstract algebra, the study of **algebraic structures**. Algebraic structures come up in everything from cryptography to computer graphics to parsing and can be used to neatly organize your own code.

Going further, we can generalize algebraic structures into F-algebras. Apart from unifying abstractions we already find useful, F-algebras also come up as a natural way to structure generic data transformations by generalizing the higher-order `fold` function, making them a useful part of any functional programmer's arsenal.

![The Haskell [`diagrams`][diagrams] library exposes a simple and consistent interface for working with shapes and pictures in terms of monoids, a kind of algebraic structure.][diagrams-example]

<!--more-->

</div>
<div class="content">

## Algebraic Structures

An **algebraic structure** is some set \(S\) with some operations *closed* over \(S\). A "closed operation" is a function that takes some number of elements in \(S\) to another element in \(S\) (rather than an element from some other set). It is "enclosed" inside \(S\). Algebraic structures can also have **identity elements** which are specific elements from \(S\) that have some sort of relationship with the defined operations.

Since we're talking about Haskell, we're going to replace sets with types, but the rest remains the same: an algebraic structure is some type&nbsp;`τ` with some number of operations and identities. All these functions take some number of arguments of type&nbsp;`τ` and produce a single result of type&nbsp;`τ`. (We don't really have to worry about closure because any function with the right signature will be closed over our type by definition.)

Haskell has a natural way of associating functions (and values) with types: typeclasses. A good example structure to start with is the **monoid** which has a single binary operation and an identity. It's actually defined in the standard library module `Data.Monoid` as follows:

```haskell
class Monoid m where
  mempty  :: m           -- identity
  mappend :: m -> m -> m -- binary operation
```

`mappend` is an awkward name, so I'll use the provided infix synonym&nbsp;`<>`.

Monoids have a few additional restrictions called the monoid laws which are invariants that hold for every valid instance of the structure. The binary operation has to be associative and the "empty" value has to be an identity for the operation:

```haskell
mempty <> a == a == a <> mempty
a <> b <> c == (a <> b) <> c == a <> (b <> c)
```

This is important for using monoids or other specific algebraic structures, but something we will broadly ignore as we generalize them to F-algebras. We *could* extend our abstraction to include these---and people have---but it's not necessary for many interesting uses.

Another example structure is the group. We use groups less than monoids in Haskell, but they're far more common in pure and applied mathematics. Groups are monoids with an additional inverse operation.

```haskell
class Group g where
  gempty   :: g
  ginverse :: g -> g
  gappend  :: g -> g -> g
```

Other examples of algebraic structures you may have run into include rings, fields, lattices and variations on each.

### Generalizing

If we look at the types of a structure's operations in uncurried form, we see a pattern emerge. Monoids have two: `τ` and `(τ, τ) -> τ`; groups have three: `τ`, `τ -> τ` and `(τ, τ) -> τ`. All these functions return a single `τ` and they all take a tuple of some number of `τ` elements---where the number can be 0 or 1.

```haskell
gempty   :: ()     -> τ
ginverse :: (τ)    -> τ
gappend  :: (τ, τ) -> τ
```

Tuples are often called product types because creating a tuple from two types behaves similarly to multiplication. This logic gives us another way of recognizing this pattern---each function takes the form `τⁿ` or `τ` multiplied by itself `n` times. Identities are not a special case in this view: we can rewrite `τ` as `() -> τ` where `()` is `τ⁰`. This is still consistent with multiplication where \(x^0 = 1\).

```haskell
gempty   :: τ⁰ -> τ
ginverse :: τ¹ -> τ
gappend  :: τ² -> τ
```

Now we have a simple way to generalize algebras by the number of operations they have and how many arguments each operation takes---the **signature** of the algebra. Monoids would have the signature (0, 2), groups would have (0, 1, 2), rings would be (0, 0, 1, 2, 2) and so on. (Don't worry about the exact details; all that's important is that different structures have different signatures.)

Algebraic structures are common patterns that have been "factored out" by mathematicians because they come up again and again---people noticed certain repeated patterns and factored them into monoids, groups and so on. We've just done the same thing at a higher level and factored all *those* patterns into a generic notion of an algebra with some signature.

Algebras in this sense are both general enough to cover most algebraic structures used by mathematicians and powerful enough to allow some interesting reasoning. Any conclusion about algebras applies equally well to any conforming sort of structure. If you're interested in these sort of conclusions, take a look at the field of [universal algebra][universal-algebra] which studies these algebras.

</div>
<div class="content">

## F-Algebras

While algebras are already interesting, we are not done factoring. So far, we have a type `m` with a bunch of functions in the form `mⁿ → m`. We can actually do a neat trick to combine all these into *one* function. Let's take a look again at monoids which have `mappend :: (m, m) -> m` and `mempty :: () -> m`. In essence, we have a choice of either giving no arguments or giving two arguments, and we can model a choice as a *sum type* like `Either`:

```haskell
op :: Monoid m => Either (m, m) () -> m
op (Left (m, m)) = mappend (m, m)
op (Right ())    = mempty ()
```

We can actually use this approach to transform *any* number of functions with the same return type into one by nesting `Either`s. However, this quickly gets unwieldly; a much better approach is to define custom semantic types for each algebra:

```haskell
data MonoidArgument m = Mappend m m
                      | Mempty

data GroupArgument g = Gappend g g
                     | Ginverse g
                     | Gempty
```

We can do the same thing for rings and lattices and so on.

This gives rise to a compact definition for each kind of structure:

```haskell
type Monoid m = MonoidArgument m -> m

type Group g = GroupArgument g -> g
```

And how can we combine all these into one notion? We just replace the part that varies between each structure with a variable:

```haskell
type Algebra f a = f a -> a
```

`f` is the algebra's signature and can be any functor. And that's all an f-algebra is: a specific function `f a -> a` for some functor `f` and some type `a`. The important part is how this construction neatly captures the idea of some algebra with a `f` as the signature.

[universal-algebra]: http://en.wikipedia.org/wiki/Universal_algebra
[banana-brackets]: banana-brackets.png
[diagrams]: http://projects.haskell.org/diagrams/
[diagrams-example]: data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMzAwcHQiIGhlaWdodD0iMzAwcHQiIHZpZXdCb3g9IjAgMCAzMDAgMzAwIiB2ZXJzaW9uPSIxLjEiPgo8ZyBpZD0ic3VyZmFjZTk3Ij4KPHBhdGggc3R5bGU9ImZpbGwtcnVsZTpub256ZXJvO2ZpbGw6cmdiKDIyLjU5OTEyNiUsMjkuMDU5NTU5JSwzMS4xNjI5NTYlKTtmaWxsLW9wYWNpdHk6MTtzdHJva2Utd2lkdGg6MS4yO3N0cm9rZS1saW5lY2FwOmJ1dHQ7c3Ryb2tlLWxpbmVqb2luOm1pdGVyO3N0cm9rZTpyZ2IoMCUsMCUsMCUpO3N0cm9rZS1vcGFjaXR5OjE7c3Ryb2tlLW1pdGVybGltaXQ6MTA7IiBkPSJNIDAgMTE0LjcxODc1IEwgMTE0LjcxODc1IDMwMCBMIDMwMCAxODUuMjgxMjUgTCAxODUuMjgxMjUgMCBaICIvPgo8cGF0aCBzdHlsZT0iZmlsbC1ydWxlOm5vbnplcm87ZmlsbDpyZ2IoMzIuMDA3ODM0JSw0MC42MzE0NzclLDQzLjQzOTE3NCUpO2ZpbGwtb3BhY2l0eToxO3N0cm9rZS13aWR0aDoxLjI7c3Ryb2tlLWxpbmVjYXA6YnV0dDtzdHJva2UtbGluZWpvaW46bWl0ZXI7c3Ryb2tlOnJnYigwJSwwJSwwJSk7c3Ryb2tlLW9wYWNpdHk6MTtzdHJva2UtbWl0ZXJsaW1pdDoxMDsiIGQ9Ik0gMTE4LjI1IDE1IEwgMTUgMTgxLjc1IEwgMTgxLjc1IDI4NSBMIDI4NSAxMTguMjUgWiAiLz4KPHBhdGggc3R5bGU9ImZpbGwtcnVsZTpub256ZXJvO2ZpbGw6cmdiKDM4LjkxMTI4NSUsNDkuMTIyMTQxJSw1Mi40NDY2MDUlKTtmaWxsLW9wYWNpdHk6MTtzdHJva2Utd2lkdGg6MS4yO3N0cm9rZS1saW5lY2FwOmJ1dHQ7c3Ryb2tlLWxpbmVqb2luOm1pdGVyO3N0cm9rZTpyZ2IoMCUsMCUsMCUpO3N0cm9rZS1vcGFjaXR5OjE7c3Ryb2tlLW1pdGVybGltaXQ6MTA7IiBkPSJNIDI0NC44Mzk4NDQgNzEuMjQ2MDk0IEwgNzEuMjQ2MDk0IDU1LjE2MDE1NiBMIDU1LjE2MDE1NiAyMjguNzUzOTA2IEwgMjI4Ljc1MzkwNiAyNDQuODM5ODQ0IFogIi8+CjxwYXRoIHN0eWxlPSJmaWxsLXJ1bGU6bm9uemVybztmaWxsOnJnYig0NC41NjY5NTElLDU2LjA3ODEzNCUsNTkuODI1OTYxJSk7ZmlsbC1vcGFjaXR5OjE7c3Ryb2tlLXdpZHRoOjEuMjtzdHJva2UtbGluZWNhcDpidXR0O3N0cm9rZS1saW5lam9pbjptaXRlcjtzdHJva2U6cmdiKDAlLDAlLDAlKTtzdHJva2Utb3BhY2l0eToxO3N0cm9rZS1taXRlcmxpbWl0OjEwOyIgZD0iTSAyNDguNjc1NzgxIDE5My41NzAzMTIgTCAxOTMuNTcwMzEyIDUxLjMyODEyNSBMIDUxLjMyODEyNSAxMDYuNDMzNTk0IEwgMTA2LjQzMzU5NCAyNDguNjc1NzgxIFogIi8+CjxwYXRoIHN0eWxlPSJmaWxsLXJ1bGU6bm9uemVybztmaWxsOnJnYig0OS40NDUyNzElLDYyLjA3ODA1NiUsNjYuMTkxMDU2JSk7ZmlsbC1vcGFjaXR5OjE7c3Ryb2tlLXdpZHRoOjEuMjtzdHJva2UtbGluZWNhcDpidXR0O3N0cm9rZS1saW5lam9pbjptaXRlcjtzdHJva2U6cmdiKDAlLDAlLDAlKTtzdHJva2Utb3BhY2l0eToxO3N0cm9rZS1taXRlcmxpbWl0OjEwOyIgZD0iTSAxNTQuMjY5NTMxIDI0Mi4zNTU0NjkgTCAyNDIuMzU1NDY5IDE0NS43MjY1NjIgTCAxNDUuNzI2NTYyIDU3LjY0MDYyNSBMIDU3LjY0MDYyNSAxNTQuMjY5NTMxIFogIi8+CjxwYXRoIHN0eWxlPSJmaWxsLXJ1bGU6bm9uemVybztmaWxsOnJnYig1My43ODE5NTMlLDY3LjQxMTgxJSw3MS44NDk0MzglKTtmaWxsLW9wYWNpdHk6MTtzdHJva2Utd2lkdGg6MS4yO3N0cm9rZS1saW5lY2FwOmJ1dHQ7c3Ryb2tlLWxpbmVqb2luOm1pdGVyO3N0cm9rZTpyZ2IoMCUsMCUsMCUpO3N0cm9rZS1vcGFjaXR5OjE7c3Ryb2tlLW1pdGVybGltaXQ6MTA7IiBkPSJNIDgyLjY5MTQwNiAxODcuNDkyMTg4IEwgMTg3LjQ5MjE4OCAyMTcuMzEyNSBMIDIxNy4zMTI1IDExMi41MTE3MTkgTCAxMTIuNTExNzE5IDgyLjY5MTQwNiBaICIvPgo8cGF0aCBzdHlsZT0iZmlsbC1ydWxlOm5vbnplcm87ZmlsbDpyZ2IoNTcuNzE0NTQzJSw3Mi4yNDg1NjQlLDc2Ljk4MDU3MSUpO2ZpbGwtb3BhY2l0eToxO3N0cm9rZS13aWR0aDoxLjI7c3Ryb2tlLWxpbmVjYXA6YnV0dDtzdHJva2UtbGluZWpvaW46bWl0ZXI7c3Ryb2tlOnJnYigwJSwwJSwwJSk7c3Ryb2tlLW9wYWNpdHk6MTtzdHJva2UtbWl0ZXJsaW1pdDoxMDsiIGQ9Ik0gOTkuMTQ4NDM4IDExNS4xNjc5NjkgTCAxMTUuMTY0MDYyIDIwMC44NTE1NjIgTCAyMDAuODQ3NjU2IDE4NC44MzU5MzggTCAxODQuODMyMDMxIDk5LjE1MjM0NCBaICIvPgo8cGF0aCBzdHlsZT0iZmlsbC1ydWxlOm5vbnplcm87ZmlsbDpyZ2IoNjEuMzMxMzYyJSw3Ni42OTY5NDclLDgxLjY5OTY5NiUpO2ZpbGwtb3BhY2l0eToxO3N0cm9rZS13aWR0aDoxLjI7c3Ryb2tlLWxpbmVjYXA6YnV0dDtzdHJva2UtbGluZWpvaW46bWl0ZXI7c3Ryb2tlOnJnYigwJSwwJSwwJSk7c3Ryb2tlLW9wYWNpdHk6MTtzdHJva2UtbWl0ZXJsaW1pdDoxMDsiIGQ9Ik0gMTU2LjM4NjcxOSAxMDQuMjE0ODQ0IEwgMTA0LjIxNDg0NCAxNDMuNjEzMjgxIEwgMTQzLjYxMzI4MSAxOTUuNzg1MTU2IEwgMTk1Ljc4NTE1NiAxNTYuMzg2NzE5IFogIi8+CjxwYXRoIHN0eWxlPSJmaWxsLXJ1bGU6bm9uemVybztmaWxsOnJnYig2NC42OTMwMDklLDgwLjgzMTQ5JSw4Ni4wODU4NzklKTtmaWxsLW9wYWNpdHk6MTtzdHJva2Utd2lkdGg6MS4yO3N0cm9rZS1saW5lY2FwOmJ1dHQ7c3Ryb2tlLWxpbmVqb2luOm1pdGVyO3N0cm9rZTpyZ2IoMCUsMCUsMCUpO3N0cm9rZS1vcGFjaXR5OjE7c3Ryb2tlLW1pdGVybGltaXQ6MTA7IiBkPSJNIDE3OS4yMjI2NTYgMTQwLjIwNzAzMSBMIDE0MC4yMDcwMzEgMTIwLjc4MTI1IEwgMTIwLjc4MTI1IDE1OS43OTY4NzUgTCAxNTkuNzk2ODc1IDE3OS4yMjI2NTYgWiAiLz4KPHBhdGggc3R5bGU9ImZpbGwtcnVsZTpub256ZXJvO2ZpbGw6cmdiKDY3Ljg0MzEzNyUsODQuNzA1ODgyJSw5MC4xOTYwNzglKTtmaWxsLW9wYWNpdHk6MTtzdHJva2Utd2lkdGg6MS4yO3N0cm9rZS1saW5lY2FwOmJ1dHQ7c3Ryb2tlLWxpbmVqb2luOm1pdGVyO3N0cm9rZTpyZ2IoMCUsMCUsMCUpO3N0cm9rZS1vcGFjaXR5OjE7c3Ryb2tlLW1pdGVybGltaXQ6MTA7IiBkPSJNIDE2MC44OTQ1MzEgMTYwLjg5NDUzMSBMIDE2MC44OTQ1MzEgMTM5LjEwMTU2MiBMIDEzOS4xMDE1NjIgMTM5LjEwMTU2MiBMIDEzOS4xMDE1NjIgMTYwLjg5NDUzMSBaICIvPgo8L2c+Cjwvc3ZnPgo=
