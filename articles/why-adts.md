---
title: Why Algebraic Data Types Just Make Sense
author: Tikhon Jelvis
---

<div class="content">

# Why Algebraic Data Types Just Make Sense

## Starting from Scratch

Let's imagine we're designing a new language from scratch. We have the basics: we can work with single values like numbers or functions. We can do arithmetic, we can pass values around, we can write functions, but that's it.

So far, we can write things like:

    1
    1 + 2
    λ x → x + 1
    f(x) = x + 1
    
Adding types to this is simple. We have our basic built-in types like `Int`, function types like `Int → Int` and polymorphic types like `a → a` where `a` can be *any* type. As a quick example of what we can do, here's the identity function with a type:

    id : a → a
    id(x) = x

## Tuples

What's the natural next step?

We want some way to work with *composite* data. Some way to combine multiple pieces of data into one. What's the simplest way to do this? We need some way to work with two pieces of data as one, so let's do that directly using an ordered pair. Here is an example pair:

    pair : (Int, Int)
    pair = (1, 2)
    
Once we have pairs, we may as well have arbitrarily sized tuples. After all, a tuple of three elements is the same as a nested pair: `(a, (b, c)) ≡ (a, b, c)`[^bot]. Tuples are very handy because they give us a convenient way to both accept multiple arguments and return multiple values. We can now write functions like this:

    f : (Int, Int) → (Int, Int)
    f(x, y) = (x * x, y * y)
    
This means we do not need to make multiple arguments or multiple return values a special case in the language. Apart from being elegant, this also makes it easier to write code the works for functions with any number of return values. I find this a *much* better design than the special multiple return values of languages like Racket which require a lot of special machinery and complexity (like Racket's `let-values`) without providing much in return.

[^bot]: Technically, this is not strictly true for non-strict languages     like Haskell thanks to the presence of ⊥. In particular, the nested form can have both `(⊥, ⊥)` and `(⊥, (⊥, ⊥))` while the flat one can only have `(⊥, ⊥, ⊥)`. However, this is usually relatively unimportant.

### Extraction

We also need some structured way to get values out of our tuples. The simplest choice—working with pairs—is to provide a `first` and `second` function:

    first : (a, b) → a
    second : (a, b) → b
    
If we were just using nested pairs instead of more general tuples, this would be enough to do anything—much like Lisp's canonical `car` and `cdr`. However, this approach would require a rather ungainly mess of `first`s and `second`s to get anything out of a large tuple. Moreover, it does not generalize well to larger structures. A better solution is to allow pattern-matching on tuples with functions or `let` statements:

    let (x, y) = (1, 2)
    f(x, y) = ...
    
This is pleasant to read and works reasonably well for larger tuples. It also allows functions like `first` and `second` to be defined in a library, so they need not be provided by the language itself.

### Records and structs

We would also like some way to name parts of our tuples to make it easier to get and set the values. We can already accomplish this by defining our own abstract type with getters and setters:

    type Point = (Int, Int)
    getX(x, y) = x
    setX((x, y), newX) = (newX, y)
    getY(x, y) = y
    setY((x, y), newY) = (x, newY)
    
This is a reasonable practice: we now have a `Point` type with explicit getters and setters for `x` and `y`, so we can refer to the coordinates by name. Again, this is somewhat reminiscent of Lisp, except with static types[^sicp]. However, this is somewhat awkward, so it makes sense to add a neater way to accomplish the same task. This is how we get to Haskell/ML records or C structs. We can imagine some syntax like:

    type Point = { x : Int, y : Int }
    
which also automatically creates some way to read and set these values. These are just like tuples except the components can be identified by name rather than position.

So we've gotten all the way to records or structs, starting with basic pairs. The best part is that this is all trivial syntax sugar: ultimately, the semantics of our record boil down to the semantics of the rather humbler pair! All the steps to get to this design are very natural, and the result strikes a great compromise between simplicity and expressiveness. 

[^sicp]: In particular, this is how [*Structure and Interpretation of Computer Programs*](http://mitpress.mit.edu/sicp/) (SICP) introduces abstract data types. 

## Interesting properties

Now let's look at the "structure" of a pair. In particular, we're interested in how we can relate a pair of types `a` and `b` to the types themselves. This is actually really simple: given an `(a, b)`, we can always get an `a` out and we can always get a `b` out. We always have our "projection" functions:

    first : (a, b) → a
    second : (a, b) → b
    
We can turn this into a diagram: ![diagram of a pair's structure](img/products.svg) This is just a visual way to see the really simple structure I described earlier.

Pairs have an additional interesting property: they're the simplest possible element with this particular structure.

</div>