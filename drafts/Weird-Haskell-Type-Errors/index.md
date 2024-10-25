---
title: Weird-Haskell-Type-Errors
author: Tikhon Jelvis
---

### Localization is hard

And even if we're dealing with a tricky case—the error points to the wrong part of the code or perhaps the message itself makes no sense—the error message gives us our only real starting point. We can only apply our other principles *after reading the message*. The error tells us **which constraints are inconsistent** and gives us a launching point **to divide and conquer**.

<!-- TODO: Some sort of example here? Or not.-->

### Common edge cases

Haskell has some rough edges which can lead to confusing error messages.

When the error *itself* is weird—not just a "normal" type error but something else—the [Haskell error index][haskell-error-index] is a great resource. You can search for the exact error you're seeing and get a detailed explanation of what's going on.

[haskell-error-index]: https://errors.haskell.org/

However, sometimes even "normal" errors are confusing in context. Here are a few that you might encounter (sourced [from the community][error-discourse-thread]).

#### No `Show` instance

Haskell's interpreter implicitly uses `show` to turn Haskell values into strings. If you evaluate an expression that does not have a `Show` instance, you get an error. The most common case is forgetting a function parameter:

``` ghci
λ> foldr (+) 0
<interactive>:3:1: error:
    • No instance for (Show ([Integer] -> Integer))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
```

In GHCi, `it` is a variable that refers to the expression you just entered and `print` is a function that GHCi implicitly calls on `it` in order to render the result.

#### Overloaded literals

Haskell numeric literals are *overloaded* by default: the expression `42` does not have to be an `Integer`, it can be *any* type that has a `Num` instance[^num-instances-discourse].

This means that if you write an arithmetic expression involving some non-numeric type, you get an error about a missing instance:

``` ghci
λ> 1 + True
<interactive>:4:3: error:
    • No instance for (Num Bool) arising from a use of ‘+’
    • In the expression: 1 + True
      In an equation for ‘it’: it = 1 + True
```

And hey, in principle, you *could* write a `Num` instance for `Bool`! It would just be an awful idea. What this really means is that `Bool` is not a number so you can't use it in arithmetic. And this error message was even *more* confusing in older versions of GHC—GHC is improving all the time so, unless you have a specific reason, it's worth using one of the latest releases.

[^num-instances-discourse]: The common problem with `Num` instances was suggested by [jackdk on Discourse](https://discourse.haskell.org/t/examples-of-haskell-type-errors/10468/8)

Extensions like `OverloadedStrings` and `OverloadedLists` make their corresponding literals behave in similar ways.

``` ghci
λ> "abc" && True
<interactive>:13:1: error:
    • No instance for (Data.String.IsString Bool)
        arising from the literal ‘"abc"’
    • In the first argument of ‘(&&)’, namely ‘"abc"’
      In the expression: "abc" && True
      In an equation for ‘it’: it = "abc" && True
```

As a general rule, error messages get worse as code gets more polymorphic. It's easy to accidentally run into this problem with overloaded literals.

``` ghci
λ> :set -XOverloadedStrings
λ> "abc" + 1
<interactive>:15:1: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘print’
      prevents the constraint ‘(Show a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance (Show a, Show b) => Show (Either a b)
          -- Defined in ‘Data.Either’
        instance Show Ordering -- Defined in ‘GHC.Show’
        instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
        ...plus 24 others
        ...plus 47 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of an interactive GHCi command: print it
```

Good rule of thumb: if you run into a confusing error message involving weird type variables and class constraints, and it's pointing to an expression involving a string, list or numeric literal, the literal could be the problem. If you know what the type *should* be, you can add an explicit type signature:

``` ghci
λ> :set -XOverloadedStrings
λ> ("abc" :: String) + 1
<interactive>:17:19: error:
    • No instance for (Num String) arising from a use of ‘+’
    • In the expression: ("abc" :: String) + 1
      In an equation for ‘it’: it = ("abc" :: String) + 1
```

#### `Foldable` instances

Core list functions on Haskell like `foldr` have been generalized to the [`Foldable`][haskell-foldable] class, leading to weird error messages for simple mistakes[^foldable-instances-discourse]:

<!-- TODO: foldable example -->

[haskell-foldable]: https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Foldable.html

[^foldable-instances-discourse]: The common problem with `Foldable` instances was suggested by [f-a](https://discourse.haskell.org/t/examples-of-haskell-type-errors/10468/4) as well as [jackdk](https://discourse.haskell.org/t/examples-of-haskell-type-errors/10468/8) on Discourse

#### Functions and do notation

The Haskell function type (`->`) has `Functor`, `Applicative` and `Monad` instances. These work the same way as the [`Reader`][haskell-reader] type but without the newtype wrapper. Unfortunately, this means that forgetting a function argument when using do-notation can give a weird error message[^reader-instance-discourse]:

[^reader-instance-discourse]: This edge case was suggested by [atravers on Discousrse](https://discourse.haskell.org/t/examples-of-haskell-type-errors/10468/15)

[haskell-reader]: https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html

#### Scoped type variables

Haskell type variables do not carry over into nested type signatures by default; for example, if you have a type variable `a` at the top level of a definition and then use `a` inside a `where` clause, the two variables will be totally separate, leading to weird errors[^scoped-type-variables-discourse]:

[^scoped-type-variables-discourse]: The common problem with scoped type variables was suggested by [olf on Discourse](https://discourse.haskell.org/t/examples-of-haskell-type-errors/10468/18)

<!-- TODO: scoped type variable -->

To be able to reuse `a` in a `where` clause, you have to enable `ScopedTypeVariables` and bind `a` with an explicit `forall`:

<!-- TODO: fixed example -->

