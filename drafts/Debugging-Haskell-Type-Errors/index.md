---
title: Debugging Haskell Type Errors
author: Tikhon Jelvis
---

Fixing Haskell type errors can be *hard*. Back when I was learning Haskell, dealing with type errors was the first real obstacle I had to overcome. I've seen exactly the same tendency with every Haskell beginner I've taught since.

At first, any error was confusing and intimidating, but, quickly, the majority of errors became straightforward—but a minority would take a lot of time, thinking and trial-and-error. Now, 15 years later, I *still* run into the occasional type error that takes concerted effort to fix. But now I've realized that *fixing type errors is a skill*: **Haskell type errors are a problem you can debug systematically**.

<!--more-->

Systematic debugging is something I only learned after years and years of programming. At first, I did not even realize debugging was a skill that could be learned—or taught. At some point I learned[^js-talk] to debug systematically, along with some principles[^nine-rules] I could follow to solve problems step-by-step rather than thrashing around until I stumbled onto the solution. We can approach Haskell type errors with the same mindset.

[^js-talk]: The first time I saw anybody talk about debugging *as a skill* was in a talk as part of Jane Street's internship program—unfortunately, more than a decade later, I don't remember exactly who gave the talk. At that point I had taken three years of CS courses at Berkeley and none of them ever touched on debugging like this; in hindsight, I would say this was the biggest missing piece in my CS education.

[^nine-rules]: [*Debugging: The 9 Indispensable Rules*][debugging-book] is, despite the click-baity titles, an amazing book for learning how to debug systematically. My approach for debugging in general and for fixing Haskell type errors in particular is heavily influenced by this book.

[debugging-book]: https://debuggingrules.com/?page_id=31

In this spirit, here are three principles that will help you systematically fix type errors even as a complete beginner—but that still me fix trickier errors with years and years of Haskell experience:

  1. **Read the error**: when you load your code and see a bunch of red text, don't panic. Stop and read the errors—the error messages are the compiler's best attempt to tell you what's going on, and they're where we will start our debugging process.

  2. **Think in constraints**: Haskell's type system works like a set of constraints (and type *inference* works like constraint solving). When you see an error, read it as “here is an *inconsistency* in your code's type” and not “here is exactly where your code is *wrong*”.

  3. **Divide and conquer**: if the fix is not immediately clear from the error message, you either need to *change some other part of the code* or, at the very least, you need to *understand some other part of the code* to figure out a fix. Use the compiler, the types and the structure of the code to find which other parts are relevant.

Let's dive into each principle and see how to put them into action.

</div>

<div class="content">

## Read the error

First step: when you see an error, *read the error*.

If there are multiple errors, *read all of them*. Or at least give them a skim.

This might sound obvious, but it's easy to overlook in the moment. When I've taught Haskell, I've consistently seen people skip reading the error. Haskell error messages can be intimidating, or perhaps they feel like you've made a mistake that you need to fix quickly. So people just make an assumption about what went wrong, jump back to the code, change something... Then, like as not, they end up with even *more* errors, more noise to sift through.

### Why are Haskell error messages hard?

Haskell error messages are not prefect. Error messages can get very verbose and don't always describe the right problem. More fundamentally, thanks to how Haskell's type checking and inference work, errors do not always point to the right part of the code[^type-error-localization]. And hey, nobody likes being told they've made an error! (Idle thought I've had repeatedly: can we redesign the type checker UX so that type errors feel more like helpful hints and less like errors or mistakes?)

Despite all this, error messages are still worth reading.

Many (most?) error messages are actually just pretty good. Once you cut through the noise, they give you the solution, no need to guess:

<!-- TODO: good error message example -->

And even if we're dealing with a tricky case—the error points to the wrong part of the code or perhaps the message itself makes no sense—the error message gives us our only real starting point. We can only apply our other principles *after reading the message*. The error tells us **which constraints are inconsistent** and gives us a launching point **to divide and conquer**.

<!-- TODO: Some sort of example here? Or not.-->

### Common edge cases

Haskell has a few rough edges that lead to confusing error messages even when the error is localized correctly. Here are a few to watch out for:

#### No `Show` instance

Haskell's interpreter implicitly uses `show` to turn Haskell values into strings.

#### Overloaded literals

Haskell numeric literals are *overloaded* by default: the expression `42` does not have to be an `Integer`, it can be *any* type that has a `Num` instance[^num-instances-discourse].

[^num-instances-discourse]: The common problem with `Num` instnces was suggested by [jackdk on Discourse](https://discourse.haskell.org/t/examples-of-haskell-type-errors/10468/8)

<!-- TODO: overloaded Num example -->

Extensions like `OverloadedStrings` and `OverloadedLists` make their corresponding literals behave in similar ways.

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

### Multiple error messages

When you have multiple similar errors, that is an additional signal by itself. (Did some other part of the code cause *all* of those errors?) And if the errors are different rather than repeating, the first error is not necessarily the best one to start with. But there's no way to know this if you don't read all of them!

<!-- TODO: multiple error message example -->

[^type-error-localization]: Type error localization in Haskell (and similar languages) is [an active area of research][localization-research] as is [the quality of compiler error messages more broadly][error-message-research]. David Binder pointed this research out to me [on Discourse][david-binder-discourse-post], including additional links and context.

[localization-research]: https://dl.acm.org/doi/10.1145/3138818

[error-message-research]: https://dl.acm.org/doi/10.1145/3344429.3372508

[david-binder-discourse-post]: https://discourse.haskell.org/t/examples-of-haskell-type-errors/10468/9

</div>

<div class="content">

## Think in Constraints

</div>

<div class="content">

## Divide and Conquer

</div>

<div class="content">

(TODO: Conclusion goes here...)
