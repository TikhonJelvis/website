---
title: Debugging Haskell Type Errors
author: Tikhon Jelvis
---

Fixing Haskell type errors can be *hard*. Back when I was learning Haskell, dealing with type errors was the first real obstacle I had to overcome. I've seen exactly the same tendency with every Haskell beginner I've taught since.

With a bit of experience, I got used to the quirks of GHC's typechecker and Haskell's standard library so most errors became easy to resolve. Most *but not all*. Worse yet, the intuition that helped me in easier cases did not scale to harder errors; instead, fixing these hard errors took a frustrating amount of time, thinking and trial-and-error. I did not have a mental toolkit for debugging confusing type errors.

Haskell type errors are not unique in this regard—I had exactly the same experience with debugging in general. When I started programming all bugs were hard; I quickly built up an intuition for fixing *most* bugs; but I did not have the mental tools to deal with hard bugs, leaving me hitting my head against a metaphorical wall when I couldn't guess the cause of a bug up-front.

Once I realized that debugging *was a skill I could learn*, I improved substantially by approaching bugs systematically. I slowed down, stopped making baseless assumptions and instead approached problems step-by-step, following some simple principles.

There is an important parallel here. Just like other sorts of bugs, **we can solve Haskell type errors systematically**. Fixing type errors is a skill, and it's a skill we can improve by slowing down and leaning on three simple principles:

 1. **Read the error**
 2. **Think in constraints**
 3. **Divide and conquer**

<!--more-->

</div>
<div class="content">

Systematic debugging is something I only learned after years and years of programming. At first, I did not even realize debugging was a skill that could be learned—or taught. At some point I learned[^js-talk] to debug systematically, along with some principles[^nine-rules] I could follow to solve problems step-by-step rather than thrashing around until I stumbled onto the solution. We can approach Haskell type errors with the same mindset.

[^js-talk]: The first time I saw anybody talk about debugging *as a skill* was in a talk as part of Jane Street's internship program—unfortunately, more than a decade later, I don't remember exactly who gave the talk. At that point I had taken three years of CS courses at Berkeley and none of them ever touched on debugging like this; in hindsight, I would say this was the biggest missing piece in my CS education.

[^nine-rules]: [*Debugging: The 9 Indispensable Rules*][debugging-book] is, despite the click-baity titles, an amazing book for learning how to debug systematically. My approach for debugging in general and for fixing Haskell type errors in particular is heavily influenced by this book.

[debugging-book]: https://debuggingrules.com/?page_id=31

In this spirit, here are the three principles—rules—I use to deal with harder type errors:

  1. **Read the error**: when you load your code and see a bunch of red text, don't panic. Stop and read the errors—the error messages are the compiler's best attempt to tell you what's going on, and they're where we will start our debugging process.
  2. **Think in constraints**: Haskell's type system works like a set of constraints (and type *inference* works like constraint solving). When you see an error, read it as “here is an *inconsistency* in your code's type” and not “here is exactly where your code is *wrong*”.
  3. **Divide and conquer**: if the fix is not immediately clear from the error message, you either need to *change some other part of the code* or, at the very least, you need to *understand some other part of the code* to figure out a fix. Use the compiler, the types and the structure of the code to find which other parts are relevant.

Let's dive into each principle and see how to put them into action.

</div>

<div class="content">

## Read the error

First step: when you see an error, *read the error*.

If there are multiple errors, *read all of them*. Or at least give them a skim. The first error you get is not necessarily the best error to start with.

This might sound obvious but, in practice, it isn't. Everyone I've mentored started out with a tendency to jump straight to their code as soon as they saw any errors. I've caught myself doing the same thing sometimes! Error messages are a bit intimidating; it feels like you've done something wrong. Wanting to fix it immediately is a natural instinct.

Instead of seeing errors as criticism, we should see them as *hints*—the compiler is trying to help us! There's an interesting UX challenge here: can we design an interface that makes errors seem helpful by default?

Unfortunately, Haskell error messages are not prefect—they can be hard to read and understand. Let's look through a few common issues and how to deal with them:

### Cutting through the noise

Haskell error messages get verbose *quickly*. Each error produces a lot of noise.

Haskell errors are verbose because they try to present all the information you'd need in a vacuum. An error message may have a number of parts:

 * the error itself
 * additional context about the error
 * steps to identify which part of the code caused the error
 * suggestions for how to fix the error

Here's a simple type error message from one of my projects with its three distinct parts highlighted in different colors:

<pre class="error">
<code>
<span class="error-heading">src/Theta/Target/Python.hs:130:50: warning: [-Wdeferred-type-errors] …</span>
    <span class="error-message">• Couldn't match expected type ‘Theta.Type’ with actual type ‘()’</span>
    <span class="error-location">• In the third argument of ‘toReference’, namely ‘()’</span>
      <span class="error-location">In the expression: toReference prefix currentModule ()</span>
      <span class="error-location">In an equation for ‘type_’:</span>
      <span class="error-location">type_ = toReference prefix currentModule ()</span>
  </code>
</pre>

This message has three parts:

 1. The first line tells us the location[^file-locations] as well as the type of error (a deferred type error warning in this case)[^deferred-errors]
 2. The second line is the actual error message.
 3. The third, fourth, fifth *and* sixth lines all tell us *where* the error is in our code.

Even the simplest type error leads to six lines of error text. It only takes a handful of errors like this to fill up an entire screen!

If you're using an editor that highlights errors in place, none of the location information—4½ out of 6 lines!—matters. The only information we need is:

  1. It's a type error.
  2. We expected a `Theta.Type` but got a `()`

So: the first trick to reading Haskell type errors is to mentally filter out the bits that don't matter—which is often *most* of the message! Hopefully we will get a flag to slim down error messages in the future.

More involved code can produce even more noise. A slightly different type error in the same `Python.hs` file, for example, produce *42 lines of localization information*[^42-lines]—none of which was useful because my editor highlighted the exact part of the code I needed to look at!

[^deferred-errors]: This type error is actually a *warning* because I have the `-Wdeferred-type-errors` flag turned on. This flag is great for development because it lets the compiler surface more type errors and lets you experiment with the working parts of your code even if other parts don't typecheck.

[^file-locations]: The location is given as a path to the file, a line number and a column number. This error is in `Python.hs` at line 130 starting on character 50. Some editors recognize this format and let you jump to the specified location.

[^42-lines]: Seriously! At least there is a great suggestion for a fix on line 4 (highlighted in blue).
  <pre class="error">
    <code>
  <span class="error-heading">src/Theta/Target/Python.hs:137:28: warning: [&#45;Wdeferred&#45;type&#45;errors] …</span>
    <span class="error-message">• Couldn't match expected type ‘Python’</span>
                  <span class="error-message">with actual type ‘Name.Name &#45;&gt; Python’</span>
    <span class="error-suggestion">• Probable cause: ‘toIdentifier’ is applied to too few arguments</span>
      <span class="error-location">In the expression: toIdentifier prefix currentModule</span>
      <span class="error-location">In a case alternative:</span>
          <span class="error-location">Theta.Newtype' name _ &#45;&gt; toIdentifier prefix currentModule</span>
      <span class="error-location">In the expression:</span>
        <span class="error-location">case baseType of</span>
          <span class="error-location">Theta.Primitive' t &#45;&gt; primitive t</span>
          <span class="error-location">Theta.Fixed' _ &#45;&gt; "bytes"</span>
          <span class="error-location">Theta.Array' a</span>
            <span class="error-location">&#45;&gt; let items = ...</span>
               <span class="error-location">in</span>
                 <span class="error-location">((Theta.Target.LanguageQuoter.fromText @Python</span>
                     <span class="error-location">$$ Text.pack</span>
                         <span class="error-location">((Theta.Target.LanguageQuoter.indentBy 0)</span>
                            <span class="error-location">("List["</span>
                               <span class="error-location">&lt;&gt;</span>
                                 <span class="error-location">(Text.unpack (Theta.Target.LanguageQuoter.toText items)</span>
                                    <span class="error-location">&lt;&gt; ("]" &lt;&gt; ""))))))</span>
          <span class="error-location">Theta.Map' a</span>
            <span class="error-location">&#45;&gt; let values = ...</span>
               <span class="error-location">in</span>
                 <span class="error-location">((Theta.Target.LanguageQuoter.fromText @Python</span>
                     <span class="error-location">$$ Text.pack</span>
                         <span class="error-location">((Theta.Target.LanguageQuoter.indentBy 0)</span>
                            <span class="error-location">("Mapping[str, "</span>
                               <span class="error-location">&lt;&gt;</span>
                                 <span class="error-location">(Text.unpack (Theta.Target.LanguageQuoter.toText values)</span>
                                    <span class="error-location">&lt;&gt; ("]" &lt;&gt; ""))))))</span>
          <span class="error-location">Theta.Optional' a</span>
            <span class="error-location">&#45;&gt; let type_ = ...</span>
               <span class="error-location">in</span>
                 <span class="error-location">((Theta.Target.LanguageQuoter.fromText @Python</span>
                     <span class="error-location">$$ Text.pack</span>
                         <span class="error-location">((Theta.Target.LanguageQuoter.indentBy 0)</span>
                            <span class="error-location">("Optional["</span>
                               <span class="error-location">&lt;&gt;</span>
                                 <span class="error-location">(Text.unpack (Theta.Target.LanguageQuoter.toText type_)</span>
                                    <span class="error-location">&lt;&gt; ("]" &lt;&gt; ""))))))</span>
          <span class="error-location">Theta.Enum' name _ &#45;&gt; toIdentifier prefix currentModule name</span>
          <span class="error-location">Theta.Record' name _ &#45;&gt; toIdentifier prefix currentModule name</span>
          <span class="error-location">Theta.Variant' name _ &#45;&gt; toIdentifier prefix currentModule name</span>
          <span class="error-location">Theta.Newtype' name _ &#45;&gt; toIdentifier prefix currentModule</span>
          <span class="error-location">Theta.Reference' name &#45;&gt; toIdentifier prefix currentModule name</span>
    </code>
  </pre>

Once you cut through the noise, most Haskell type errors are pretty good. This example is pretty clear: I need to replace the `()` value with a `Theta.Type` value.

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
