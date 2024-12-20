---
title: Debugging Haskell Type Errors
author: Tikhon Jelvis
published: 2024-11-01 17:58:09
modified: 2024-11-02 17:26:14
---

Fixing Haskell type errors can be *hard*. Learning how to understand and fix type errors was the first real obstacle I faced when I first picked up the language. I've seen the same tendency with every Haskell beginner I've taught.

With a bit of experience, I got so used to the quirks of GHC's typechecker and Haskell's standard library that I could resolve most type errors intuitively. Most *but not all*. Worse yet, the intuition that helped me in easier cases did not scale to harder errors; instead, fixing hard errors required frustrating trial-and-error. I did not have a mental toolkit for debugging confusing type errors.

![An intimidating block of error messages for a single mistake!](many-type-errors.png "A screenshot of some Haskell code side-by-side with an intimidating screenful of bright orange error messages."){.no-border}

At the same time, I was going through the same story with debugging in general. When I started programming all bugs were hard; gradually, I developed an intuition for fixing *most* bugs; but I did not have the mental tools to deal with hard bugs, leaving me thrashing around when my initial assumptions about a bug were wrong.

I was missing one key insight: **you can debug systematically**. Debugging is a skill you can learn—not just glorified guess-and-check. Realizing this, my approach to debugging improved significantly. I slowed down, stopped jumping in based on my initial assumptions and instead approached problems step-by-step, following some simple principles.

This insight translated to Haskell. **We can fix Haskell type errors systematically.** It's a skill you can learn.

Let's look at a simple framework for fixing type errors by following three principles:

 1. **Read the error**
 2. **Think in constraints**
 3. **Divide and conquer**

<!--more-->

</div>
<div class="content">

Systematic debugging is something I only learned almost a decade after I first started programming. In hindsight, it's a bit surprising—none of the tutorials, books, online discussions or college courses I took ever treated debugging as a concrete skill and never covered any specific debugging techniques or principles.

At the same time, debugging is easily one of the most important skills for any sort of programmer; everyone from the hobbyist to the academic to the professional spends at least as much time and effort debugging as they do writing code.

The first time I heard anyone talk about debugging as a systematic skill was in a lecture during an internship.[^js-talk] Shortly afterwards, somebody recommended a book[^nine-rules] which had nine “rules”—really rules-of-thumb or general principles—for debugging. By following these principles, I could solve problems step-by-step rather than thrashing around until I stumbled onto the solution.

We can approach Haskell type errors with the same mindset, with some variations on general debugging principles tailored to Haskell's style of type system specifically. (The principles apply just as well to other [Hindley-Milner][hm] languages like OCaml.)

[^js-talk]: The first time I saw anybody talk about debugging *as a skill* was in a talk as part of Jane Street's internship program—unfortunately, more than a decade later, I don't remember exactly who gave the talk. At that point I had taken three years of CS courses at Berkeley and none of them ever touched on debugging like this; in hindsight, I would say this was the biggest missing piece in my CS education.

[^nine-rules]: [*Debugging: The 9 Indispensable Rules*][debugging-book] is, despite the click-baity title, an amazing book for learning how to debug systematically. My approach for debugging in general and for fixing Haskell type errors in particular is heavily influenced by this book.

[debugging-book]: https://debuggingrules.com/?page_id=31

Here are three principles I use to deal with harder type errors:

  1. **Read the error**: when you load your code and see a bunch of red text, don't panic. Stop and read the errors—the error messages are the compiler's best attempt to tell you what's going on, and they're where we will start our debugging process.
  2. **Think in constraints**: Haskell's type system works like a set of constraints (and type *inference* works like constraint solving). When you see an error, read it as “here is an *inconsistency* in your code's types” and not “here is exactly where your code is *wrong*”.
  3. **Divide and conquer**: if the fix is not immediately clear from the error message, you need to *understand other parts of the code* to figure out a fix. Use the compiler, the types and the structure of the code to find which other parts are relevant.

Let's dive into each principle and see how to put them into action.

</div>

<div class="content">

## Read the Error

First step: when you see an error, **read the error**.

If there are multiple errors, **read all of them**. The first error you get is not necessarily the best starting point.

This might sound obvious but, in practice, it isn't. Everyone I've mentored started out with a tendency to jump straight to their code as soon as they saw an error. I've caught myself doing the same thing! Error messages are a bit intimidating; it feels like you've done something wrong. Wanting to fix the error immediately is a natural impulse.

As you get a bit more experience, you'll learn to quickly recognize the most common types of errors you'll encounter. Some errors are clear right away; others are confusing, but understandable once you learn the pattern[^error-patterns-post]. And then there's the minority of errors that point your in the wrong direction or are plain *weird*; it's these final errors where slowing down and proceeding systematically is the most important.

[^error-patterns-post]: I recently asked the community [for examples of confusing error messages][discourse-error-messages] and got a ton of great examples. There were too many good examples to include in this post—which is already a bit too long—so I'm planning to write a follow-up post focused just on common patterns of confusing type errors.

[discourse-error-messages]: https://discourse.haskell.org/t/examples-of-haskell-type-errors/10468

### Cutting Through the Noise

Haskell error messages get verbose *fast*. Each error produces a lot of noise.

Haskell errors are verbose because they try to present all the information you'd need in a vacuum. Most error messages will have several parts giving distinct information, like:

 * The error itself.
 * Additional context about the error.
 * Steps to identify which part of the code caused the error.
 * Suggestions for how to fix the error.

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

 1. The first line tells us the location[^file-locations] as well as the type of error (a deferred type error warning).[^deferred-errors]
 2. The second line is the actual error message.
 3. The third, fourth, fifth *and* sixth lines all tell us *where* the error is in our code.

Even the simplest type error leads to six lines of error text. It only takes a handful of errors like this to fill an entire screen!

If you're using an editor that highlights errors in place, none of the location information—4½ out of 6 lines!—matters. The only information we need is:

  1. It's a type error.
  2. We expected a `Theta.Type`  value but got `()`.

So: the first trick to reading Haskell type errors is to mentally filter out the bits that don't matter—often *most* of the message![^context-flag]

[^context-flag]: On GHC 9.8 and later, the noisy context information can be disabled with the `-fno-show-error-context` flag. In `ghci` you can enable this flag with `:set`:

    ``` ghci
    λ> :set -fno-show-error-context
    ```

More involved code produces even more noise. A slightly different type error in the same [`Python.hs` file][theta-python-hs], for example, produced *42 lines of localization information*[^42-lines]—none of which was useful because my editor highlighted the exact part of the code I needed to look at!

[theta-python-hs]: https://github.com/TikhonJelvis/theta-idl/blob/stage/theta/src/Theta/Target/Python.hs

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

Once you cut through the noise, most Haskell type errors are reasonably clear. For example the message for this (somewhat contrived) error is clear: I need to replace the `()` with a value of the type `Theta.Type`. Even the error with 42 lines of noise had the correct suggestion that I was missing an argument to a function.

However, some errors will not be nearly as clear. Perhaps the message itself is confusing or there are several errors and it is not clear which one to start from. Other times, the error *attribution* is wrong: either the error is pointing to the wrong part of the code, or the type of error itself is misleading. (We'll talk more about attribution and localization in later sections.)

Even in those cases, the error messages are still worth reading. A message might not point us to a solution directly but it still give us information. One of my personal debugging principles is to start debugging by getting all the information I can out of a system before doing anything else; for Haskell type errors, the error messages are the information we start with.

Error messages will be our *starting points* for understanding what's going on and starting our [divide and conquer](#divide-and-conquer) process to find the real cause of the error.

### Multiple Error Messages

What should you do when you write some new code—or just make a single innocuous change—and see two screens of error messages?

**Don't panic**.

Remember that *Haskell error messages are verbose*; once you cut through the noise, those two screens of errors reduce to a handful of distinct errors.

Instead of jumping into the first error in the list, take a step back and read *all* of the errors. The first error you see may not be the best starting point. Moreover, patterns in the errors can be a useful indicator for diagnosing the underlying problem.

Multiple errors often group into a single “logical” error. For example, if we change the type of a function parameter, we'll get an error for every call site. A slightly contrived example:

``` haskell
render :: Int -> String
render x = show x

add :: Int -> Int -> String
add a b = render $$ a + b

sub :: Int -> Int -> String
sub a b = render $$ a - b
```

If we change the type signature of `render` to `render :: Integer -> String` we will get *two* errors for that *one* change:

<pre class="error">
<code>
<span class="error-heading">src/Example.hs:7:20: warning: [-Wdeferred-type-errors] …</span>
    <span class="error-message">• Couldn't match expected type ‘Integer’ with actual type ‘Int’</span>
    <span class="error-location">• In the second argument of ‘($$)’, namely ‘a + b’</span>
      <span class="error-location">In the expression: render $$ a + b</span>
      <span class="error-location">In an equation for ‘add’: add a b = render $$ a + b</span>
<hr>
<span class="error-heading">src/Example.hs:10:20: warning: [-Wdeferred-type-errors] …</span>
    <span class="error-message">• Couldn't match expected type ‘Integer’ with actual type ‘Int’</span>
    <span class="error-location">• In the second argument of ‘($$)’, namely ‘a - b’</span>
      <span class="error-location">In the expression: render $$ a - b</span>
      <span class="error-location">In an equation for ‘sub’: sub a b = render $$ a - b</span>
<hr></code></pre>

These two errors group into a single logical error: the type for the argument we need to pass to `render` has changed.

Real-world code is not going to be quite this clean; in one of my projects, changing a function from taking a `Maybe AST` value to an `AST` value resulted in 16 type errors with a couple of variations on the actual error message—all stemming from a single change to a type signature!

Was the mistake in the change to the function's type signature, or was the change intentional and now all the call sites need fixing? 

The compiler fundamentally has no way to know without reading your mind.

In lieu of mind-reading, the compiler treats type signatures as sources of truth and gives you a ton of errors. When you're making the change intentionally this is actively useful: you get a checklist of every location in your program that you need to update. But if the change to the function was a typo, it's a bit confusing—you get a ton of errors and none point to the actual mistake—so you have to read all the errors and notice the pattern in order to diagnose and fix the actual problem.

A similar pattern to watch out for is when a single change leads to several different errors pointing to the same place. I ran into this with some of my own code recently, which had the following call to `mapM`—don't worry about the details:

``` haskell
toModule Theta.Module {..} prefix = do
  definitions <- mapM (toDefinition prefix moduleName) types
  ...
```

What would happen if I left out the `moduleName` argument in `toDefinition`? 

``` haskell
toModule Theta.Module {..} prefix = do
  definitions <- mapM (toDefinition prefix) types
  ...
```

Because Haskell functions are curried by default, `toDefinition prefix` would still be a function, but it would not match the type `mapM` expected. However, instead of getting an error that pointed out the missing argument directly, I got several type errors instead (noisy output skipped up for readability):

<pre class="error">
<code>
<span class="error-heading">src/Theta/Target/Python.hs:64:24: warning: [-Wdeferred-type-errors] …</span>
    <span class="error-message">• Couldn't match type ‘m’ with ‘(->) (Theta.Definition Theta.Type)’</span>
      <span class="error-message">Expected: Name.ModuleName -> m (m0 Python)</span>
        <span class="error-message">Actual: Name.ModuleName</span>
                <span class="error-message">-> Theta.Definition Theta.Type -> m0 Python</span>
    <span class="error-location">...</span>
<hr>
<span class="error-heading">src/Theta/Target/Python.hs:64:45: warning: [-Wdeferred-type-errors] …</span>
    <span class="error-message">• Couldn't match type ‘Theta.Definition Theta.Type’</span>
                     <span class="error-message">with ‘Name.ModuleName’</span>
      <span class="error-message">Expected: Data.Map.Internal.Map Name.Name Name.ModuleName</span>
        <span class="error-message">Actual: Data.Map.Internal.Map</span>
                  <span class="error-message">Name.Name (Theta.Definition Theta.Type)</span>
    <span class="error-location">...</span>
<hr>
<span class="error-heading">src/Theta/Target/Python.hs:65:41: warning: [-Wdeferred-type-errors] …</span>
    <span class="error-message">• Couldn't match type ‘m0 Python’ with ‘Python’</span>
      <span class="error-message">Expected: [Python]</span>
        <span class="error-message">Actual: [m0 Python]</span>
    <span class="error-location">...</span>
<hr></code></pre>

The three error messages—with all their text—were a bit intimidating, but I gave them a quick scan and noticed that they were all pointing to roughly the same part of my code, a hint that they share the same underlying cause.

In this case, it turned out that the first error message *was* the best one to start with. But if the code had been written in a slightly different order (say the call to `mapM` was in a `where` clause—we could have gotten exactly the same errors in a different order and the best starting point could have been the second or third error instead.

Reading the first error message carefully, we can see that `Couldn't match type ‘m’ with ‘(->) (Theta.Definition Theta.Type)` is telling us that we are missing an argument—which becomes much clearer if you read the next two lines in the error:

<pre class="error">
<code>
      <span class="error-message">Expected: Name.ModuleName -> m (m0 Python)</span>
        <span class="error-message">Actual: Name.ModuleName</span>
                <span class="error-message">-> Theta.Definition Theta.Type -> m0 Python</span>
</code>
</pre>

`Expected` and `Actual` types are often more useful than the top-level error message.

</div>

<div class="content">

## Think in Constraints

While Haskell's error *messages* can be confusing, I've found that **error attribution** is a larger problem. It doesn't matter how well-written and well-formatted your error messages are if the error is pointing in the wrong place!

Some level of error misattribution is inevitable. The core problem is that types can't tell us that some code is *right* or *wrong*; **types can only point out inconsistencies**.

You always have multiple parts of your code that you can change to fix a type error: if you pass an invalid argument to a function, you can change the argument, change the argument's type, change the function's definition or use a different function altogether. Or maybe it's a sign you need an even larger refactoring!

Which change is “correct” *depends on your intentions*. The compiler cannot read your mind and does not know anything about the world outside your code, so it cannot know what your code is *supposed* to do.

This is fundamentally true for all languages but it's exacerbated in Haskell because Haskell's type system is so flexible and expressive, and because Haskell has *global type inference* à la [Hindley Milner][hm].

To understand what Haskell's type errors indicate about our code and how to compensate for confusing error localization, we need to understand how Haskell's types act like constraints and how Haskell's type inference and type checking act as constraint resolution.

### Haskell Types as Constraints

How does Haskell determine what type an expression should have?

A good mental model is that Haskell starts by treating an expression or variable as able to have *any* type (`x :: a`) then looks through the code for anything that would force (*constrain*) the expression to have a more specific type.

A constraint could be:

  - an explicit type signature: if Haskell sees `x :: Int` in the code, it will proceed with the assumption that `x` has type `Int`
  - using `x` in a context that restricts its type; if Haskell sees `x && False` in the code, it will proceed with the requirement that `x` has type `Bool`
  - an implicit constraint that lets `x` be polymorphic: if Haskell sees `x + 1`, it will assume `x` has the type `Num a => a`
  
Ideally, all the constraints are consistent. If `x` has the type `Int` everywhere in your code, everything is good. Alternatively, if `x` is constrained to `Int` at one point and `Num a => a` at another, things are still good. `Int` is an instance of `Num` so the two signatures are compatible and `x` has the more specific of the two types (`Int`).

A type error is what we get when these constraints are *not* consistent. For example:

 1. We see `x + 1` on line 10, constraining `x` to `Num a => a`
 2. We see `x && y` on line 20, constraining `x` to `Bool`
 3. `Bool` is not an instance of `Num`, so these two types are incompatible
 
So now we need to generate a type error. Should the error point to line 10 or line 20?

There's no real way to know. Perhaps you meant to write `x' + 1` at line 10. Perhaps you meant to write `even x && y` on line 20, or maybe `x + y'`. Or maybe you meant to define a `Num` instance for `Bool`![^num-instance-for-bool]

All that the compiler knows is that you have to change *some* part of the code in order to make the types consistent. There are multiple places you could change, but an error message can only point to *one*, so the compiler has to choose somehow. The way real-world compilers choose where to point an error is more-or-less arbitrary, an implementation detail of the typechecking algorithm maybe coupled with some rough heuristics. This *ad hoc* approach works surprisingly well in practice but it isn't—fundamentally can't be—perfect.[^type-error-localization]

So when you encounter a type error pointing to a line of code that seems totally correct, don't panic! There's a good chance that the problem is in some other line of code and the typechecker chose the “wrong” line.

Understanding Haskell's types as constraints will help us to track down the actual source of the error. As we [divide and conquer](#divide-and-conquer) the codebase, the candidates in our code will be *the lines that introduce the constraints that led to the type error we are fixing*.

[^num-instance-for-bool]: A `Num` instance for `Bool` would require an [orphan instance][orphans] and would be an awful idea in practice, but it *would* be valid Haskell, and, hey, it even makes sense conceptually: if we have 8/16/etc-bit integers as `Num` instances, why *not* make `Bool` a 1-bit integer?

    That would be bad from a UX point of view—treating a `Bool` value as a number is almost definitely a programming mistake, and if it's intentional you can use the [`fromEnum`][fromEnum] function to make it explicit—but it would be conceptually coherent. 
                                                                                                                                                                      
[fromEnum]: https://hackage.haskell.org/package/base/docs/Prelude.html#v:fromEnum

[orphans]: https://wiki.haskell.org/Orphan_instance

[hm]: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system

### Type Signatures as Assertions

An important aspect of Haskell's type checking and type inference is that type signatures act like *assertions*. That is, when Haskell sees `x :: Int`, it will take this *as given* for the rest of the code. This is true even if `x` is defined to be something that can't be an `Int`.

If we load the following code, we'll get two type errors:

``` haskell
x = False

y = x + 1

z = 2 * x + y
```

```
src/Theta/Misc.hs:3:7: warning: [-Wdeferred-type-errors] …
    • No instance for (Num Bool) arising from a use of ‘+’
    • In the expression: x + 1
      In an equation for ‘y’: y = x + 1
  |
src/Theta/Misc.hs:5:11: warning: [-Wdeferred-type-errors] …
    • No instance for (Num Bool) arising from a use of ‘+’
    • In the expression: 2 * x + y
      In an equation for ‘z’: z = 2 * x + y
  |
```

(Side note: that second error is a great example of arbitrary attribution: why does it point to `+` and not `*` as the reason we need a `Num` instance? Either choice would have been totally valid!)

Now let's add a type signature to `x`:

``` haskell
x :: Int
x = False

y = x + 1

z = 2 * x + y
```

With this type signature, we've *asserted* that `x` has the type `Int`. Now Haskell will treat `x` as an `Int` everywhere in the code even though `x` is defined as `False`. We will only get a single error for the definition itself, but no errors for `y` or `z`:

```
src/Theta/Misc.hs:2:5: warning: [-Wdeferred-type-errors] …
    • Couldn't match expected type ‘Int’ with actual type ‘Bool’
    • In the expression: False
      In an equation for ‘x’: x = False
  |
```

Type signatures are Haskell's way of letting us explicitly specify our intentions. By telling the compiler that `x :: Int`, it knows that `y` and `z` are fine, but that the definition `x = False` is inconsistent. The code is still semantically the same, but we get a more pointed error message.

Type signatures can also constrain the type of an expression *more* than it would be otherwise. A definition `x = []` will have the type `x :: [a]`, but if we add an explicit signature like `x :: [Int]`, the code will compile with the more specific type. Just like the previous example this can give you more specific type error messages, as well as avoiding weird edge cases like the [monomorphism restriction][monomorphism-restriction].

[monomorphism-restriction]: https://wiki.haskell.org/Monomorphism_restriction

Type signatures in Haskell are—mostly—optional. You can write entire Haskell programs without annotating any types yourself, relying entirely on type inference. In practice, however, including top-level type signatures *is a really good idea* because it communicates your intent to both the compiler and to anybody else reading your code. You will consistently get clearer, better-attributed type errors if you write explicit type signatures.

The more types you specify as type signatures, the more specific your type errors will be—but don't forget that the type signature itself can be wrong! Some of the trickier type errors I've had to solve boiled down to a mistake in a type signature rather than a mistake in an expression.

### More Signatures, More Better

Type signatures let us isolate parts of our code for the typechecker, giving us better error messages and localization. This gives us a technique for debugging confusing type errors: **add more type signatures**.

Apart from top-level definitions, you can also add explicit signatures for:

  - `let` and `where` definitions
  - variables bound in do-notation
  - pattern-match variables
  - arbitrary sub-expressions (`x + (y * 10 :: Double)`)
  - typeclass implementations
  
Adding a type signature is a way to assert something you believe about your code's types. Maybe you're right about the type, maybe you're wrong, but the type signature will help in either case:

  - if your type signature is wrong, you'll get a new type error from it and you will have learned something new about your code
  - if your type signature is right, you'll give the typechecker more information to provide clearer, better-localized errors
  
I've cleared up numerous confusing type errors in real-world code by adding a type signature or two to helper functions defined in `where` clauses. Sometimes I even pull out sub-expressions into a `where` or `let` clause just to add a type signature—while you *can* add type signatures directly inside expressions, code often reads much better with those subexpressions pulled out into their own definitions.

There's nothing wrong with leaving type signatures you added to debug a type error after you're done fixing your code. If the type signature helped once, it will likely help again; and, regardless, the explicit type signature will help anybody reading the code in the future.

<!-- Some old footnotes, may or may not fit into this sections -->

[^type-error-localization]: Type error localization is [an active area of research][localization-research] as is [the quality of compiler error messages more broadly][error-message-research]. 
David Binder pointed this research out to me [on Discourse][david-binder-discourse-post], including additional links and context.

    Some of the research approaches are promising and seem to work well in practice, but have heavyweight dependencies: for example, [one promising approach][type-error-localization-smt] requires solving a MaxSMT problem to find the "best" error location. That works well, but do we really want our compiler to depend on an SMT solver with cutting-edge capabilities just for better error messages?

[localization-research]: https://dl.acm.org/doi/10.1145/3138818

[error-message-research]: https://dl.acm.org/doi/10.1145/3344429.3372508

[david-binder-discourse-post]: https://discourse.haskell.org/t/examples-of-haskell-type-errors/10468/9

[type-error-localization-smt]: https://cs.nyu.edu/~wies/publ/practical_smt-based_type_error_localization.pdf

</div>

<div class="content">

## Divide and Conquer

So: you've read your error messages, you've added some type signatures, but you still can't find what's causing the type error. The code highlighted by the error looks fine and it's not clear what's actually wrong.

What do we do?

We need to find which other part of the code is incorrectly causing our types to be inconsistent. We could try jumping around the code based purely on intuition, but it's easy to go in the completely wrong direction if your initial guesses aren't right. Alternatively, we could try reading our code from start to end—does code even have a start and an end?—but that would take a lot of work!

Instead of jumping around in an *ad hoc* way or doing a linear scan of our code, we can borrow an idea from the world of algorithms and find the problem through **divide and conquer**.

Remember that a type error corresponds to an *inconsistency* between type constraints in your codebase. An inconsistency is not a single point that is wrong; rather, it is composed of multiple components that are incompatible and we can search through them separately.

### Violated Expectations

A type error highlights a specific expression gives us the two incompatible sides:

  - The **actual** type the expression has.
  - The type that the context of the expression **expected**.
  
We don't know which one is “wrong”, we just know that they do not match.

Some type errors explicitly list the “expected” and “actual” sides, like we saw in an earlier example:

```
src/Theta/Target/Python.hs:65:41: warning: [-Wdeferred-type-errors] …
    • Couldn't match type ‘m0 Python’ with ‘Python’
      Expected: [Python]
        Actual: [m0 Python]
    ...
```

Other errors leave us to reason out the two sides from the error message, as we saw in a different example:

```
src/Theta/Misc.hs:5:11: warning: [-Wdeferred-type-errors] …
    • No instance for (Num Bool) arising from a use of ‘+’
    ...
```

The literal text of this error tells us that `Bool` does not have a `Num` instance—but that's fine, `Bool` should really *not* have a `Num` instance! Booleans aren't numbers.

Instead, we should read this message as:

```
Expected: an instance of Num
  Actual: Bool
```

As you see different kinds of type error messages, it's worth learning how to translate all of them into this format. Writing out the two sides explicitly when you first see an error can help.

### Searching through the Code

The two sides of a type error give us the perfect starting point for dividing our problem into two halves:

  1. Why does the compiler believe our expression has its *actual* type?
  2. Why does the compiler believe the surrounding context *expected* the type it did?
  
Often, we will have a good idea of which side to look at: either the actual or the expected type are “obviously” correct. (That said, always be wary of anything that seems “obvious”—believing the wrong thing to be obvious is the easiest way to go off on a wild goose chase!)

Even if neither side is clearly right, we've still made progress by splitting our big problem (“why are we getting this type error?”) into two smaller problems.

The next step is to take one of these sides and *figure out what constraints led to that particular type*.

One way to do this is by reading the code and reasoning through the types in your head—a pain at first but manageable with a bit of experience. You only have to reason about types, not about what the code actually does: static types are a *syntactic property* of the program, so they can only depend on the code and not on runtime behavior or state.

We also have a few tools that can help us figure out what's going on with our types:

  - An IDE or haskell-language-server can tell you the type inferred for a specific identifier or expression in your code.
  - You can replace parts of your code with [typed holes][typed-holes] to see what types are inferred for those parts.
  - `ghci` has several commands for inspecting types:
    - the [`:t` command][ghci-t] gives you the type of an expression
    - the [`:i` command][ghci-i] gives you information about an identifier, including all the typeclass instances for a type or all the implementing types for a typeclass
    - the [`:k` command][ghci-k] will tell you the [kind] of a type and can simplify type expressions (with `:k!`)

[ghci-t]: https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#ghci-cmd-:type
[ghci-i]: https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#ghci-cmd-:info
[ghci-k]: https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#ghci-cmd-:kind

[typed-holes]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/typed_holes.html
[kind]: https://wiki.haskell.org/Kind

    
If we have a good idea of what parts of the code constrain the expression that led to our type error, but that is not enough to *resolve* the type error, we can continue the search in the same way: figure out which parts of the code constraint the parts we're currently looking at. We're searching through the type dependencies of our code like a graph.

Of course, this graph of type dependencies can get *big*. Searching through it effectively will always require some intuition about what could reasonably cause the errors we're seeing. 

Writing **additional type signatures** is a powerful tool for managing this large search space. By asserting types with type signatures, we can fence off the parts of the code we've looked at from the parts we're still investigating, directing where the type checker looks. (More realistically, I often add type signatures simply because [more signatures is more better](#more-signatures-more-better) rather than based on any sort of sophisticated tactical reasoning!)

My advice here is to try to search more-or-less systematically and to think of types in terms of constraints, but not to overthink beyond that. At first this will sometimes take a lot of effort, but this gets much easier with experience: experience with Haskell in general, with GHC in particular and even with the libraries and abstractions you're using.

</div>
<div class="content">

Haskell type errors can be *hard*. Haskell has the reputation for bad type error messages but, while the messages *do* have issues, a more common problem is *bad error attribution*: type errors do not always give the “right” reason for the problem or point to the “right” part of the code.

While getting comfortable fixing Haskell type errors will only come with experience and practice, we can start by approaching the problem systematically. This both gives you a foundation for *learning* how to solve type errors as well as helping you deal with trickier errors even once you have more experience.

When you see an error, you can follow three principles to deal with it:

  - **read the error**—the error is your best starting point (and sometimes reading the error explains the error!)
  - **think in constraints**—it's not about “right” and “wrong”, it's about two sides being incompatible
  - **divide and conquer**—why do we have the type we have? why do we need the type we need?
  
At first, all of these principles will take conscious effort to apply. But with a bit of experience, it becomes a habit—a habit that will save you a lot of time and frustration, and a habit I wish I had developed earlier myself!
