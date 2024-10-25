---
title: Debugging Haskell Type Errors
author: Tikhon Jelvis
---

Fixing Haskell type errors can be *hard*. Learning how to understand and fix type errors was the first real obstacle I faced when I first picked up the language. I've seen the same tendency with every Haskell beginner I've taught.

With a bit of experience, I got used to the quirks of GHC's typechecker and Haskell's standard library so most errors became easy to resolve. Most *but not all*. Worse yet, the intuition that helped me in easier cases did not scale to harder errors; instead, fixing these hard errors took a frustrating amount of time, thinking and trial-and-error. I did not have a mental toolkit for debugging confusing type errors.

Haskell type errors are not unique in this regard—I had exactly the same experience with debugging in general. When I started programming all bugs were hard; I quickly built up an intuition for fixing *most* bugs; but I did not have the mental tools to deal with hard bugs, leaving me hitting my head against a metaphorical wall when I couldn't guess the cause of a bug up-front.

I was missing one key insight: **you can approach debugging systematically**. Debugging is a skill you can learn. Realizing this, my approach to debugging improved significantly: I slowed down, stopped making baseless assumptions and instead approached problems step-by-step, following some simple principles.

Just like general debugging, **we can approach Haskell type errors systematically**. Fixing type errors is a skill you can learn. Let's look at a simple framework for fixing type errors by following three principles:

 1. **Read the error**
 2. **Think in constraints**
 3. **Divide and conquer**

<!--more-->

</div>
<div class="content">

Systematic debugging is something I only learned almost a decade after I first started programming. In hindsight, it's a bit surprising—none of the tutorials, books, online discussions or college courses I took ever treated debugging as a concrete skill, much less taught any techniques or approaches. At the same time, debugging is easily one of the most important skills for any sort of programmer; everyone from the hobbyist to the academic to the professional spends at least as much time and effort debugging as they do writing code.

The first time I heard anyone talk about debugging as a systematic skill was during a learning session as part of an internship.[^js-talk] Shortly afterwards, somebody recommended a book[^nine-rules] which had nine “rules”—more like general principles—for debugging. By following these principles, I could solve problems step-by-step rather than thrashing around until I stumbled onto the solution.

We can approach Haskell type errors with the same mindset.

[^js-talk]: The first time I saw anybody talk about debugging *as a skill* was in a talk as part of Jane Street's internship program—unfortunately, more than a decade later, I don't remember exactly who gave the talk. At that point I had taken three years of CS courses at Berkeley and none of them ever touched on debugging like this; in hindsight, I would say this was the biggest missing piece in my CS education.

[^nine-rules]: [*Debugging: The 9 Indispensable Rules*][debugging-book] is, despite the click-baity titles, an amazing book for learning how to debug systematically. My approach for debugging in general and for fixing Haskell type errors in particular is heavily influenced by this book.

[debugging-book]: https://debuggingrules.com/?page_id=31

In this spirit, here are the three principles I use to deal with harder type errors:

  1. **Read the error**: when you load your code and see a bunch of red text, don't panic. Stop and read the errors—the error messages are the compiler's best attempt to tell you what's going on, and they're where we will start our debugging process.
  2. **Think in constraints**: Haskell's type system works like a set of constraints (and type *inference* works like constraint solving). When you see an error, read it as “here is an *inconsistency* in your code's type” and not “here is exactly where your code is *wrong*”.
  3. **Divide and conquer**: if the fix is not immediately clear from the error message, you either need to *change some other part of the code* or, at the very least, you need to *understand some other part of the code* to figure out a fix. Use the compiler, the types and the structure of the code to find which other parts are relevant.

Let's dive into each principle and see how to put them into action.

</div>

<div class="content">

## Read the error

First step: when you see an error, *read the error*.

If there are multiple errors, *read all of them*—or at least give them a skim. The first error you get is not necessarily the best error to start with.

This might sound obvious but, in practice, it isn't. Everyone I've mentored started out with a tendency to jump straight to their code as soon as they saw any errors. I've caught myself doing the same thing! Error messages are a bit intimidating; it feels like you've done something wrong. Wanting to fix it immediately is a natural impulse.

Instead of seeing errors as criticism, we should see them as *hints*—the compiler is trying to help us! There's an interesting UX challenge here: can we design an interface that makes errors seem helpful by default?

Unfortunately, Haskell error messages are not prefect—they can be hard to read and understand. Let's look through a few common issues and how to deal with them:

### Cutting through the noise

Haskell error messages get verbose *quickly*. Each error produces a lot of noise.

Haskell errors are verbose because they try to present all the information you'd need in a vacuum. Most error messages will have several parts giving distinct information, like:

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

More involved code can produce even more noise. A slightly different type error in the same `Python.hs` file, for example, produced *42 lines of localization information*[^42-lines]—none of which was useful because my editor highlighted the exact part of the code I needed to look at!

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

Once you cut through the noise, most Haskell type errors are pretty good. For example this—somewhat artificial—error is clear: I need to replace the `()` with a value of the type `Theta.Type`.

Of course, some errors will not be nearly as clear. Perhaps the message itself is confusing or there are several errors and it is not clear which one to start from. Other times, the error *attribution* is wrong: either the error is pointing to the wrong part of the code, or the error cause itself is misleading. (We'll talk more about attribution and localization in later sections.)

However, even in those cases, the error messages are still worth reading. The messages might not point us to a solution directly, but they still give us information. One of my personal debugging principles is to start debugging by getting all the information I can out of a system; for Haskell type errors, the error messages are the only starting information we get.

More importantly, error messages give us a valuable *starting point* letting us [divide and conquer](#divide-and-conquer) to find the real cause of the error.

### Multiple error messages

What should you do when you write some new code—or just make a single innocuous change—and see two screens of error messages?

**Don't panic**.

Remember that *Haskell error messages are verbose*; once you cut through the noise, those two screens of errors turn into a handful of distinct errors.

Instead of jumping into the first error in the list, take a step back and read *all* of the errors. The first error you see may not be the best starting point. Moreover, patterns in the errors can be a useful indicator for where the problem came from.

Often, several errors group together into a single “logical” error. For example, if we change the type of a function parameter, we'll get an error for every callsite. A slightly contrived example:

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
  |
<span class="error-heading">src/Example.hs:10:20: warning: [-Wdeferred-type-errors] …</span>
    <span class="error-message">• Couldn't match expected type ‘Integer’ with actual type ‘Int’</span>
    <span class="error-location">• In the second argument of ‘($$)’, namely ‘a - b’</span>
      <span class="error-location">In the expression: render $$ a - b</span>
      <span class="error-location">In an equation for ‘sub’: sub a b = render $$ a - b</span>
   |
</code>
</pre>

Real-world code is not going to be quite this clean; in one of my projects, changing a function from taking a `Maybe AST` value to an `AST` value resulted in 16 type errors with a couple of variations on the actual error message—all stemming from a single change to a type signature!

Was the mistake in the change to the function's type signature, or was the change intentional and now all the call sites need fixing? 

The compiler fundamentally has now way to know that without reading your mind. 

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
<hr>
</code>
</pre>

The three error messages—with all their text—were definitely a bit intimidating, but I gave them a quick scan and noticed that they were all pointing to roughly the same part of my code, a hint that they share the same underlying cause.

In this case, it turned out that the first error *was* the one to start with. Reading the error message carefully, we can see that `Couldn't match type ‘m’ with ‘(->) (Theta.Definition Theta.Type)` is actually telling us that we are missing an argument—which becomes much clearer if you read the next two lines in the error:

<pre class="error">
<code>
      <span class="error-message">Expected: Name.ModuleName -> m (m0 Python)</span>
        <span class="error-message">Actual: Name.ModuleName</span>
                <span class="error-message">-> Theta.Definition Theta.Type -> m0 Python</span>
</code>
</pre>

(Another quick heuristic: the `Expected:` and `Actual:` lines are often more useful than the “actual” error message.)

</div>

<div class="content">

## Think in Constraints

What aspect of Haskell's type system leads to confusing type errors?

<!-- Some old footnotes, may or may not fit into this sections -->
[^type-error-localization]: Type error localization in Haskell (and similar languages) is [an active area of research][localization-research] as is [the quality of compiler error messages more broadly][error-message-research]. David Binder pointed this research out to me [on Discourse][david-binder-discourse-post], including additional links and context.

[localization-research]: https://dl.acm.org/doi/10.1145/3138818

[error-message-research]: https://dl.acm.org/doi/10.1145/3344429.3372508

[david-binder-discourse-post]: https://discourse.haskell.org/t/examples-of-haskell-type-errors/10468/9

</div>

<div class="content">

## Divide and Conquer

</div>

<div class="content">

(TODO: Conclusion goes here...)
