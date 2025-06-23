---
title: Writing Code To Be Read at a Glance
author: Tikhon Jelvis
---

In software engineering circles, there is a common adage: "code is read more than it is written". But this is not the whole picture! **Code is skimmed more than it is read.**

We read code more than we write it because we spend more time maintaining than we do writing from scratch. A line of code, once written, still has a long and storied life ahead of it. You write code once and then return to it multiple times, fixing bugs, adding features, refactoring. To do this, of course, you have to change the existing code—and to change it you have to read and understand it. And not just you; future programmers will also work on the code, pursuing their own goals, operating under different constraints.

But for every part of the code you need to revisit to in depth, there will be dozens of related parts you're not touching directly. You need to navigate through the codebase to find the relevant code for your work, and you need to track the code *surrounding* your work for context. You don't have to understand the related parts of code *fully*; you're content understanding *what they are supposed to do*. You do this not by reading the code in detail—nobody has the time or the working memory to keep the whole codebase in detail in their head!—but by scanning through the code quickly and getting just the *gist* of the code.

This is a multiplicative relationship. Just as you end up reading code multiple times for each time you write it, you end up skimming multiple times for each piece you read. **Writing code you can understand *at a glance* is at least as important as writing code that you can read at all.**

<!--more-->

</div>
<div class="content">

## Shape

What does writing code that's easy to read at a glance actually involve? To me, it comes down to thinking about the **"shape"** of my code. The structure of the code should give you a quick idea of what it's supposed to do.

An immediate consequence of caring out the shape of your code is that related code *looks related* and unrelated code *looks unrelated*—regardless of implementation. How your code looks is a key affordance for guiding the reader's attention without forcing them to read the code in detail.

### Verbosity

Verbose identifiers obscure the shape of your code. This doesn't mean you should *never* use verbose names in your code, just that you should be restrained and tasteful. If an identifier comes from far away—a function from a logically distant module, say—giving it a descriptive name can outweigh the downsides.

As an illustrative—if extreme—example, let's compare three versions of the same logic. Here's some Java code using [BigDecimal]:

``` java
final BigDecimal result = a.multiply(x.pow(2)).plus(b.multiply(x.plus(c)));
```
[BigDecimal]: https://docs.oracle.com/javase/8/docs/api/java/math/BigDecimal.html

Java does not have operator overloading. If it did[^java-operator-overloading], the code might look like this instead:

``` java
final BigDecimal result = a * (x * x) + b * x + c;
```

[^java-operator-overloading]: Example from [Is it time for operator overloading in Java?](https://blogs.oracle.com/javamagazine/post/is-it-time-for-operator-overloading-in-java) in
Java Magazine

Both of these implement the same mathematical formula:

$$
\text{result} = ax^2 + bx + c
$$

This is about as simple as a polynomial gets, but even that wasn't immediately clear from the first version of the code: you'd have to pay attention to read the sequence of named method calls to understand what was going on. The version with overloaded operators is a real contrast, although it still requires more attention than the math notation. If the first snippet is like reading a paragraph, the second snippet is like reading a short sentence and the math notation is like reading a single word.

With the math notation, we can *instantly* tell we're looking at a polynomial just from the shape. The information we care about for polynomials are the terms; the addition and multiplication holding everything together is more like an implementation detail. The math notation reflects this by visually grouping together the information for each term and using an operator (+) as "punctuation" to pull it together. Addition is part of what makes the polynomial *a polynomial* but, once we know what we're looking at, it can fade into the background.

### Context

People are naturally good at tracking context. We can see this with natural language all the time: the same word or phrase can have somewhat different—or sometimes radically different—meanings in different contexts, but this is so natural that, once people are used to it, they barely notice. What was the last time you thought about how red wine is actually purple, or that, for programmers, "strings" and "threads" have absolutely nothing to do with each other?

Context sensitive meanings aren't free. The way words in natural languages have different meanings ([polysemy]) is a real obstacle for early language acquisition. But part of the reason that polysemy is difficult is that *native speakers do not even notice they are relying on it*! Once you get comfortable in a language—whether a totally different language, or just the jargon and conventions of a new social group—tracking the meaning of words based on context becomes *free*. The reason natural languages have polysemy is that relying on context takes *less* mental energy than communicating in verbose, fully-explicit phrases.

[polysemy]: https://en.wikipedia.org/wiki/Polysemy

We can take advantage of this natural tendency in programming. If I'm working on a module that is implementing an HTTP client for the Stripe, I'm going to be fine using `get` to mean "send an authenticated HTTP GET request to Stripe". But in a broader, less Stripe-specific context, I would want to write something like `Stripe.get` instead[^qualified-imports].

[^qualified-imports]: This is one of the reasons I really like qualified imports in languages like Haskell. My rule of thumb: if I catch myself using the same suffix or prefix on a bunch of identifiers (`getHttps`, `postHttps`... etc), I probably want to group them into a module that can be imported qualified instead. In the module they'll just be `get` and `post`, but users can import them as `Https.get` and `Https.post` if that makes more sense *in their code's context*.

### Plumbing

Some of your code is the "meat" of your expression, the logic that matters for whatever you are doing. The rest is more like plumbing—code that we need to keep everything working but less significant in any *particular* instance. Think of type conversions, control flow, error propagation, configuration management... Sometimes fixing a bug will hinge on how a specific config value flows into your function but, most of the time, you care far more about *what the function does*.

Quickly distinguishing plumbing code from logic is key for understanding and navigating code quickly. When you're scanning through a codebase, you can ignore plumbing code altogether. I've found this is where certain "controversial" language features like macros, overloaded operators and control-flow abstractions.[^control-flow-abstractions]

[^control-flow-abstractions]: Control flow abstractions like monads, continuations and algebraic effects let us abstract over and deemphasize a lot of plumbing code. People find these abstractions hard to learn up-front—perhaps because they are so *abstract*—but they're worth the effort both as a way to manage plumbing overhead and for their high power-to-weight ratios.

As an example, using infix operators for plumbing constructs makes the plumbing visually distinct from "normal" identifiers while also giving your code some additional visual structure by naturally organizing the expression up into groups.

After a while, plumbing operators start to fade into the background when you're scanning through code. Squint a bit, and you start seeing similarities between code that might be doing the same thing in different contexts. Consider the `Applicative` operators in Haskell: they let us apply functions over values in some functor (like `Maybe` and `IO`) in a way that's immediately reminiscent of normal function application:

```haskell
f a b c             -- normal function application
f <$$> a <*> b <*> c -- over an Applicative
```

This style also lets us see how operators naturally group code together:

```haskell
f (a + b) (c * d)
f <$$> a + b <*> c * d
```

Applicative notation might read like line noise to the uninitiated—and, honestly, it's not exactly the best example of clean plumbing code—but, once you're comfortable in Haskell, it just melts away. (Which is also 100% true of Lisp's parentheses! Write enough Lisp and you stop seeing them. It is uncannily like the "I don't see code" scene from *The Matrix*.)

### Information Content

One perspective I've found useful is to think about the minimum amount of information an expression *must* contain. A polynomial in a single variable, for example, only really needs its coefficients. A web route needs the route, the methods it supports, any variables it takes and the variables' types.

Anything else is unnecessary from a raw information point of view. We might still *need* additional code—for plumbing, for structure or just as an implementation detail—but, for thinking about API design, we want to be able to distinguish the core information the code is conveying from everything else. "Everything else" may be useful for organizing our code, but it might also be nothing more than unavoidable—or, depressingly often, completely unforced—boilerplate. 

**Boilerplate definitely makes code harder to read at a glance.**

As a general guide, I try to eliminate more and more of the inessential code as I repeat a particular kind of expression more and more. In extreme cases, a table layout might be the most readable option if you have a whole bunch of structured rows of code repeating.

</div>
<div class="content">

## Math Notation

A great example in my eyes is mathematical notation. Compare the following two ways of writing the same expression:

![A paragraph about integrating a polynomial compared to normal notation for the same integral.](integral-notation.png)

This is illustrative, if exaggerated, example.

Integral notation might be unfamiliar to a beginner, but it is wonderfully efficient. You can tell what the expression represents *immediately* thanks to the integral sign and the equation layout. Imagine squinting until you see the general outline of *any* integral expression—that's the same principle I talk about as the "shape of the code".

It's easy to quickly identify parts of the equation to figure out what's going on: the limits are distinct from the equation itself and distinct from the variable of integration (ie \(dx\)). The polynomial itself continues the same theme: \(+\) as an operator gives structure to the polynomial, emphasizing its nature as a *set of terms*. In a sense, the \(+\) is just plumbing that gets out of our way so that we can identify the content *specific* to this polynomial (namely the coefficients and degree).

The paragraph, on the other hand, has the advantage of being readable by anyone, even if they aren't familiar with notation for integrals. But it has a fatal flaw: you *have* to read it. Every time. We have to read the text word-by-word to understand that it describes an integral and to see what the limits and function being integrated are. We can't even tell that this is an integral of a polynomial without close reading!

If this paragraph were surrounded by other paragraphs, we wouldn't be able to tell it apart from any other prose. I  often start reading papers by skipping explanations and looking at figures and equations until I find what I need—I wouldn't be able to do that without special notation and visual structure.[^language-dsls]

[^language-dsls]: This is also why I am not a big fan of APIs and domain-specific languages that try to mimic natural language text. I associate this style with Ruby and CoffeeScript, but it goes all the way back to SQL if not earlier. Just like reading math in paragraph form, *reading your DSL expression word-by-word is the last thing I want to do!*

</div>
<div class="content">

Good code is, ultimately, a human factors problem. We want to understand how people interact with code—how they read, write, skim, navigate, modify, reuse and repurpose code—and then write and organize our code in a style that makes these interactions as easy and natural as possible. 

Writing code to be read at a glance is just part of the story, but it's something I've found useful and important. Code I've written keeping this principle in mind just feels *lighter*. And that, fuzzy as it sounds, makes a real, practical difference to how much time, energy and focus it takes to work on a codebase. Making code skimmable is nowhere near the most important aspect of making code pleasant and effective, but it does matter, and I have not seen people explicitly talking about it.

I don't have much advice for putting these ideas into action. For me, just realizing that I valued code I could read at a glance and keeping that in mind when I wrote future code was all it took. I quickly developed the right habits and now I don't have to think about it.
