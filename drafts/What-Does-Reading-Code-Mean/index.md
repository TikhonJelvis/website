---
title: What-Does-Reading-Code-Mean
author: Tikhon Jelvis
---
There's a common programming aphorism: "code is read more often than it is written."[^quote-origin] Since we spend more time *reading* rather than *writing* code, it is worth putting extra time and attention into writing **readable** code.

[^quote-origin]: A cursory internet search didn't find a clear original source for the quote, but it looks like the general idea was popularized by Robert C. Martin in [a passage from *Clean Code*][clean-code]

  > Indeed, the ratio of time spent reading versus writing is well over 10 to 1. We are constantly reading old code as part of the effort to write new code. ...[Therefore,] making it easy to read makes it easier to write.

  In context, this is a bit more specific: we spend more time reading than writing code because we end up reading our own code /as we're writing it/; this dynamic would apply even to "throw-away" code. I'm not sure how true this is in practice; at least /some/ throw-away code—like command-line invocations in an interactive terminal session—are pretty close to "write once read never". I believe that putting some effort into keeping even 100% throw-away code readable pays off, but that's a topic that deserves its own article!

  I expect other people articulated the idea well before *Clean Code* was written, but it's plausible that *Clean Code* propelled the idea—as an appropriately pithy saying—into hacker folklore.

[clean-code]: https://www.goodreads.com/quotes/835238-indeed-the-ratio-of-time-spent-reading-versus-writing-is


And hey, readable code is great! I put a lot of weight on keeping my own code readable—hardly a unique position. We all want code that's easy to read, right? But what does that *mean*? Engineers seem to take readability as a self-evident concept, but it isn't; I've seen too much "readable" code that wasn't[^understandable-code]. Just saying that code should be "readable", while true, is simply not enough.

[^understandable-code]: I've also seen code that is *readable* but not *understandable*. Readability is necessary but not sufficient to make a codebase easy to underatand and work with, just like the best prose won't save muddled, compelx or inconsistent ideas!


One problem is that we're all familiar with reading prose, but code is *not* prose. **Reading code is fundamentally different from reading a novel.** Unlike prose, which is fluid, flexible and ambiguous, code needs to have a level of structure and precision at several layers of abstraction simultaneously so that it can run on a computer while also making sense to humans at both a high and a low level. Analogies between code and prose are as likely to be misleading as not.

The other problem is that reading code is not a single action. Instead, **we read code in several fundamentally different ways as we program**.  When I'm teaching people to write readable code, I have a modified version of the quote at the start of this article:

> Code is read more often than it is written, but it is skimmed more often than it is read.

<!--more-->

</div>
<div class="content">

My model for reading code has three distinct modes:

  1. **Skimming**: looking through code quickly in order to understand the logical layout of a codebase and find the parts I'm currently concerned with.

  2. **Reading**: reading code in enough detail to understand what it *means*—what concepts code constructs represent and what the code is *supposed* to do.

  3. **Close reading**: reading code in enough detail to understand /what it actually does/. While *similar* to normal reading, close reading requires more attention and working memory—it's something I only do when I need to debug or when the code doesn't make sense at a higher level.

The trick is that code can be more or less readable in each of these modes separately—and readability for one mode can be in tension with readability for another. This explains common disagreements I've seen about readability: somebody focused on *close reading* might prefer more explicit control flow with less higher-level structure; somebody focused on higher-level reading would disagree, prefnerring clearer abstractions at the expense of deemphasized control flow. When both see readability as universal and one-dimensional, how can they resolve their disagreement? So people throw up their hands and insist readability is subjective, when they're actually just talking past each other.[^subjective]

[^subjective]: "Readability is subjective" is a sentiment I've seen repeatedly in online discussions, often in discussions comparing different programming languages or arguing about the value of code review and pull requests. Prioritizing different reading modes is not the only reason people may disagree about readability: how easily somebody will be able to read code also depends on familiarity, background and context. Having multiple dimensions and depending on context is not the same as being subjective; while readability inherently has *some* subjective component, it's consistently exaggerated in discussions.


</div>
<div class="content">

What does this model of *reading* code tell us about *writing* code?

When we're skimming code, we're scanning for key information; we don't want to parse anything visually dense. Even when we're reading code at a higher level, we rarely want to mentally inspect every little detail. Only when we get to close reading—once we're already investing time and attention to a specific part of the codebase—do we need to follow everything. For the first two modes, we care about what we can understand **at a glance**. How well can we navigate and understand code without visually parsing and interpreting each line in detail?

As an illustrative—if somewhat extreme—example, let's compare three versions of the same logic. Here's some Java code using [BigDecimal]:

``` java
final BigDecimal result = a.multiply(x.pow(2)).plus(b.multiply(x.plus(c)));
```

Java does not have operator overloading. If it did[^java-operator-overloading], the code might look like this instead:

``` java
final BigDecimal result = a * (x * x) + b * x + c;
```

Both of these implement the same mathematical formula:

$$
\text{result} = ax^2 + bx + c
$$

This is about as simple as a polynomial gets, but even that wasn't immediately clear from the first version of the code: you'd have to pay attention to read the sequence of named method calls to understand what was going on. The version with overloaded operators is a real contrast, although it still requires more attention than the math notation. If the first snippet is like reading a paragraph, the second snippet is like reading a short sentence and the math notation is like reading a single word.

With the math notation, we can *instantly* tell we're looking at a polynomial just from the shape. The information we care about for polynomials are the terms; the addition and multiplication holding everything together is more like an implementation detail. The math notation reflects this by visually grouping together the information for each term and using an operator (+) as "punctuation" to pull it together. Addition is part of what makes the polynomial *a polynomial* but, once we know what we're looking at, it can fade into the background.

Math notation in general tends to convey information effectively at a glance, which makes it great for skimming and higher-level reading, but can be a bit of a pain for understanding all the details. When I first see a math book, I'll have to spend some extra effort deciphering notation in my head—but, once I'm used to it, I can quickly skip around mathematical texts by just looking at equations until I find what I'm looking for.

[BigDecimal]: https://docs.oracle.com/javase/8/docs/api/java/math/BigDecimal.html

[^java-operator-overloading]: Example from [Is it time for operator overloading in Java?](https://blogs.oracle.com/javamagazine/post/is-it-time-for-operator-overloading-in-java) in
Java Magazine

</div>
<div class="content">

Humans are naturally good at tracking context. The same word or symbol [can have different meanings in different contexts][polysemy] and people *don't even notice*. Have you ever thought about how the "white" in "white wine" is really yellow? Or how the parentheses in `f(x)` mean "function application", but the parentheses in `(a + b) * c` are just for grouping? This is a classic obstacle for language learners—in part because native speakers find resolving the ambiguity *so easy* that they don't realize it would need explanation!

This applies to code as well. **How people will understand code depends on the context that they have.** And readers will have different amounts of context depending on the mode they're in:

  1. When somebody is *skimming* code, they're quickly covering broader sections of the code so they'll have less context.

  2. When somebody is *reading* code, they're focused on a something specific, so they have pretty solid context.

  3. By the time somebody gets to a *close reading* of the code, they'll have the clearest idea of where they are in the code and what's going on.

This dynamic affects how I write and organize code. Parts of the code that are relevant to closer reading—like helper functions and variables—are written with more specific context in mind. Inside the guts of a function, short names like `x` or `t` can be perfectly clear and help keep expressions easy to visually parse; but I would reach for a more explicit name for the parts of the code relevant for skimming.

[polysemy]: https://en.wikipedia.org/wiki/Polysemy
