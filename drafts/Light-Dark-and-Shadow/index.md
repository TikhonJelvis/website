---
title: Light, Dark and Shadow
author: Tikhon Jelvis
---
One of the most useful skills I've picked up from math and computer science is **smoothly moving between different perspectives on a concept or situation**. In our complex world, we almost never have a single “best” way to understand something.

For example, we could see a probability distribution as a prediction, as a representation of uncertainty about something external, as a PDF/CDF or as the process we would use to sample it. (Flip that view around, and we can think of a simulation *as* a probability distribution!)

Similarly, we can see a unit of code as some instructions for a computer to run, as a model of something external, as a formalization of some fuzzy concept or as a modular component of a larger system.

Some shifts are small and clear. A PDF and a CDF carry the same information in almost the same way; the only difference is in what aspect of a probability distribution they emphasize, and what calculations they make easier or harder—the same principle that motivates engineers to choose different reference frames for different problems.

Other shifts in perspective change how we understand a construct at a higher level. Does a probability distribution represent possible random outcomes from a process, or does it measure our *uncertainty* about something? Is probability a measure of chance and risk or belief and knowledge? The underlying construct stays the same, but our interpretation changes. Is this distinction meaningful even meaningful?

Stealing a framework from on of my favorite math essays[^when-is-one], each perspective will:

 - shine a light on some aspects of our object
 - leave other aspects in shadow
 - keep some aspects completely in the dark

What do my examples shine a light on? What do they keep in the shadows?

[^when-is-one]: The essay is Barry Mazur's [“When is one thing equal to some other thing?”][when-is-one-pdf]; I highly recommend it as long as you don't mind going off the rails into category theory around page 3!

[when-is-one-pdf]: https://people.math.osu.edu/cogdell.1/6112-Mazur-www.pdf

<!--more-->

</div>
<div class="content">

## The Ladder of Abstraction

A similar idea in programming and computer science is “moving up and down the ladder of abstraction”[^ladder], which has been a foundational skill for me as a programmer almost as long as I've been programming.

[^ladder]: I am *pretty* sure I first heard the term “ladder of abstraction” taking Brian Harvey's course on [The Structure and Interpretation of Computer Programs][scip] as a freshman at Berkeley, but I might have also picked it up from [this classic interactive essay by Bret Victor][bret-victor-ladder] around the same time. Regardless, the ability to work at different levels of abstraction has been a foundational skill for me as a programmer almost as long as I've been programming.

In programming, shifting between levels of abstraction can be critical for debugging logic problems and fixing performance issues. To fix a logic bug, we need to both understand what the code *should* be doing (a higher-level view), what the code is *actually* doing (a lower-level view) and why the two do not match (both views at once).

I worked on a team that replaced a clustering algorithm that took hours to run with an alternate system that ran in 10 seconds. To do that I first had to understand what the existing code did and why it was slow, but then I also had to understand how the clustering algorithm fit into the broader system. (What was it *supposed* to do?) I needed this understanding because we could not “just” make the existing algorithm faster; we needed to factor our some up-front work that could be reused across multiple requests as well as switching to an approximate approach. The approximation worked well *in this particular context*, but it was not universal.

A similar distinction comes up when writing code. Code can be wrong because it does the wrong thing, but it can also be wrong if it does the right thing for the wrong reasons or if it does not fit into the broader design of our system. 

At a previous role, I worked on an automated accessibility product that tracked the “states” of a page we were analyzing. A state would have a URL, a sequence of interactions to get to the state, a screenshot and some other metadata. Our frontend used an endpoint to fetch the screenshot for a state. At some point, we needed to display screenshots that did *not* map to a specific state; instead of adding a new endpoint to do this, a programmer added a new `ScreenshotState` construct that had a screenshot but none of the other metadata of a normal state. This let us reuse the same logic for storing and serving screenshots—but made the rest of the system far more confusing because we now had a mix of states that actually represented states and states that were just containers for a screenshot.

The `ScreenshotState` code *ran* correctly, but was wrong at a higher level of abstraction; it did not introduce bugs immediately, but it did introduce *potential* bugs and increased the cognitive load for understanding and extending our system.

I like to think about code that is right for the wrong reasons as the programmer's version of [Gettier counterexamples][gettier], but that's a pretty quirky and imperfect connection. [Jimmy Koppel's “Three Levels of Software”][three-levels] is a more grounded take on the same idea.

[gettier]: https://fitelson.org/proseminar/gettier.pdf
[three-levels]: https://www.pathsensitive.com/2018/01/the-three-levels-of-software-why-code.html

</div>
<div class="content">

## Broader Perspectives

But useful perspectives do not always travel in one dimension! The highest-impact shifts in perspective might be entirely orthogonal to higher or lower levels of abstraction. It took me years of additional learning and development to understand how to shift perspectives along dimensions other than the level of abstraction.

[sicp]: https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/index.html
[bret-victor-ladder]: https://worrydream.com/LadderOfAbstraction/
