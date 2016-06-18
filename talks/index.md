---
title: Talks
author: Tikhon Jelvis
---

<div class="content">

# Talks

Here are the slides for talks I've given at meetups and conferences. Presenting is fun! Unfortunately, most were not recorded. I included links to videos for the ones that were.

![Me giving a talk at the SF Haskell meetup at [Wagon].](img/me-giving-talk-at-wagon.jpg)

My older slides were produced with a customized version of [org-html5presentation.el](https://gist.github.com/kinjo/509761) that does not work properly in Firefox. I've included pdf versions for these---they are ugly but readable. Newer slides, based on [reveal.js](http://lab.hakim.se/reveal-js/#/) (still built from [org mode](https://github.com/yjwen/org-reveal)) should work everywhere (including mobile).

[Wagon]: http://wagonhq.com

</div>
<div class="content">

## Programming Languages/Haskell

[Compose 2016][compose] hosted by JP Morgan in New York:

  * [Analyzing Programs with Z3](compose-2016) - modeling and analyzing programs with the Z3 SMT solver: bounded verification, program synthesis… etc.

[SF Haskell meetup](http://www.meetup.com/Bay-Area-Haskell-Users-Group/) hosted by [Wagon]:

  * [Inductive Graphs](inductive-graphs-at-wagon) ([video](http://begriffs.com/posts/2015-09-04-pure-functional-graphs.html)) - working with graphs in a functional style using the [functional graph library][fgl] (fgl)

[BayHac 2015](http://bayhac.org) - a small annual Haskell conference/hackathon:

  * [Thinking with Laziness](thinking-with-laziness) ([video](http://begriffs.com/posts/2015-06-17-thinking-with-laziness.html)) - understanding laziness and using it to write more modular, expressive and elegant programs

[Haskell Hackers meetup][haskell-hackers] (at Hacker Dojo):

  * [Nondeterminism](nondeterminism.html) - lightweight nondeterministic programming in Haskell using the list monad
  * [Inductive Graphs](inductive-graphs.html) - introduction to functional graph algorithms (my newer talk at Wagon is better)
  * [Analyzing Programs with SMT Solvers](analyzing-programs-with-smt.html) - see newer Compose talk

Two lightning talks at [BayHac 2013](http://www.haskell.org/haskellwiki/BayHac2013):

  * [Program Synthesis for ArrayForth](af-slides.html) - brief overview of what I did working on the [Chlorophyll compiler][chlorophyll compiler] at Berkeley
  * [Algebras and Coalgebras](algebras.html) - explanation of algebras and co-algebras based on a [post](http://stackoverflow.com/questions/16015020/what-does-coalgebra-mean-in-the-context-of-programming/16022059#16022059) I wrote

A longer talk at [Hac φ 2013](http://www.haskell.org/haskellwiki/Hac_%CF%86):

  * [Program synthesis](synthesis-slides.html) ([pdf](synthesis-slides.pdf)) - a general overview of program synthesis techniques

[compose]: http://www.composeconference.com/2016
[fgl]: https://hackage.haskell.org/package/fgl
[chlorophyll compiler]: http://pl.eecs.berkeley.edu/projects/chlorophyll/

## Type Theory

I gave a series of talks introducing type theory at the [SF Types, Theorems and Programming Languages](http://www.meetup.com/SF-Types-Theorems-and-Programming-Languages/) meetup at [Mixrank](http://mixrank.com) HQ, going from the untyped λ-calculus to dependent types.

  * [Untyped Lambda Calculus](untyped-lambda-calculus.html) ([pdf](untyped-lambda-calculus.pdf))
  * [Simply Typed Lambda Calculus](stlc.html) ([pdf](stlc.pdf))
  * [Fun with Curry-Howard](curry-howard.html) ([pdf](curry-howard.pdf))
  * [Polymorphism---System F](system-f.html) ([pdf](system-f.pdf))
  * [Dependent Types](dependent-types.html) ([pdf](dependent-types.pdf))

I'm not sure I would recommend following *these slides*, but this general progression is a great way to learn the underlying theory. Implementing interpreters for each stage, working up from the untyped lambda calculus to dependent types, is a great way to learn---it's less of a climb than it seems!

## FRP

I did a survey of functional reactive programming (FRP) for CS 263 at Berkeley. At the end of the semester, I gave a presentation on the subject:

  * [Introduction to FRP](../frp) - includes a bit more detail about my example code

</div>

[haskell-hackers]: http://www.meetup.com/haskellhackersathackerdojo/
