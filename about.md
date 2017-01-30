---
title: About
author: Tikhon Jelvis
---

<div class="content">

# About

![Me at event hosted by Quora in New York.][quora-nyc]

I'm Tikhon Jelvis: a programmer focusing on programming languages.

At the moment, I'm applying ideas from the programming languages to operations research at Target, as part of the data science and analytics group. We're developing a framework in Haskell for expressing and solving large-scale stochastic optimization problems.

I'm particularly interested in:

  * functional programming—especially Haskell
  * domain-specific languages
  * interactive programming tools
  * static analysis
  * type theory
  * semantics
  * program synthesis

Turns out these ideas are applicable pretty much everywhere—including retail. Who knew?

[quora-nyc]: img/tikhon-and-viktor.jpg

## Contact

Please feel free to reach by email: [tikhon@jelv.is](mailto:tikhon@jelv.is).

You can find me in a few places online:

  * [Quora](https://www.quora.com/Tikhon-Jelvis/answers)
  * [Stack Overflow](http://stackoverflow.com/users/286871/tikhon-jelvis)
  * [\@TikhonJelvis](https://twitter.com/tikhonjelvis) on Twitter
  * [GitHub](https://github.com/tikhonjelvis)

</div>

<div class="content">

## Projects

I've worked on a [few different side-projects](projects.html) over the years. The most recent is [Cow][^name], a diff and merge tool that compares parse trees rather than plain text.

Taking advantage of the extra structure present in parse trees, Cow can do a better job of grouping related changes together as well as higher-level analysis like finding moved blocks of code—even if the block was slightly modified as it was moved.

![A block of code that was both moved *and* modified.][cow-diagram]

## Background

I'm currently working as a "Lead Data Scientist" at Target's data science and analytics group (EDABI). All our code is in Haskell and builds with Nix, which is pretty awesome!

Before that, I worked at Esper, an early stage startup developing scheduling software. This included both backend work in OCaml, frontend work developing a Chrome extension in TypeScript and even a bit of iOS and Android.

I studied CS at Berkeley with an informal focus on programming languages. I did undergraduate research on program synthesis, working on the [Chlorophyll] project with Professor Bodik. One of these days I'll have an actual publications section, but for now my only paper comes from that project, published at [PLDI 2014][pldi]. I didn't graduate, but who knows—I might go back one of these days.

Apart from research I also interned at a few different companies from a mid-size enterprise software company to several (very) early stage startups. One highlight was a summer at Jane Street Capital, a proprietary trading firm in New York famous for using OCaml. Seeing the raw power of typed functional programming at scale there cemented my preference for ML-style languages, although I still go for Haskell over OCaml.

Outside CS, I'm an avid skier and Magic player. I used to fence saber competitively, but have been on a hiatus for a few years now. I'm interested in typography and type design, but haven't had a chance to dive too deeply into the subject. I like literature, particularly anything with an existential bent, and a range of experimental music from progressive metal to minimalist compositions and modern jazz.

[Esper]: https://esper.com/
[Cow]: cow
[cow-diagram]: cow/img/cow-thumbnail.png

[Chlorophyll]: http://pl.eecs.berkeley.edu/projects/chlorophyll/
[pldi]: http://conferences.inf.ed.ac.uk/pldi2014/acceptedpapers.html

[^name]: The name is a work in progress! Then again, so is the whole project.

</div>
