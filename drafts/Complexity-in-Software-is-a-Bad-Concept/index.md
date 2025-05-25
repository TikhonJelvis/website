---
title: Complexity is Bad—as a Concept
author: Tikhon Jelvis
published: 2025-05-25 14:57:59
---

I don't like talking about "complexity" any more because while almost everyone agrees complexity is "bad", people have radically different ideas of what "complexity" means.

"Complexity" in software conflates several distinct concepts:

  - plain poor design
  - complexity in operation rather than design (or technology)
  - inescapable real-world complexity
  - lots of moving pieces and details, code that strains working memory
  - abstract or novel concepts that are hard to learn up-front, but easy long-term
  - probably more I haven't considered off-hand

::: {.pull-quote .left style="width: 30%"}
> People tend towards mental models that are simple, legible and wrong.
:::

Some of these notions are basically opposites! At the Pareto frontier of system design, there is a fundamental trade-off between having more abstract concepts that are harder to learn up-front and exposing more details that make systems hard to work with on an ongoing basis. But people just dismiss both of these as "complexity"! These are two ideas that absolutely should not be conflated.

<!--more-->

I've seen lots of subtle sociotechnic dynamics dismissed as "complexity". For example, some of the most effective software I've seen has been situated software[^situated-software]; that is, software built for, and largely in a specific social context. Think seemingly messy code that is incestuously coupled to a single person's or team's workflow. This software might actually be perfectly simple in context, but it's going to seem painfully baroque to anybody outside that context.

[^situated-software]: "Situated software" is the idea that writing software for a specific social context—specific people doing specific things—is **qualitatively different** from writing software meant for others to use. A script you write for yourself takes less work, and different *sorts* of work, from building a product, open source project or library.

    One key difference: you can implicitly rely on a shared understanding of what the code needs to do—how it interacts with people, processes and other systems—rather than needing to encode that understanding in the design of the system, or generalize the system *beyond* that understanding.

    I've been rereading Peter Naur's classic ["Programming as Theory Building"][theory-building], and it gives me a simple explanation for how situated software is so different from non-situated software: situated software can directly reflect and rely on the existing "theory" (read "mental model") of its social context, rather than need to develop and articulate a theory that makes sense outside its immediate context.

    "Situated software" is still one of the most useful concepts I've learned to understand the nature of programming as a discipline. I'd recommend reading [Clay Shirky's][shirky] original essay that coined the term, as well as follow-up articles by [David R. MacIver][maciver] and [Hillel Wayne][wayne].

[shirky]: https://gwern.net/doc/technology/2004-03-30-shirky-situatedsoftware.html
[maciver]: https://www.drmaciver.com/2018/11/situated-software/
[wayne]: https://buttondown.com/hillelwayne/archive/situated-software/

[theory-building]: https://pages.cs.wisc.edu/~remzi/Naur.pdf

People tend towards mental models that are simple, legible and wrong. More complex models are less *controllable* models. It's easier to oversimplify than to understand social dynamics in nuance. The way complex systems—and any system comprised of people is complex—behave is inherently adaptable, context-specific and varied, not amenable to easy top-down control. A bad simple model might be simpler than a better complex model, but when you *apply* the bad model, you end up with more complexity overall[^bad-models]—the raw complexity in reality hasn't gone anywhere, but now you've introduced a larger gap between reality and the way you're trying to control it.

[^bad-models]: Lorin Hochstein had a couple of great posts that got me thinking about this.

    One about how [good models protect us from bad models][good-models]: a good understanding of a complex phenomenon might not be "actionable", but it's still important because it keeps us from making decisions based on *bad* understandings that *are* actionable.

    And [a follow-up][bad-analysis] working through an example of how bad analysis based on an actionable-but-wrong model can cause real problems.

[good-models]: https://surfingcomplexity.blog/2025/04/15/good-models-protect-us-from-bad-models/

[bad-analysis]: https://surfingcomplexity.blog/2025/05/10/when-a-bad-analysis-is-worse-than-none-at-all/

Given all I've seen, I've come to the conclusion that generic exhortations about "complexity" are actively harmful. If you're going to write a universally applicable rant, just write about how bad design is bad and good design is good! At least that's something that people will disagree with—I've met far more people who insist there is no such thing as "good" or "bad" design than people who insist that complexity is actually better than simplicity.
