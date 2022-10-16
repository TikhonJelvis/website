---
title: What-Does-Reading-Code-Mean
author: Tikhon Jelvis
---
There's a common programming aphorism: "code is read more often than it is written."[^quote-origin] Since we spend more time *reading* rather than *writing* code, it is worth putting extra time and attention into writing **readable** code.

And hey, readable code is great! I put a lot of weight on keeping my own code readable—hardly a unique position. We all want code that's easy to read, right? But what does that *mean*? Engineers seem to take readability as a self-evident concept, but it isn't; I've seen too much "readable" code that wasn't[^understandable-code]. Just saying that code should be "readable", while true, is simply not enough.

One problem is that we're all familiar with reading prose, but code is *not* prose. **Reading code is fundamentally different from reading a novel.** Unlike prose, which is fluid, flexible and ambiguous, code needs to have a level of structure and precision at several layers of abstraction simultaneously so that it can run on a computer while also making sense to humans at both a high and a low level. Analogies between code and prose are as likely to be misleading as not.

The other problem is that reading code is not a single action. Instead, **we read code in several fundamentally different ways as we program**.  When I'm teaching people to write readable code, I have a modified version of the quote at the start of this article:

> Code is read more often than it is written, but it is skimmed more often than it is read.

<!--more-->

In practice,  I see *three* distinct modes for reading code:

  1. **Skimming**: looking through code quickly in order to understand the logical layout of a codebase and find the parts I'm currently concerned with.

  2. **Reading**: reading code in enough detail to understand what it *means*—what concepts code constructs represent and what the code is *supposed* to do.

  3. **Close reading**: reading code in enough detail to understand /what it actually does/. While *similar* to just reading code, *closely* reading code requires a lot more attention and working memory—it's something I only do when I either need to *debug* code, or when code *doesn't make sense* at a higher level.
  
How we read code in each mode is fundamentally different, which means that how we make code *readable* for each mode differs too. While some aspects might make code more or less readable across all the modes, the modes have their own needs which can be in tension or, at least, not overlap.

[^quote-origin]: A cursory internet search didn't find a clear original source for the quote, but it looks like the general idea was popularized by Robert C. Martin in [a passage from *Clean Code*][clean-code]

  > Indeed, the ratio of time spent reading versus writing is well over 10 to 1. We are constantly reading old code as part of the effort to write new code. ...[Therefore,] making it easy to read makes it easier to write.
  
  In context, this is a bit more specific: we spend more time reading than writing code because we end up reading our own code /as we're writing it/; this dynamic would apply even to "throw-away" code. I'm not sure how true this is in practice; at least /some/ throw-away code—like command-line invocations in an interactive terminal session—are pretty close to "write once read never". I believe that putting some effort into keeping even 100% throw-away code readable pays off, but that's a topic that deserves its own article!
  
  I expect other people articulated the idea well before *Clean Code* was written, but it's plausible that *Clean Code* propelled the idea—as an appropriately pithy saying—into hacker folklore.

[clean-code]: https://www.goodreads.com/quotes/835238-indeed-the-ratio-of-time-spent-reading-versus-writing-is

[^understandable-code]: I've also seen code that is *readable* but not *understandable*. Readability is necessary but not sufficient to make a codebase easy to underatand and work with, just like the best prose won't save muddled, compelx or inconsistent ideas!
