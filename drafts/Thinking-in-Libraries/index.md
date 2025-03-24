---
title: Thinking in Libraries
author: Tikhon Jelvis
---

One of the simplest  programming tricks I've learned: organizing my logic as if I were writing a library—even when the logic will only get used once as part of an application. This is a shift in perspective rather than a prescriptive technique, so I've taken to calling it **thinking in libraries**.

For years, I just assumed that thinking in libraries was the “obvious” way to write code. I'd get personally affronted by code that was not organized along conceptual lines. But, having worked with a wide range of people and teams, I've realized that this approach is not always obvious. I've worked with multiple large codebase that have not been organized along these lines and, even when I've worked in codebases that were, I cannot recall anybody ever explicitly talking about it.

And, of course, like any other idea about programming and design, it is not universal. Some experienced engineers are aware of this approach, but see it as adding unnecessary abstraction boundaries and complexity[^disagree].

[^disagree]: After I posted about thinking in libraries on Twitter, [this response by @Profpatsch on Twitter][disagree-tweet] provided a clear counterpoint.

    > That’s literally the worst advice for most application code. If something only gets used once, inline inline inline. Don’t introduce abstraction boundaries where there are none! Add comments to section the code instead.

[disagree-tweet]: https://x.com/Profpatsch/status/1903804230382604359

<!--more-->

</div>
<div class="content">

Thinking in libraries has some tangible benefits like making code easier to test and reuse,

I am not doing this to support code reuse for the sake of code reuse; rather, it is a way to naturally write code grouped into logically self-contained units that directly reflect the conceptual design of whatever I am doing. The resulting code is easier to test and reuse, but I see this as *evidence of a good design* rather than a goal in and of itself.

I picked this approach up from Haskell, where I organize my code into concept-oriented modules and types, with as much of the core logic written in pure functions as possible. The final application becomes a relatively small amount of Haskell code written *in terms of the library*, primarily responsible for user interaction and external I/O.

There's a close relationship between writing library-style code and **bottom-up design**. Libraries are like domain-specific building blocks that you can pull together to build your system. Since the library components make sense on their own—they represent something useful and meaningful in general, without any context from your broader application—you can design useful, testable components before you have a clear top-down design for your system. In practice, I've found I naturally mix top-down and bottom-up design as I go along; bottom-up design gets reflected in each of the library modules I write, while top-down design determines which modules I write in the first place.

</div>
<div class="content">

## Examples

### Theta

### CXScore

Since then, I've found this design mindset useful in other languages and even in applications where complex I/O is integral to the core functionality. At CXScore, I developed a system in Python for running automated accessibility tests against live websites; most of the internal logic depended directly on a live Chromium session, so it would not have been possible to write it as pure functions. The initial version of this system mixed together our own backend concerns, DOM interaction and accessibility-specific logic, but this quickly became hard to navigate, understand and test. To address this, I wrote an internal Python library for interacting with Chrome; this library combined [Playwright] with [CDP][^cdp] to both control live Chrome browsers and run analysis against static [DOM snapshots][dom-snapshots][^dom-snapshots]

[Playwright]: https://playwright.dev

[CDP]: https://chromedevtools.github.io/devtools-protocol/

[^cdp]: The [Chrome DevTools Protocol][CDP], which is the interface Chrome provides for interacting with a live browser session from outside the Chrome process. CDP was initially developed to implement Chrome's built-in devtools (hence the name) but has since expanded to a wide range of browser automation capabilities.

[dom-snapshots]: https://chromedevtools.github.io/devtools-protocol/tot/DOMSnapshot/

[^dom-snapshots]: DOM snapshots are a CDP feature for quickly getting a flattened, serialized version of a page's DOM state. This is very useful, but the flattened representation is hard to work with; my library expanded the raw snapshot into a tree structure as well as adding in additional information like accessibility tree nodes.

</div>
<div class="content">

Thinking in libraries helps me write code that is naturally organized to match my conceptual model for whatever I am doing. There are some more tangible benefits too—code that is naturally easier to test, reuse and extend, as well as systems that naturally segregate I/O and state management from core logic—but I believe that most of these benefits are the result of a codebase that's aligned with a clear conceptual model, regardless of the exact approach used to design it.
