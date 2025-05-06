---
title: Thinking in Libraries
author: Tikhon Jelvis
---

One of the simplest  programming tricks I've learned: organizing my logic as if I were writing a library—even when the logic will only get used once as part of an application. This is a shift in perspective rather than a prescriptive technique, so I've taken to calling it **thinking in libraries**.

For years, I just assumed that thinking in libraries was the “obvious” way to write code. I'd get personally affronted by code that was not organized along conceptual lines. But, having worked with a wide range of people and teams, I've realized that this approach is not always obvious. I've worked with multiple large codebase that have not been organized along these lines and, even when I've worked in codebases that were, I cannot recall anybody ever explicitly talking about it.

Like any other idea about programming and design, it is not universal. Some experienced engineers are aware of this approach, but see it as adding unnecessary abstraction boundaries and complexity[^disagree].

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

In my Haskell code, the library parts of my logic were often “pure”, not depending on external state an I/O. While some of the benefits I got came from segregating external effects from logic—something that's great on its own—thinking in libraries can work well even in situations where state and I/O are inescapably linked to core logic.

I recently spent two years working at CXScore, and early stage startup that was building a system to crawl web apps and automatically find accessibility problems.

The core logic for accessibility testing is fundamentally tied to the browser. Modern browsers—and modern websites—are so dynamic and so complicated that we simply cannot analyze them without hooking directly into a running browser session. We can do some analysis by loading a page in a browser and taking a static DOM snapshot, but even that is not sufficient for all the accessibility rules we needed to check. The majority of our code needed to open, navigate and interact with pages *live*. The DOM and the JavaScript runtime are so complicated that we can't usefully simulate them in pure functions or even mock them for testing. When you're writing code to crawl and inspect live pages, either it works against live pages or it doesn't.

When I first arrived at CXScore, we had an initial application built that could crawl web pages and run some core accessibility checks like checking contrast and tabbing through the page. The code for the application was organized entirely in terms *of the application*: code to load web pages, code to navigate in a live browser, code to take and analyze DOM snapshots, code to manage user accounts and provide APIs to our own frontend... all organized as part of the app logic in the backend Node.js code.

When I joined, I had the chance to take a different approach: we were just starting to write some more complex algorithms and machine learning models in Python, so it made sense to start a self-contained codebase in Python that communicated with the Node.js backend through an HTTP API. 

Our initial Python algorithms depended just on static, serializable DOM snapshots, but I realized that we would quickly need to give our new algorithms direct access to a running Chrome session. Since round-tripping through the Node.js system would have been difficult, I needed to design a Python system that could interact with dynamic web pages in flexible ways—and the code could be organized along different principles than the existing Node.js codebase.

So I did what I always find natural: I started by writing a Python library for interacting with the browser. The first version of this was a thin wrapper over [Playwright] with a different interface in Python. I initially decided to start with a thin wrapper because I wanted to sand off some of the awkward edges of Playwright and to have more control over APIs in our own code, but it quickly proved to be a critical decision because we needed convenient access to Chrome-specific  [CDP][^cdp] functionality that Playwright did not expose.

The “library” code I wrote was not really organized as a Python library; it was really just a set of normal Python modules within a single namespace in our existing Python service code. Thinking of this code as a library was a shift in perspective, not a matter of tooling or a hard abstraction boundary at the language level.

This internal library was logically self-contained: you could easily use it to do browser automation that did not need to know anything about accessibility or our automated crawling and testing application. The exact set of features would be a bit odd—I was only implementing functionality we needed immediately, or, occasionally, things that simply seemed useful to me—but you could always expand it while keeping the same overall design and interface.

In hindsight, writing this library—which initially seemed redundant—proved to be one of my best engineering decisions during those two years. The “automation layer”, for lack of a better word, was a major *leverage point* for the system as a whole, so making it explicit and well-designed had an outsize effect on all of our code. Having a library under my control abstracting over interacting with the browser had several advantages:

  - we had a repository for managing the complexity and edge-cases we learned from working with real-world web apps
  - splitting DOM code—and DOM hacks—from accessibility-specific logic was the path of least resistance
  - my machine learning colleagues could interactively develop and test their logic in a Python interpreter
  - the code interface and documentation helped me teach my colleagues about how the DOM worked, without overwhelming them with unnecessary details

[Playwright]: https://playwright.dev

[CDP]: https://chromedevtools.github.io/devtools-protocol/

[^cdp]: The [Chrome DevTools Protocol][CDP], which is the interface Chrome provides for interacting with a live browser session from outside the Chrome process. CDP was initially developed to implement Chrome's built-in devtools (hence the name) but has since expanded to a wide range of browser automation capabilities.

[dom-snapshots]: https://chromedevtools.github.io/devtools-protocol/tot/DOMSnapshot/

[^dom-snapshots]: DOM snapshots are a CDP feature for quickly getting a flattened, serialized version of a page's DOM state. This is very useful, but the flattened representation is hard to work with; my library expanded the raw snapshot into a tree structure as well as adding in additional information like accessibility tree nodes.

</div>
<div class="content">

Thinking in libraries helps me write code that is naturally organized to match my conceptual model for whatever I am doing. There are some more tangible benefits too—code that is naturally easier to test, reuse and extend, as well as systems that naturally segregate I/O and state management from core logic—but I believe that most of these benefits are the result of a codebase that's aligned with a clear conceptual model, regardless of the exact approach used to design it.
