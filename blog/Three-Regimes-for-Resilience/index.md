---
title: Three Regimes for Resilience
author: Tikhon Jelvis
published: 2025-01-24 17:22:51
modified: 2025-01-24 17:41:02
modified: 2025-01-24 17:42:17
modified: 2025-01-24 17:42:43
---

A useful mental tool for designing resilient software: **consider how the system will operate in three separate regimes**:

 1. The happy path. Everything is handled automatically with no manual intervention needed.
 2. Small issues crop up requiring *narrowly scoped*—but not necessarily *routine*—human intervention.
 3. A large, unexpected issue crops up, affecting the entire system and requiring *major*, *novel* human intervention

I'm intentionally talking about "issues" rather than "incidents" because this framework applies to situations that do not parse as "software incidents": think customer support issues, physical emergencies, regulatory audits or even emerging market *opportunities*. Quickly pivoting or expanding into a new area is not an *incident*, but it sure feels like a major *issue* to the folks working on it!

In hindsight, this would have been a useful tool for designing the inventory control system I worked on at Target, and it's a tool I'm going to use for future systems I design.
 
![A retail distribution center. (Photo credit: [Nick Saltmarsh][flickr])](distribution-center-stock-photo.jpg "A stock photo of a retail distribution center, the interior of a big wharehouse with shelves stocked with pallets and several forklifts on the side.")

[flickr]: https://www.flickr.com/people/nsalt/

<!--more-->

</div>
<div class="content">

I spent six years at Target working on several parts of our inventory control system. Looking back, these three regimes would have been a valuable tool for understanding the overall design of the system—not just our software, but also the people and processes it was supporting. Different aspects of inventory control naturally map onto the three regimes:

 1. Automated replenishment could handle most items at most stores, most of the time.
 2. Sometimes, specific items would behave differently at specific stores. Perhaps this was a shortcoming in our automated system, but it could also be something else: a store needed extra inventory of a display or we needed to rebalance transfer orders for operational reasons.
 3. When COVID hit, shopping patterns changed *overnight*. People started buying larger quantities of certain items and the distribution of shopping trips between weekdays and weekends changed substantially.

None of these were software incidents *per se*, but they were all situations where more adaptable and resilient software would have had a major impact.

Considering each of these regimes separately would not let us completely "solve" these sorts of issues ahead of time, but it *would* let us design our system to handle them more gracefully.

 1. We'd still want to handle as much as we could automatically, but having a more explicit idea about how to detect and handle the limits of our automated system would have given us more room to iterate and improve the automated system over time.
 2. Understanding that some kinds of manual intervention are *inevitable*—and possibly even *desirable*—would have led us to integrate feedback and control mechanisms from store managers and inventory analysts from the beginning. For example, this explains why it was important for *inventory control policies* to be interpretable, but less important for *the optimization algorithm that produced them*: policies were an interface for people handling operational issues, while the optimization algorithm only affected our automated decision-making.
 3. Considering how to respond to unexpected changes would motivate better observability, explicit manual controls and flexible code design. Good abstractions and code quality not only makes maintenance easier, but also makes it easier to quickly develop *ad hoc* solutions to problems that crop up—maybe we can't immediately come up with a *good* solution to a major change in shopping patterns, but can we make it easy to quickly write and test some sort of heuristic to tide us over in the short term?
 
It's always easier to see alternate design options in hindsight. My hope is that codifying some mental design tools like this will both help me think about design more broadly in the future and communicate my design considerations more clearly.

</div>
<div class="content">

A key insight is that these regimes are, largely, independent. Each of the regimes carries its own design considerations, and how well a system is designed for one does not necessarily indicate how well it is designed for another.

In the safety world, we know that small incidents do not necessarily come before large incidents, and eliminating small incidents is not sufficient to prevent catastrophic failure. A particularly memorable example: *on the same day* as the *Deepwater Horizon* disaster, executives were visiting the rig to celebrate seven years without lost-time incidents[^deepwater].

The same principle applies to the sorts of non-safety-related issues that crop up in complex software systems.

[^deepwater]: > There were few indications of any trouble with the Deepwater Horizon before the explosion. The rig won an award from the MMS for its 2008 safety record, and on the day of the disaster, BP and Transocean managers were on board to celebrate seven years without a lost-time accident. A BP spokesman said rigs hired by BP have had better safety records than the industry average for six years running, according to MMS statistics that measure the number of citations per inspection. BP has been a finalist for a national safety award from the MMS for the past two years.

    From [the Wall Street Journal][wsj] as quoted [on Wikipedia][wiki]
    
[wsj]: https://web.archive.org/web/20160930174214/https://www.wsj.com/articles/SB10001424052748704307804575234471807539054

[wiki]: https://en.wikipedia.org/wiki/Deepwater_Horizon_explosion

</div>
<div class="content">

There's a nice—albeit limited—parallel between this three regime framework and [Rumsfeld's unknowns][1]:

 1. The known knowns
 2. The known unknowns
 3. The unknown unknowns
 
[1]: https://en.wikipedia.org/wiki/There_are_unknown_unknowns

It's easy to focus on the happy path at the expense of the other regimes because that's where we know the most. After all, the main reason we need manual intervention in cases 2 and 3 is that we *don't* know exactly how to handle it up-front, at least not to the level of fully realizing the logic in code[^formalization].

[^formalization]: A fun way to think about it: programming languages are formal systems, so any logic that we implement in code has been *fully formalized*—even if we haven't actually thought the logic through at that level! Code to do something is an assertion that we know *exactly* how to do it *to the lowest level*, even if that's never *really* true.

Explicitly seeing a design in terms of *all three* regimes helps us correct for this tendency. Even when we can't fully handle potential issues up-front—and even when we can't *know* what those issues are going to be!—we can still design our system to be better or worse in those situations.
