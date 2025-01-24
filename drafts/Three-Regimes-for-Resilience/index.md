---
title: Three Regimes for Resilience
author: Tikhon Jelvis
---

A useful mental tool for designing resilient software: **consider how the system will operate in three separate regimes**:

 1. Everything is going well and being handled automatically.
 2. Small issues crop up, requiring either *narrowly scoped*, possible *routine* human intervention
 3. A large, unexpected issue crops up, affecting the entire system and requiring *major*, *novel* human intervention

I'm intentionally talking about "issues" rather than "incidents" because this framework applies to situations that do not parse as "software incidents", like customer support issues, physical emergencies, regulatory audits or even emerging market *opportunities*. Quickly pivoting or expanding into a new area is not an *incident*, but it sure feels like a major *issue* to the folks working on it!

I spent six years at Target working on several parts of our inventory control system. Looking back, these three regimes would have been a valuable tool for understanding the overall design of the system. Different aspects of inventory control naturally map onto the three regimes:

 1. "Normal" automated replenishment could handle most items at most stores, most of the time. We spent a lot of time thinking about this design because it is the most natural place to focus.
 2. Sometimes, specific items would behave differently at specific stores. Perhaps a store needed extra inventory for a display, or there was an unexpected local event they wanted to adjust for. We could have made this much smoother by spending more time thinking about how to handle manual overrides from both inventory analysts and store managers.
 3. When COVID hit, shopping patterns *immediately* changed. People started buying larger quantities of certain items and the distribution of shopping trips between weekdays and weekends changed substantially. Our models and software weren't really flexible enough to quickly adapt to this change, but Target as a whole was able to adjust to the situation quickly thanks to *a lot* of creative manual work from analysts in the supply chain organization.

None of these were software incidents *per se*, but they were all situations where more adaptable and resilient software would have had a major impact.

A key insight is that these regimes are, largely, independent. Each of the regimes carries its own design considerations, and how well a system is designed for one does not necessarily indicate how well it is designed for another.

In the safety world, we know that small incidents do not necessarily come before large incidents, and eliminating small incidents is not sufficient to prevent catastrophic failure. A particularly memorable example: *on the same day* as the *Deepwater Horizon* disaster, executives were visiting the rig to celebrate seven years without lost-time incidents[^deepwater].

The same principle applies to the sorts of non-safety-related issues that crop up in complex software systems.

[^deepwater]: > There were few indications of any trouble with the Deepwater Horizon before the explosion. The rig won an award from the MMS for its 2008 safety record, and on the day of the disaster, BP and Transocean managers were on board to celebrate seven years without a lost-time accident. A BP spokesman said rigs hired by BP have had better safety records than the industry average for six years running, according to MMS statistics that measure the number of citations per inspection. BP has been a finalist for a national safety award from the MMS for the past two years.

    From [the Wall Street Journal][wsj] as quoted [on Wikipedia][wiki]
    
[wsj]: https://web.archive.org/web/20160930174214/https://www.wsj.com/articles/SB10001424052748704307804575234471807539054

[wiki]: https://en.wikipedia.org/wiki/Deepwater_Horizon_explosion

There's a nice—albeit limited—parallel between this three regime framework and [Rumsfeld's unknowns][1]:

 1. The known knowns
 2. The known unknowns
 3. The unknown unknowns
 
[1]: https://en.wikipedia.org/wiki/There_are_unknown_unknowns

It's easy to focus on the happy path at the expense of the other regimes because that's where we know the most. After all, the main reason we need manual intervention in cases 2 and 3 is that we *don't* know exactly how to handle it up-front, at least not to the level of fully realizing the logic in code[^formalization].

[^formalization]: A fun way to think about it: programming languages are formal systems, so any logic that we implement in code has been *fully formalized*—even if we haven't actually thought the logic through at that level! Code to do something is an assertion that we know *exactly* how to do it *to the lowest level*, even if that's never *really* true.

Explicitly seeing a design in terms of *all three* regimes helps us correct for this tendency. Even when we can't fully handle potential issues up-front—and even when we can't *know* what those issues are going to be!—we can still design our system to be better or worse in those situations.
