---
title: Writing Code To Be Read at a Glance
author: Tikhon Jelvis
---

In software engineering circles, there's a common adage: code is read more than it is written. This is a useful insight that pushes us to writing readable code, but I feel it's only part of the story. Just as code is read more than written, it is **skimmed more than read**. This idea should influence code style just as much as the original.

The reason code is read more than it is written is related to a simple reality of most software projects: more time and effort is spent on *maintaining* code than on *writing* it in the first place. You write the code once and then return to it multiple times, fixing bugs, adding features, refactoring. To do this, of course, you have to change the existing code---and to change it you have to read it and understand it.

But for every part of the code you need to return to in depth, there are four or five other modules that merely affect it. You don't have to understand these fully since you're not modifying them; you're content understanding *what the code is supposed to do*. And before you even get there, you have to find the specific part of the code you need to change. Again, this requires quickly scanning through large parts of the project---not trying to understand exactly what's going on in each section.

All this adds up to a similar multiplicative relationship: just as you'll end up reading multiple times for each piece of code you write, you'll end up skimming multiple times for each one you read. This means that writing code you can understand *at a glance* is at least as important as writing code that you can read at all.
