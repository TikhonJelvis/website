---
title: Nondeterminism
author: Tikhon Jelvis
---

[Nondeterministic programming][np] is a programming paradigm that extends a normal language with a **choice operator**. Control flow is specified as sets of choices at different parts in the program, along with some constraints. When run, the program makes the choices needed to satisfy all the constraints. The actual choice is usually performed by backtracking at runtime, but this is below the level of abstraction. One useful way to view this is that the computation *branches* at each choice point, trying all possible choices to see which ones satisfy the constraints.

Nondeterministic programming is a very useful model for various search and constraint-satisfaction problems because it abstracts over the actual search logic involved. You specify the constraints and possibilities in a declarative manner without needing to deal with the lower-level details of the search algorithm. It also forms the backbone of logic programming like Prolog.

In Haskell, nondeterminism and nondeterministic programming are very naturally modeled with lists. This makes it surprisingly easy to implement and embed a nondeterministic DSL into Haskell---in fact, this is exactly what the list monad instance does! Looking at lists like this gives us a very useful perspective on laziness and a productive way to think about monads.

![Backtracking makes the execution of a nondeterministic program look like a tree rather than a linear sequence.][tree]

[np]: http://en.wikipedia.org/wiki/Nondeterministic_programming
[tree]: tree.png

<!--more-->

</div>
<div class="content">

# Composition

Our exciting tale starts, as so often in functional programming, with function composition: `g ∘ f`. We can take a function `f ∷ a → b`, a function `g ∷ b → c` and combine them to get a function `a → c`. One of the most fundamental ideas in functional programming.

<div class="figure" id="function-composition">
<p>
Simple function composition.
</p>
</div>