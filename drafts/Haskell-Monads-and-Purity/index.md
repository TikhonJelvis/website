---
title: Haskell, Monads and Purity
author: Tikhon Jelvis
---

I believe the notion that Haskell uses "monads" to enforce purity is rather misleading. It has certainly caused quite a bit of confusion! It's very much like saying we use "rings to do arithmetic". We don't! We use numbers, which just happen to form a ring with arithmetic operations. The important idea is the *number*, not the ring. You can---and most people *do*---do arithmetic without understanding or even knowing about rings. Moreover, plenty of rings don't have anything to do with arithmetic.

Similarly, in Haskell, we use types to deal with effects. In particular, we use `IO` and `State` and `ST`, all of which happen to form monads. But it is still better to say we use "the `IO` type" to do IO than to say we use "monads" to do IO or that we use the `State` type for state.

Now, how does this work? Well, I'll give you one view on the matter, using `State` as a case study.

<!--more-->

</div>
<div class="content">

## State

Perhaps the most important philosophical idea is that Haskell does *not* "enforce" purity. Rather, Haskell as a language simply does not include any notion of effects. Base Haskell is exclusively about **evaluation**: going from an expression like `1 + 2` to `3`. This has *no* provisions for side effects; they simply don't make any sense in this context[^1].

Side effects and mutation are not prohibited; they simply weren't added in the first place. But let's say that, on a flight of fancy, you decide you really want some mutable state in your otherwise-happy Haskell code. How can you do this? Here's one option: implement the state machinery yourself. You can do this by creating a little sub-language and writing an interpreter for it. The language itself is actually just a data type; the interpreter is just a normal function which takes care of evaluating your stateful sub-program and maintaining the state.

Let's call our sub-language's type `State`. A single `State` "action" has some value as the result, so it needs a type parameter: `State a`. For the sake of simplicity, let's imagine you only ever want a single piece of state—an `Int`. (You could easily generalize this to any type of state, say a variable `s`, or, with a bit more effort, a state type that could change over time. But we'll consider the simplest case.)

So what sorts of actions do we need to support? One thing we want to do is *ignore* the current state: if we have an `a` already, we want to have a `State a` which just evaluate to that. It's a way to add constants to our sub-language.

We also want some way to get and set the state. Getting should just evaluate to the current value of the state; setting should take the new value to set. Since setting does not have a meaningful return value, we can just return `()`. Three basic actions: **return** some value, **get** the state and **set** the state:

    return ∷ a → State a
    get    ∷ State Int
    set    ∷ Int → State Int

However, these actions by themselves are rather boring. We need some way to string them together—some way to *compose* actions. The simplest way is to just sequence two actions: run one, get the new state and run the next one. We can call this an infix `>>`:

    (>>) ∷ State a → State b → State b
    
When our interpreter sees a composed action like this, it will just evaluate the left one, get the resulting state and evaluate the second one using this new state. For example, `set 10 >> get` will result in `10`. However, this is still missing something. We can't really use the value of the state in interesting ways. In fact, we can't even write a simple program to increment the state value! We need some way to use arbitrary Haskell logic inside of our stateful programs.

How do we do this? Well, since we want to use arbitrary logic, we're looking to support some sort of user-supplied function embedded into our language. Since Haskell only knows how to work with `a` values rather than `State a` values, this function should take a `a`. Since we want to use this logic to set the state, this function should return a state action. The type we want looks something like this:

    a → State b
    
This function has an interesting "shape" (for lack of a better word). By going from a normal value (`a`) to a state action (`State b`), we're injecting normal Haskell into our state sub-language. Since we're also transitioning from `a` to `b`, we're enabling *arbitrary* logic in the function. We can now use this to write an incrementing function:

    addToState ∷ Int → State ()
    addToState x = set (x + 1)
    
Now, how do we intersperse this sort of function with our normal state actions? Well, we want to take a state action, one of these functions, combine them and get the result. The type we want, then, is:

    State a → (a → State b) → State b
    
Quite similar to our existing `>>` function; the only difference is that the second "action" is a function now. When our interpreter sees this, it will evaluate the lefthand side, plug the result into the righthand function, get the new state action and run it with the updated state.

With this, we can combine `get` and `addToState` giving us a program that increments our current state by `1`. We can use this approach to execute arbitrary logic using our state. We can call this sort of composition `>>=`, a fancier version of `>>`:

    (>>=) ∷ State a → (a → State b) → State b
    
Using this `>>=` operator, we can write the increment program:

    increment = get >>= addToState
    
and if we want to increment three times in a row, we can do this:

    increment3 = increment >> increment >> increment
    
It's important to note that the `>>=` doesn't *do* anything; it just produces some data structure which contains both the lefthand side and the function. It's our interpreter function that will ever do anything. `increment` is just a normal value of the type `State ()`; nothing special. This means that the order in which our expressions get evaluated does not affect how our state works—we're just evaluating an AST; the state effects are all managed by the interpreter function. This neatly separates our stateful, observable execution from Haskell's evaluation order which is below our level of abstraction and therefore not observable.

Now whenever we want to use some mutable state, we just write our program using this `State a` type, combining the disparate parts using `>>=`. Then, to actually use this, we invoke our interpreter function. This function looks at the `State a` value and evaluates it, threading the changing state through each part. The exact details of how the interpreter works, or even how a `State a` value is represented internally, are not particularly important.

</div>
<div class="content">

Unfortunately, using `>>=` and `>>` gets rather ugly, so we also want to throw in some syntax sugar to make using this type look like a reasonable imperative program. This will make our little sub-language bearable and the resulting code easier to read. We can o this with two core patterns: `action₁ >> action₂` will get transformed into two lines of code:

    do action₁
       action₂

and `action₁ >>= \ x → action₂` will be transformed into

    do x ← action₁
       action₂

Now our sequences of actions actually look like sequences of statements in a normal language.

We've added state to a language where it normally simply does not exist. All using normal functions and data types, with just a bit of syntax sugar to help the medicine go down. Nifty. But not really *magical*.

By now, you've probably realized that `return` and `>>=` make `State` a monad. The syntax sugar, of course, is do-notation. But if you're interested in how we get mutable state, this doesn't tell you very much. Instead, the relevant bits are what the `State a` type looks like (it's basically an AST of commands) and what the interpreter does. The fact that it's a monad is useful, but does not entirely characterize the `State` type.

[^1]: This is why `unsafePerformIO` is unsafe: it's completely foreign to the programming model.

</div>
<div class="content">

## IO

Sure, we can add state by producing a little AST and explicitly evaluating it. But it's easy to model state because it's an entirely internal phenomenon to the program. But how do we deal with *external* effects—IO? This is something we *fundamentally* can't express with normal Haskell types and functions.

The basic idea is the same: we assemble a program and run it through an interpreter which manages the effects. The main difference is that this interpreter itself is not written in Haskell—it has to be built in. This interpreter is actually the Haskell runtime system and, by extension, the whole operating system.
    
So, in some sense, `IO` *is* magical, but this is inevitable: to produce the actual effects, it has to talk to the operating system and ultimately the hardware directly. You simply can't do this within normal Haskell because Haskell only knows about evaluating expressions. Talking to the OS is generally below our level of abstraction. And ultimately, this requires magic because it *is* magic—it's built right into the hardware! 

So the runtime system is our interpreter for `IO`. A Haskell program has a `main` value which is an IO action; when you run this program, what you're actually doing is evaluating `main` and then giving it to the runtime system to interpret. This system then runs the effects, intoning the appropriate incantations to make things happen in the real world.

The `IO` type is just something the compiler and runtime system understand and run. It is a monad in basically the same way as our `State` type above. Conceptually, the only difference is in how it gets interpreted.

In reality, of course, things aren't implemented like this. But it's a reasonable way of thinking about it. 

So the final verdict: Haskell does not "use monads to manage effects". Rather, Haskell has types like `IO` to manage effects, and `IO` happens to form a monad. Moreover, it turns out that the monad operations are very useful to write actual programs by composing `IO` actions.

</div>
<div class="content">

## Why

After reading all this, a reasonable question is simply "why?". Why bother with all this? Why separate effects out and build all this conceptual machinery to achieve them again?

There are a few answers to this, but all ultimately boil down to raising our level of abstraction and making our language more expressive.

This system **separates evaluation and execution**: calculating a value and performing an action are now different. Haskell does not have a hard notion of "now" strung throughout the code, because the order of evaluation does not have visible effects[^2]. We're no longer constrained in writing our definitions in the order they need to be evaluated; instead, we're free to organize our code, even at a very local level, based on how we want it to be read. This extra notion of "now" and an environment that changes over time (ie mutable state) is not necessary in most code; getting rid of it limits cognitive load.

In essence, we move evaluation *below our level of abstraction*. Instead of being an omnipresent facet of every line we write, evaluation now happens largely in the background. Is it always a perfect abstraction? No. We have performance concerns which can sometimes be dire and we have ways to completely break the abstraction (`unsafePerformIO` and friends). But does it have to be perfect to be useful?

I doesn't. Think of it like garbage collection: GC moved allocating and deallocating memory below our level of abstraction and pushed programming languages ahead. At the same time, it has similar pitfalls to non-strictness: performance concerns and similar ways to break the abstraction (`unsafe` blocks, FFI, low-level apis into the GC). And yet it's incredibly useful, a net win in a majority of applications.

To me, at least, this separation is also very useful at a more semantic level: it helps me *think* about code. I hold actions and calculations mentally distinct and Haskell adroitly reflects this distinction. I tend to organize my code along these lines even in other languages that do not distinguish between the two directly.

Our pure code lives in an idyllic world free from hidden dependencies and complexity, and we can still splash in a dash of state—or any other sort of effect—at will.

Moreover, these effects we splash in are now first-class citizens. You can write functions that depend on having access to a particular effect or ones that are polymorphic over their capabilities. You have fine control over exactly what the "effect" is: you could have just state or variables or IO or a restricted subset of IO or...

This fine control over effects isn't useful just for making your code more maintainable: it also gives us new capabilities. The "halo" application is concurrency and parallelism, where Haskell libraries really shine. For concurrency, things like STM would be *tricky*, at best, without tight controls over IO; for parallelism, the same is true of Haskell's strategies which parallelize code *deterministically*---they're guaranteed not to change the semantics, just the running time.

[^2]: It can, however, still cause visible *performance* issues, which is why performance optimization is often cited as one of the hardest aspects of practical Haskell.
