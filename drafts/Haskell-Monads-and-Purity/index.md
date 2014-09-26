---
title: Haskell, Monads and Purity
author: Tikhon Jelvis
---

I believe the notion that Haskell uses "monads" to enforce purity is rather misleading. It has certainly caused quite a bit of confusion! It's very much like saying we use "rings to do arithmetic". We don't! We use numbers, which just happen to form a ring with arithmetic operations. The important idea is the *number*, not the ring. Moreover, plenty of rings don't have anything to do with arithmetic.

Similarly, in Haskell, we use types to deal with effects. In particular, we use `IO` and `State` and `ST`, all of which happen to form monads. But it is still much better to say we use "the `IO` type" to do IO than to say we use "monads" to do IO.

Now, how does this work? Well, I'll give you one view on the matter.

Perhaps the most important philosophical idea is that Haskell does *not* "enforce" purity. Rather, Haskell as a language simply does not include any notion of effects. Haskell is exclusively about **evaluation**: going from an expression like `1 + 2` to `3`. This has *no* provisions for side effects; they simply don't make any sense in this context^1.

Side effects and mutation are not prohibited; they simply weren't added in the first place. But let's say that, on a flight of fancy, you decide you really want some mutable state in your otherwise-happy Haskell code. How can you do this? Here's one option: implement the state machinery yourself. You can do this by creating a little sub-language and writing an interpreter for it. The language itself is actually just a data type; the interpreter is just a normal function which takes care of evaluating the stateful program and maintaining the state.

Let's call our language's type `State`. A single `State` "action" has some value as the result, so it needs a type parameter: `State a`. For the sake of simplicity, let's imagine you only ever want a single piece of state—an `Int`. 

So what sorts of actions do we need to support? One thing we want to do is *ignore* the current state: if we have an `a` already, we want to have a `State a` which just evaluate to that. We also want some way to get and set the state. Getting should just evaluate to the current value of the state; setting should take the new value to set. Since setting does not have a meaningful return value, we can replace it with `()`. Three basic actions: **get** the state, **set** the state and **return** some value:

    get ∷ State Int
    set ∷ Int → State Int
    return ∷ a → State a
    
However, these actions by themselves are rather boring. We need some way to string them together—some way to *compose* actions. The simplest way is to just sequence two actions: run one, get the new state and run the next one. We can call this an infix `>>`:

    (>>) ∷ State a → State b → State b
    
When our interpreter sees a composed action like this, it will just evaluate the left one, get the resulting state and evaluate the second one using this new state. For example, `set 10 >> get` will result in `10`. However, this is still missing something. We can't really use the value of the state in interesting ways. In fact, we can't even write a simple program to increment the state value! We need some way to use arbitrary Haskell logic inside of our stateful programs.

How do we do this? Well, since we want to use arbitrary logic, we're looking for some sort of function. Since Haskell only knows how to work with `a` values rather than `State a` values, this function should take a `a`. Since we want to use this logic to set the state, this function should return a state action. So the type we want looks something like this:

    a → State b
    
This function has an interesting "shape" (for lack of a better word). Since we're going from a normal value (`a`) to a state action (`State b`), we're injecting normal Haskell into our state sub-language. Since we're going from `a` to `b`, we're including *arbitrary* logic in the function. We can now use this to write an incrementing function:

    addToState x = set (x + 1)
    
Now, how do we intersperse this sort of function with our normal state actions? Well, we want to take a state action, one of these functions, combine them and get the result. The type we want, then, is:

    State a → (a → State b) → State b
    
When our interpreter sees this, it will evaluate the lefthand side, plug the result into the righthand function, get the new state action and run it with the updated state. Using this, we can combine `get` and `addToState`, which will give us a program that increments our current state by `1`. We can use this approach to execute arbitrary logic using our state. We can call this sort of composition `>>=`, which looks like a fancier version of our earlier `>>`:

    (>>=) ∷ State a → (a → State b) → State b
    
Using this `>>=` operator, we can write the increment program:

    increment = get >>= addToState
    
and if we want to increment three times in a row, we can do this:

    increment3 = increment >> increment >> increment
    
It's important to note that the `>>=` doesn't *do* anything; it just produces some data structure which contains both the lefthand side and the function. It's our interpreter function that ever does anything with this. So `increment` is just a normal value of the type `State ()`; nothing special. This means that the order in which our expressions get evaluated does not affect how our state works—we're just evaluating an AST; the state effects are all managed by the interpreter function.

Now whenever we want to use some mutable state, we just write our program using this `State a` type, combining the disparate parts using `>>=`. Then, to actually use this, we invoke our interpreter function. This function looks at the `State a` value and evaluates it, threading the changing state through each part. Unfortunately, using `>>=` and `>>` gets rather ugly, so we also want to throw in some syntax sugar to make using this type look like a normal imperative program. That will make programming in this stateful sub-language bearable.

We've added state to a language where it normally simply does not exist. All using normal functions and data types, with just a bit of syntax sugar to help the medicine go down. Nifty. But not really *magical*.

By now, you've probably realized that `return` and `>>=` make `State` a monad. The syntax sugar, of course, is do-notation. But if you're interested in how we get mutable state, this doesn't tell you very much. Instead, the relevant bits are what the `State a` type looks like (it's basically an AST of commands) and what the interpreter does. The fact that it's a monad is useful, but does not entirely characterize the `State` type.

Sure, we can add state by producing a little AST and explicitly evaluating it. But it's easy to model state because it's an entirely internal phenomenon to the program. But how do we deal with *external* effects—IO? The basic idea is the same: we assemble a program and run it through an interpreter which manages the effects. The main difference is that this interpreter has to do some deep magic to get the actual effects: it has to talk to the operating system and thus the hardware directly. You simply can't do this within normal Haskell because Haskell only knows about evaluating expressions. But really, this requires magic because it *is* magic—ultimately, it's built right into the hardware! 

In a normal Haskell program, the magical interpreter is the runtime system. A Haskell program has a `main` value which is an IO action. When you run this program, what you're actually doing is evaluating `main` and then giving it to the runtime system to interpret. The runtime system then runs the effects, intoning the appropriate incantations to make things happen in the real world.

The `IO` type is just something the compiler and runtime system understand and run. It is a monad in basically the same way as our `State` type above. Conceptually, the only difference is in how it gets interpreted.

In reality, of course, things aren't implemented like this. But it's a good way to think about it. 

So the final verdict: Haskell does not "use monads to manage effects". Rather, Haskell has types like `IO` to manage effects, and `IO` happens to form a monad. Moreover, it turns out that the monad operations are very useful to write actual programs by composing `IO` actions.

^1: This is why `unsafePerformIO` is unsafe: it's completely foreign to the programming model.