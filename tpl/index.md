---
title: TPL
author: Tikhon Jelvis
---

<div class="content">
<h1 class="tpl"> tpl </h1>

TPL is a simple dynamically typed programming language I designed and implemented, largely for fun. Creating a language is very fascinating---you feel virtually omnipotent because you have complete control over everything.

NOTE: This language is a work in progress. At any given time, this text is probably outdated and the interpreter probably doesn't work. Having to no real users gives me a lot of freedom :).

You can get the source on [GitHub](https://github.com/TikhonJelvis/tpl). There is a more in-depth [User Guide](guide.html), but it isn't completely up-to-date at the moment. Also, unless you want to actually write TPL code, it isn't terribly interesting.

## Design

The core design goals were flexibility and simplicity. I wanted to create a small language based on simple ideas that was nonetheless sufficient to express things like flow control (loops, if statements), OO primitives (classes) and such as libraries rather than core language features.

The language was not designed with maintainability or performance in mind: I sacrificed everything in order to unify disparate concepts and make everything as simple and flexible as possible. I think designing a completely pragmatic language, worrying about performance, maintainability, accessibility and simplicity simultaneously, would not be nearly as much fun as designing something wantonly flexible.

## Features

TPL has several features which differentiate it from other silly scripting languages like Python or JavaScript.

### Deferred Values

The first feature is laziness. In TPL, you can defer an expression so that it does not get evaluated immediately. This is really the same as wrapping it in a lambda in a different language; however, the user of the variable does not have to know if it is wrapped or not.

As an example, in `x := 1 + y`, `1 + y` gets evaluated and that value is given to `x`; if we defer it with `x := λ → 1 + y`, `x` will be a function. So `1 + y` will only get evaluated when you *use* `x`. However, since the syntax for just looking up a variable and calling a function with no arguments is the same, the caller does not need to know whether `x` is deferred or not.

This approach is simple, but has two problems. For one, the `λ → ...` syntax is ugly. For two, we cannot reference `x` without calling it, which could be useful. Happily, we can solve both of these problems in one go: we add some syntax to defer an expression. Now, instead of `λ → 1 + y`, we can write `$(1 + y)`; if we want to reference `x` without calling it, we just wrap it again: `$x`. This has very little syntactic and conceptual overhead while giving you a lot of control over when an expression gets evaluated.

Since the expression will get evaluated *every* time `x` is used, this also gives you a very lightweight way to do simple reactive programming. In particular, imagine this:

    x := 1
    y := $(2 * x)
    y -- y is 2
    x <- 10
    y -- y is 20
    
This can also be used by frameworks. For example, a variable `mousePosition` could always be a point corresponding to the current mouse position. 

### Lazy Arguments

Apart from being able to defer an expression, we can also control when a function's arguments get evaluated. This is also a very useful feature---we can now implement functions like short-circuiting and. In other languages (mainly Lisps), a similar effect can be had with macros. While macros can do significantly more than just controlling evaluation order, I believe my approach is simpler. It is also nice that such functions are just that---functions. They behave just like normal functions except that they do not evaluate certain arguments unless they have to.

The syntax for this is simple: we use a `$` in front of the parameter's name. So the following `const` function would never evaluate its second argument:

    const x $y := x
    
Conceptually, calling this function `const 1 (x + y)` is the same as calling a normal function and deferring the lazy argument: `const 1 $(x + y)`. This is why the `$` syntax is used. Internally, the argument's value is just deferred before being passed into the function.

### Objects

Another interesting feature is the object system. At its core, the design was heavily influenced by JavaScript's objects and Lua's tables. The main idea is the same: an object is just a mapping from keys to values which can "inherit" from another object. The child object just looks up any keys it doesn't have itself in its parent object.

Additionally, instead of inheriting from a parent, the child object can have a custom function for looking up unknown properties. This is very similar to Python's `defaultdict`.  You can also have a custom function for setting values.

All this makes objects very lightweight---you have no classes but do effectively have prototype-based inheritance---and very customizable. However, this, by itself, is not terribly interesting: Lua essentially support exactly the same features already.

The interesting trick is different: I realized that lexical scoping and a prototype-based object system actually behave exactly the same way. "Environments" or "scopes" are just like objects that inherit from their parent scope. So I decided to combine the two concepts---in TPL, objects and scopes are actually the same thing. In fact, you can think of objects just as a reification of scopes.

So you can access the current scope you're working in as an object, and you can take an arbitrary object and set it as the current scope. Moreover, given a closure, you can access and set its closed environment in the same way.

This means that you have full programmatic control over local variables at really any level of the hierarchy. As seen above, you can even have a custom function for looking up unknown variables!

To make this more general, you also have access to the *dynamic* scope---the scope at the call site. This lets you implement things like dynamic scope or having a function add variables to the scope of its caller. (This seems like a useless feature, but it is great for things like pattern matching where you want to bind new local identifiers).

### Customizablity

All these features can be put together to fulfill my original goal: you can write control structures as libraries. Let's look at a concrete example, a Python-style for-loop. It won't look the same as Python because TPL needs parentheses, so our goal syntax is something like this:

    for i in (1..10) (
      temp := i + 10
      print (i + temp)
    )
    
To make defining things like this possible, we need to take advantage of some built-in functions: `exprToString`, `with`, `set` and `get`. The first just takes an deferred expression like `$x` and returns it as a string (`"x"` in this case). `with` replaces a closures environment with the given object. `set` sets a key in an object using a string as the name. `get` lets you look up local variables via a string name, which allows you to access special values like `*context*` which is the current scope of the *caller*.

So here is how the definition of the for-loop looks:

    for $x $in ls $body := (
      context := get "*context*"
      map (λ item → with (context.set (exprToString x) item) body) ls
      null
    )
    
In the end we return `null` because a for-loop should not really have a value.

You can write things as fundamental as loops in TPL itself. This also means that if you want something not included in the language---like a traditional class system, for example---it should be possible to implement it yourself. I think this sort of flexibility is very important: it gives the language room to grow. 