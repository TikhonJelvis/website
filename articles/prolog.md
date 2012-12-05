---
title: Write Yourself a Prolog
author: Tikhon Jelvis
---

<div class="content">

# Write Yourself a Prolog (working title)

Let's implement a simple Prolog interpreter in Haskell---this is a very effective and, hopefully, fun way to learn about both languages.

This tutorial is going to cover how to develop a relatively simple and small Prolog interpreter in Haskell. We'll cover the semantics of Prolog as well as more quotidien things like parsing and dealing with user input.

</div>
<div class="content">

## Prologue: Prolog

Before we can write any code, we have to have some understanding of what we're to do! So the first step is understanding the basics of Prolog. It isn't important to understand everything immediately; rather, you just need the general idea and some of the vocabulary.

If you want a more thorough introduction to Prolog, you can look at
the online course [Learn Prolog Now](http://www.learnprolognow.org/) or the [Prolog WikiBook](http://en.wikibooks.org/wiki/Prolog).

### Logic Programming

Prolog is a **logic** programming language. It operates in a fundamentally different way from imperative (Java, Python) and functional (Haskell, OCaml) languages.

The core abstraction in a logic programming language is an inference rule. These rules define constraints on variables. (A constraint could be something like "A and B are equal".) To run a program, you just find a set of assignments to the variables that obeys the constraints.

One interesting idea is that there is *more* than one such assignment. Thus a Prolog program can have *multiple* results for a single input, which is very different from imperative or functional programs. 

A real Prolog interpreter can do some other things, like arithmetic and file IO. However, in the interests of simplicity, we will only implement a subset of the full language.

### Structure

Let's start by looking at the structure of a Prolog program.

A Prolog program is a set of logical **rules** defining **predicates** that operate on **atoms**.


An atom is just some indivisible value, a single item of data. We will support one kind of atom--a symbol. A symbol's value is just its name, much like symbols in other languages like Scheme and Ruby. Atoms have to start with lower-case letters, so the following are possible atoms: `a`, `foo`, `fooBar`. In each case, the atom's name is also its value. Real Prolog interpreters support other kinds of data like strings and  numbers; we will omit these for simplicity.

A predicate is a relation over atoms. Each predicate accepts some particular number of atoms; given the right number of arguments, the predicate either holds or doesn't hold. As an example, we can imagine a predicate called `eq` representing equality--it holds on two atoms only if they are equal. So `eq(x, x)` would hold and `eq(x, y)` would not. A predicate's name has to start with a lower-case letter.

We could define predicates by listing out all the atoms they hold on. For example, for `eq`, we could write:

```prolog
eq(a, a).
eq(b, b).
eq(c, c).
...
```
    
However, this is somewhat tedious, especially for infinite relations like `eq`. Happily, we can write rules to define predicates more generally. The simplest form of rule is the same as the above; we can call this a "fact". To express more general rules, we need to introduce the idea of a variable. A variable is just a name that can correspond to *any* atom. Variable names have to start with upper-case letters. This lets us write `eq` very succinctly:

```prolog
eq(X, X).
```
    
This statement just says that *any* atom `X` is equal to itself. One way to think of this is by looking at `X` as a universally qualified variable: `∀X. eq(X, X)`. Now, if we want to write relations involving all possible values of `X`, we are set. However, for most interesting programs, we want to constrain variables somehow. In logical expressions, you would use implication for this: `∀x. P(x) → Q(x)`. So we need to introduce some notion of implication to our rules. We do  this using the `:-` operator. If you squint, it looks like `←`. Now we can write rules like:

```prolog
foo(X, Y) :- bar(X), eq(X, Y).
```
    
This says that `foo` holds for any `X` for which `bar` holds and any `Y` equal to that `X`. We can also have multiple rules for a single predicate, as before:

```prolog
foo(baz, qux).
foo(X, Y) :- bar(X), eq(X, Y).
```
<!-- ## Types -->

<!-- So, a Prolog program is made up of three components: atoms, variables --> <!-- and predicates. These terms are then combined into rules. We can use --> <!-- this information to write our types. -->

<!-- Types are usually the best place to start a Haskell program. Designing --> <!-- the types helps codify what you expect your program to do. I find it --> <!-- helps clarify my own thinking and gives a rough sketch of the --> <!-- program's ultimate structure. -->

<!-- We start by defining a type for terms, which can be atoms, variables --> <!-- or predicates: -->

<!--     data Term = Atom String -->
<!--               | Var String -->
<!--               | Pred Predicate deriving (Show, Eq) -->

<!-- Here a predicate refers to a name with its arguments--the whole --> <!-- expression `eq(x, x).` rather than just `eq`. So a predicate is a name --> <!-- followed by some number of terms: -->

<!--     data Predicate = Predicate String [Term] deriving (Show, Eq) -->
    
<!-- Now we just need to define a type for rules. A rule has to start with --> <!-- some predicate (called the "goal") and can have an optional body of --> <!-- other predicates (after the `:-`). The type looks like this: -->

<!--     data Rule = Rule Predicate [Predicate] deriving (Show, Eq) -->
    
<!-- These types sketch out the general structure of Prolog programs. This --> <!-- will make designing the rest of the interpreter simpler. As we add --> <!-- functionality to the interpreter, we will need to revise these types; --> <!-- however, they offer a good starting point. -->

</div>