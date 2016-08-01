---
title: Analyzing Programs with Z3
author: Tikhon Jelvis
---

< Intro paragraph—SMT solvers are wonderful, example with simple language… blah, blah. >

</div>
<div class="content">

# IMP

IMP is a bare-bones imperative language used as a teaching tool. It's too limited for real programming, but its simple operational semantics make it easy to analyze programs by hand---or in our case, with an SMT solver.

Let's start by looking at how IMP works and implementing an interpreter for it in Haskell.

## Abstract Syntax

The place to start with pretty much any language is its abstract syntax—what can it express? The abstract syntax is specified as a series of grammars which translate almost verbatim to algebraic data types. It's a particularly apropos starting point in Haskell, since types are often the first thing we write anyhow.

Note that this does not need to correspond one-to-one with the actual syntax of the language---it just needs to capture the core constructs of the language. This is why it's called an "abstract syntax" as opposed to a "concrete syntax" which you would use to write programs. The abstract syntax can omit unneeded details like how programs are parsed or constructs that are pure "syntax sugar"[^syntax-sugar].

[^syntax-sugar]: Syntax sugar generally refers to syntactic constructs that can be expressed purely in terms of *other* language constructs. We can get rid of syntactic sugar by transforming it into core language constructs before we analyze or run the code---this step is called "desugaring". 

    The for-in loop in many languages is a good example. It's syntax sugar that can be transformed into a while loop and an iterator. That is,

    ```java
    for (x in xs) { 
      <body> 
    }
    ```
    
    might desugar into:
    
    ```java
    iter = xs.iterator()
    while (iter.hasNext()) {
      x = iter.getNext()
      <body>
    }
    ```

    Syntax sugar is useful because it provides convenience features to the programmer without making the language substantially more complex. Since the for-in loop's semantics are defined in terms of existing features, tools and compilers don't need to know about how it works (except maybe for nice error messages); instead, they can just work on the program after desugaring.

IMP is naturally broken up into expressions and commands (statements). A notable simplification compared to normal imperative languages is that expressions can't have side-effects and commands don't evaluate to a value, neatly delineating the two. I wouldn't like this property in a language *to use*, but it's useful in a language *to analyze*.

### Expressions

For simplicity, we only care about two kinds of expressions:

  * **arithmetic expressions** simplify to a number and can be bound to variables
  * **boolean expressions** simplify to a true or false value and are used for control flow, but can't be bound to a variable
  
Again, this is a simplification over real languages: instead of supporting different kinds of expressions equally, perhaps with a type system, we have two syntactically distinct expressions which can only be used in mutually exclusive contexts.

Arithmetic is pretty much what you'd expect, with basic operators, numeric literals and variables:

  \[ \begin{align}
       A ::&= \text{x} \\
           &|\quad n \\
           &|\quad A + A  \\
           &|\quad A - A  \\
           &|\quad A \times A  \\
           &|\quad A \div A
      \end{align}
  \]

Here's how this translates to an algebraic data type (ADT):

```haskell
data AExp = Lit Int
          | Var Name
          | AExp :+: AExp
          | AExp :-: AExp
          | AExp :*: AExp
          | AExp :/: AExp 
            deriving (Show, Eq)
```

`:+:`, `:-:`, `:*:` and `:/:` are *infix constructors*---in Haskell, starting an infix identifier with `:` is like starting a normal identifier with a capital letter. This isn't necessary but makes values of the type a bit easier to write by hand. For example, here's what the expression `a + (7 / b)` would look:

```haskell
Var "a" :+: (Lit 7 :/: Var "b")
```

The algebraic data type is a reasonable reflection of how we abstractly think of the language's syntax. It comes with a natural way to decompose expressions in the language---pattern matching. ADTs are one of the main reasons people often recommend languages like Haskell and ML for working on compilers.

Another advantage of using an ADT is that the compiler knows all the possible cases in the type. If we enable the right warning (`-fwarn-incomplete-patterns`[^patterns-warning]), it will tell us if we forget a case as we're defining a function.

[^patterns-warning]: Also enabled by the `-W` and `-Wall` flags. You can enable a flag for a single session from inside `ghci`:

    ```
    λ> :set -fwarn-incomplete-patterns
    ```
    
    You can also add this to your `.cabal` file or your global GHCi config file (`~/.ghci` on Linux systems).

    Personally, I think this warning should be on by default (as they are in OCaml). It's incredibly useful, especially on larger projects.

Boolean expressions are similar. For simplicity, the language does not have boolean variables, so we only have literals and operators:

  \[ \begin{align}
       B ::&= \text{true} \\
           &|\quad \text{false} \\
           &|\quad A \le A \\
           &|\quad A = A \\
           &|\quad B \lor B \\
           &|\quad B \land B \\
           &|\quad !B
      \end{align}
  \]
  
This grammar translates to an algebraic data type exactly the same way as `AExp`.

### Commands

Commands are a bit more interesting: since IMP is a simple *imperative* language, the whole program is structured as a bunch of statements one after the other.

  \[ \begin{align}
       Cmd ::&= \text{skip} \\
          &|\quad \text{x} \gets A & \text{assignment} \\
          &|\quad Cmd ; Cmd & \text{seq} \\
          &|\quad \text{if}\ B\ \text{then}\ Cmd\ \text{else}\ Cmd & \text{if} \\
          &|\quad \text{while}\ B\ Cmd & \text{while} \\
     \end{align}
  \]
  
We have some normal control flow and an assignment statement to define and update variables---nothing surprising there. The two other constructs are a bit more subtle: `skip` exists to represent empty code blocks and `seq` (`;`) *sequences* two commands one after the other. A code block with multiple commands is represented as those commands chained together with `seq`.

Here's what this looks as an algebraic data type:

```haskell
data Cmd = Skip
         | Set Name AExp
         | Seq Cmd Cmd
         | If BExp Cmd Cmd
         | While BExp Cmd
           deriving (Show, Eq)
```

An alternative encoding is to replace `Skip` and `Seq` with a list of commands (ie `Block [Cmd]`). Think about it this way: `skip` is `[]` and `seq` is `:`. This would probably make the resulting code a bit neater, but I kept `Skip` and `Seq` explicit to make the connection between the abstract syntax and the `Cmd` type more explicit; it also fits better with how we'll define IMP's operational semantics.

The `AExp`, `BExp` and `Cmd` types---and the grammars they represent---provide a skeleton for everything else we'll be doing: writing an interpreter, talking about operational semantics and compiling to Z3 formulas. This is the sort of code that is naturally type-directed.

</div>
<div class="content">

## Operational Semantics

As I alluded to earlier, IMP is used as a teaching language because it has simple **operational semantics**. Operational semantics are a way to specify what a program *means* based on *how it runs*. One way to think about it is that we're formally expressing an interpreter for the language using logical inference rules. IMP's operational semantics are a great place to start because they let us reason about IMP code and translate neatly to both an IMP interpreter and a compiler to Z3 formulas.

I'll go through the semantics (based loosely on [notes][imp-semantics] by Andrew Myers) rule by rule, including how to translate them to code. At the end, we'll have all the pieces we need for a simple interpreter.

The semantics naturally follow the *shape* of the syntax and our interpreter follows the *shape* of our types. In particular, I'll group the semantics into three sections for arithmetic, booleans and commands, and the interpreter into three functions split the same way.

IMP is an imperative program and we have to keep track of the state at each step, denoted as \(\sigma\) in the inference rules. Each step is an arrow (\(\to\)) which takes an expression and a state (\(\langle e, \sigma\rangle\)) to some result.

Each rule is structured as an if-then: the part *above* the line is a condition and the part *below* the line is the result. Many results don't have additional conditions like this, so the top part is empty.

\[
\frac{\text{condition}}{\text{result}}
\]

For the sake of simplicity, we'll assume all variables are initialized to \(0\) so that we don't have to deal with errors.

### Arithmetic

Arithmetic expressions always evaluate to an integer, and might depend on the current state of the program (the scope).

```haskell
aexp :: Scope -> AExp -> Int
```

Literals evaluate to themselves:

\[
\frac{}{\langle n, \sigma\rangle \to n}
\]

```haskell
aexp _ (Lit i) = i
```

Variables are looked up in the scope:

\[
\frac{}{\langle x, \sigma\rangle \to \sigma[x]}
\]

```haskell
aexp scope (Var name) = scope ! name
```

If arithmetic expressions \(a_1\) and \(a_2\) evaluate to \(n_1\) and \(n_2\) respectively, \(a_1 \mathtt{+}\ a_2\) evaluates to \(n_1 + n_2\):

\[
\frac{\langle a_1, \sigma\rangle \to n_1 \quad \langle a_2, \sigma\rangle \to n_2}{\langle a_1 \mathtt{+}\ a_2, \sigma\rangle \to n_1 + n_2}
\]

```haskell
aexp scope (a_1 :+: a_2) = aexp scope a_1 + aexp scope a_2
```

The rest of the operators work the same way.

### Booleans

Boolean expressions work pretty much exactly like arithmetic expressions, so I won't include the rules or code here. Look at the [`bexp`][bexp] function in the `Imp` module for the full details.

<!-- TODO: Add a link to the correct line of code on GitHub (or GitLab?) -->
[bexp]: #

### Commands

Commands are where things get interesting. Commands don't have a value, they can only change the state of the program. In the rules this means a state \(\sigma\) and a command \(c\) evaluate to a new state \(\sigma'\): \(\langle c, \sigma \rangle \to \sigma'\).

```haskell
cmd :: Scope -> Cmd -> Scope
```

\(\text{skip}\) does nothing.

\[
\frac{}{\langle\text{skip}\ , \sigma\rangle \to \sigma}
\]

```haskell
cmd scope Skip = scope
```

Assigning a variable \(x\) to an arithmetic expression \(a\) evaluates \(a\) and updates the scope:

\[
\frac{\langle a, \sigma\rangle \to n}{\langle x \gets a, \sigma\rangle \to \sigma [x := n]}
\]

```haskell
cmd scope (Set name aexp) = set scope name (aexp scope aexp)
```

A sequence of two commands runs the first command and then runs the second command with the updated state.

\[
\frac{\langle c_1, \sigma \rangle \to \sigma' \quad \langle c_2, \sigma'\rangle \to \sigma''}{\langle c_1; c_2, \sigma\rangle \to \sigma''}
\]

```haskell
cmd scope (Seq c_1 c_2) = let scope' = cmd scope c_1 in
                          cmd scope' c_2
```

An if-statement evaluates its condition and then chooses the appropriate branch. It's easier written as two rules, one for each branch. (For simplicity all ifs have else clauses and an empty branch is signified with \(\text{skip}\).)

\[
\frac{\langle b, \sigma\rangle \to \text{true} \quad \langle c_1, \sigma\rangle \to \sigma'}{\langle \text{if}\ b\ \text{then}\ c_1\ \text{else}\ c_2, \sigma\rangle \to \sigma'}
\]
\[
\frac{\langle b, \sigma\rangle \to \text{false} \quad \langle c_2, \sigma\rangle \to \sigma'}{\langle \text{if}\ b\ \text{then}\ c_1\ \text{else}\ c_2, \sigma\rangle \to \sigma'}
\]

```haskell
cmd scope (If cond c_1 c_2) = if bexp scope cond
                                 then cmd scope c_1
                                 else cmd scope c_2
```

</div>
<div class="content">

## Interpreter

The three functions we just defined, taken together, give us a complete interpreter for IMP. It's simple and doesn't handle parsing or errors, but captures all the language's behavior. Here are the three evaluation functions in one place---they're pleasantly short:

```haskell
aexp :: Scope -> AExp -> Int
aexp _ (Lit i)     = i
aexp σ (Var name)  = σ ! name
aexp σ (e₁ :+: e₂) = aexp e₁ + aexp e₂
aexp σ (e₁ :-: e₂) = aexp e₁ - aexp e₂
aexp σ (e₁ :*: e₂) = aexp e₁ * aexp e₂
aexp σ (e₁ :/: e₂) = aexp e₁ `div` aexp e₂

bexp :: Scope -> BExp -> Bool
bexp _ True'  = True
bexp _ False' = False
bexp σ (e₁ :<=: e₂) = aexp e₁ <= aexp e₂
bexp σ (e₁ :==: e₂) = aexp e₁ == aexp e₂
bexp σ (b₁ :|: b₂)  = bexp b₁ || bexp b₂
bexp σ (b₁ :&: b₂)  = bexp b₁ && bexp b₂
bexp σ (Not b)      = not (bexp b)

cmd :: Scope -> Cmd -> Scope
cmd σ Skip                   = σ
cmd σ (Set name e)           = set σ name (aexp e)
cmd σ (Seq c₁ c₂)            = let σ' = cmd σ c₁ in cmd σ' c₂
cmd σ (If cond c₁ c₂)        = 
    if bexp σ cond then cmd σ c₁ else cmd σ c₂
cmd σ loop@(While cond body) = 
    if bexp σ cond then cmd σ (Seq body loop) else σ
```

You can see how the code follows the "shape" of the types: it naturally follows the variants in each type, with each individual case being short and straightforward.

</div>
<div class="content">

# Z3

## SMT

Z3 is a powerful **SMT solver** from Microsoft Research[^license]---it efficiently solves constraint satisfaction problems for certain classes of logical constraints. The actual kinds of constraints are somewhat limited to achieve good performance but they are a really good fit to modeling and reasoning about programming languages.

[^license]: Z3 was recently released under an MIT license, so it's usable for any purpose including commercial work.

### SAT

SMT solvers grew out of efficient SAT solvers---SMT stands for SAT modulo theories. A SAT solver solves **Boolean satisfaction** problems: given a formula over a set of Boolean variables \(x_i\):
\[
(x_1 \lor \lnot x_2) \land (x_1 \lor x_3 \lor \lnot x_4) \land \cdots 
\]
a SAT solver tries to find an assignment that **satisfies** the formula---a value for each \(x_i\) such that the entire formula is true. If such an assignment does not exist, the formula is **unsatisfiable**.

SAT in general is NP-complete, but existing algorithms---most commonly based on backtracking---are fast on most *real-world instances* of the problem. This makes SAT solvers an important practical tool for everything from security research to hardware verification.

### Theories

One downside of SAT is that pure Boolean formulas are not particularly expressive. It's often hard to encode our domain of interest as a Boolean formula and, in the process of encoding, the solver loses high-level information about the domain.

SMT solvers like Z3 plug this gap by extending the SAT paradigm to include other kinds of variables and constraints (called "theories"). We can, for example, write formulas using numeric variables:
\[
x_1 \le 10 \land x_3 \le x_1 + x_2 \land \cdots
\]

There is a wide range of possible theories we could use:

  * **linear real arithmetic**: linear constraints over real numbers
  * **unbounded integers**: arithmetic and constraints of unbounded integers
  * **algebraic numbers**: real numbers that are solutions to polynomials with integral coefficients
  * **bitvectors**: fixed-size words, including bitwise operations---perfect for modeling signed and unsigned variables in programming languages
  * **floating point numbers**: actual IEEE 754 floating point numbers, implemented as bitvectors
  * **arrays**: integer-indexed arrays of values, useful for modeling memory
  * quite a few more

Different SMT solvers support a different subset of all the possible theories, and have vastly different performance on them. Different solvers might be best suited to different tasks, but people tend to default to Z3 because it has top-tier performance across a wider range of theories than any other solver.

For language modeling tasks, I tend to stick exclusively to bitvectors. They can directly model signed or unsigned integers and can easily be adjusted to use fewer bits if we need to improve the performance of a formula. For the sorts of constraints I generate, I've found bitvectors significantly faster than integers or arrays. But don't take this as gospel: I've heard the opposite (unbounded integers being faster than bitvectors) from people working on projects like LiquidHaskell.

In general, it's quite hard to predict SMT performance ahead of time. It's sensitive not only to what theory you use but also to seemingly irrelevant details of how you encode your constraints. In practice, your best bet is to try a few different encodings out if the solver is not performing as well as it should.

</div>
<div class="content">

## Haskell

There are currently two good ways to interact with Z3 from Haskell:

  * [sbv] is a high-level embedded DSL that can generate formulas for multiple SMT solvers
  * [z3] is a set of API bindings specific to Z3
  
I've experimented with SBV in the past and wrote up a general overview of the library for [24 Days of Hackage][sbv-24] a couple years back. While I have qualms about its design---debugging is harder than it has to be and too many problems result in runtime errors---it's pleasant to use and a good way to get familiar with using an SMT solver.

For this project, though, I decided to use the Z3 API bindings instead. The bindings provide a significantly lower-level interface than SBV that corresponds closely to Z3's C API. It's more awkward for interactive use from GHCi but gives more direct access to the solver. As a rough guide, you'd use SBV for solving specific problems and the Z3 API for implementing tools to solve problems---like SBV itself. (Note that SBV does not use the bindings.)

The `z3` package provides a version of every API call wrapped in a `Z3` monad that manages all the state we need to talk with the solver. We'll use this to organize our code, build up formulas and dispatch them to Z3. The `Z3` monad is not quite a dedicated DSL but, for something so simple, it's surprisingly expressive.

</div>
<div class="content">

# Compiling to Z3

Now that we understand IMP's operational semantics and have the code for an interpreter, we'll take that same logic and use it to build a formula that Z3 can understand. The formula will encode the execution of a program *symbolically*, letting us ask Z3 about *all possible executions* of the program.

## Analyzing Programs

A formula representing a program breaks down into three sets of variables:

  * **inputs**: the starting states
  * **outputs**: the results
  * **intermediate states**: the state of the program as it's executing
  
Depending on which variables we solve for, we can answer different questions about the code:

  * setting inputs and solving for outputs gets us an interpreter
  * setting outputs and solving for inputs gets us a reverse interpreter
  * adding constraints over intermediate variables lets us check invariants
  * comparing inputs and outputs for two programs lets us verify they're the same

The one limitation of this approach is that we have to construct a finite, non-recursive formula for Z3 to solve---but programs might loop forever. Trying to model unbounded iteration in general would quickly run into the limitations of Z3 and, indeed, the limitations of *any* tool thanks to the Halting Problem.

Our solution to this limitation is pragmatic and a bit ugly: we just arbitrarily add a cutoff as we're generating the formula. If some execution of the program loops more than we allow, the formula will fail. Any analysis we do like this will be conservative by nature---it'll fail on some programs that should work. Moreover, exactly when it fails depends on how large a formula we generate, a completely arbitrary limit.

That said, no approach can be perfect when modeling Turing-complete languages. In practice, an arbitrary cutoff is actually useful in applications where performance matters---a program which takes too long to run may as well be wrong.


[imp-semantics]: http://www.cs.cornell.edu/courses/cs6110/2013sp/lectures/lec05-sp13.pdf
