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
       AExp ::&= \text{x} \\
           &|\quad n \\
           &|\quad AExp + AExp  \\
           &|\quad AExp - AExp  \\
           &|\quad AExp \times AExp  \\
           &|\quad AExp \div AExp
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
       BExp ::&= \text{true} \\
           &|\quad \text{false} \\
           &|\quad AExp \le AExp \\
           &|\quad AExp = AExp \\
           &|\quad BExp \lor BExp \\
           &|\quad BExp \land BExp \\
           &|\quad !BExp
      \end{align}
  \]
  
This grammar translates to an algebraic data type exactly the same way as `AExp`.

### Commands

Commands are a bit more interesting: since IMP is a simple *imperative* language, the whole program is structured as a bunch of statements one after the other.

  \[ \begin{align}
       cmd ::&= \text{skip} \\
          &|\quad \text{x} \gets AExp & \text{assignment} \\
          &|\quad cmd ; cmd & \text{seq} \\
          &|\quad \text{if}\ BExp\ \text{then}\ cmd\ \text{else}\ cmd & \text{if} \\
          &|\quad \text{while}\ BExp\ cmd & \text{while} \\
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

An alternative encoding is to replace `Skip` and `Seq` with a list of commands (ie `Block [Cmd]`). Think about it this way: `skip` is `[]` and `seq` is `:`. This would probably make the code a bit neater, but I kept `Skip` and `Seq` explicit to make the connection between the abstract syntax and the `Cmd` type more explicit; it also fits better with how we'll define IMP's operational semantics.

The `AExp`, `BExp` and `Cmd` types---and the grammars they represent---provide a skeleton for everything else we'll be doing: writing an interpreter, talking about operational semantics and compiling to Z3 formulas. The rest of the code is very type-directed.

</div>
<div class="content">

## Operational Semantics

As I alluded to earlier, IMP is used as a teaching language because it has simple **operational semantics**. Operational semantics are a way to specify what a program *means* based on *how it would be interpreted*. One way to think about it is that we're formally expressing an interpreter for the language using logical inference rules.

I'll go through the semantics (based loosely on [notes][imp-semantics] by Andrew Myers) rule by rule, including how to translate them to code. At the end, we'll have all the pieces we need for a simple interpreter.

The semantics naturally follow the *shape* of the syntax and our interpreter, in turn, will naturally follow the shape of our types. In particular, I'll break the semantics up into sections for arithmetic, booleans and commands, and the interpreter into three functions split the same way.

IMP is an imperative program and we have to keep track of the state at each step, denoted as \(\sigma\) in the inference rules. Each step is an arrow (\(\to\)) which takes an expression and a state (\(\langle e, \sigma\rangle\)) to some result.

Each rule is structured as an if-then: the part *above* the line is a condition and the part *below* the line is the result. Many results don't have additional conditions like this, so the top part is empty.

\[
\frac{\text{condition}}{\text{result}}
\]

For the sake of simplicity, we'll assume all variables are initialized to \(0\) so that we don't have to deal with errors.

### Arithmetic

Arithmetic expressions always evaluate to an integer, and can access the scope.

```haskell
evalAExp :: Scope -> AExp -> Int
```

Literals evaluate to themselves:

\[
\frac{}{\langle n, \sigma\rangle \to n}
\]

```haskell
evalAExp _ (Lit i) = i
```

Variables are looked up in the scope:

\[
\frac{}{\langle x, \sigma\rangle \to \sigma[x]}
\]

```haskell
evalAExp scope (Var name) = scope ! name
```

If arithmetic expressions \(a_1\) and \(a_2\) evaluate to \(n_1\) and \(n_2\) respectively, \(a_1 \mathtt{+}\ a_2\) evaluates to \(n_1 + n_2\):

\[
\frac{\langle a_1, \sigma\rangle \to n_1 \quad \langle a_2, \sigma\rangle \to n_2}{\langle a_1 \mathtt{+}\ a_2, \sigma\rangle \to n_1 + n_2}
\]

```haskell
evalAExp scope (a_1 :+: a_2) = evalAExp scope a_1 + evalAExp scope a_2
```

The rest of the operators work the same way.

### Booleans

Boolean expressions work pretty much exactly like arithmetic expressions, so I won't include the rules or code here. Look at the [`evalBExp`][evalBExp] function in the `Imp` module for the full details.

### Commands

Commands are where things get interesting. Since they don't have a value, they just evaluate to a new state (\(\sigma'\)), a nice simplification over normal languages.

```haskell
evalCmd :: Scope -> Cmd -> Scope
```

\(\text{skip}\) does nothing.

\[
\frac{}{\langle\text{skip}\ , \sigma\rangle \to \sigma}
\]

```haskell
evalCmd scope Skip = scope
```

Assigning a variable \(x\) to an arithmetic expression \(a\) evaluates \(a\) and updates the scope:

\[
\frac{\langle a, \sigma\rangle \to n}{\langle x \gets a, \sigma\rangle \to \sigma [x := n]}
\]

```haskell
evalCmd scope (Set name aexp) = set scope name (evalAExp scope aexp)
```

A sequence of two steps runs both commands one after the other, passing the updated state to the second command:

\[
\frac{\langle c_1, \sigma \rangle \to \sigma' \quad \langle c_2, \sigma'\rangle \to \sigma''}{\langle c_1; c_2, \sigma\rangle \to \sigma''}
\]

```haskell
evalCmd scope (Seq c_1 c_2) = let scope' = evalCmd scope c_1 in
                              evalCmd scope' c_2
```

An if-statement evaluates its condition and then chooses the appropriate branch. It's easier written as two rules, one for each branch. (For simplicity all ifs have else statements and an empty branch is signified with \(\text{skip}\).)

\[
\frac{\langle b, \sigma\rangle \to \text{true} \quad \langle c_1, \sigma\rangle \to \sigma'}{\langle \text{if}\ b\ \text{then}\ c_1\ \text{else}\ c_2, \sigma\rangle \to \sigma'}
\]
\[
\frac{\langle b, \sigma\rangle \to \text{false} \quad \langle c_2, \sigma\rangle \to \sigma'}{\langle \text{if}\ b\ \text{then}\ c_1\ \text{else}\ c_2, \sigma\rangle \to \sigma'}
\]

```haskell
evalCmd scope (If cond c_1 c_2) = if evalBExp scope cond
                                    then evalCmd scope c_1
                                    else evalCmd scope c_2
```

[imp-semantics]: http://www.cs.cornell.edu/courses/cs6110/2013sp/lectures/lec05-sp13.pdf
