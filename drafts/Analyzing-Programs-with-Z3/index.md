---
title: Analyzing Programs with Z3
author: Tikhon Jelvis
---

< Intro paragraph—SMT solvers are wonderful, example with simple language… blah, blah. >

</div>
<div class="content">

# IMP

Imp is a bare-bones imperative language used as a teaching tool. It's too limited for real programming, but its simple operational semantics make it easy to analyze programs by hand---or in our case, with an SMT solver.

Lets start by looking at how IMP works and implementing an interpreter for it in Haskell.

## Abstract Syntax

The place to start with pretty much any language is its abstract syntax—what can it express? The abstract syntax is specified as a series of grammars which translate almost verbatim to algebraic data types. It's a particularly apropos starting point in Haskell, since types are the first thing we write in most projects.

IMP is broken up into three parts: arithmetic expressions, boolean expressions and commands (statements). A notable simplification compared to normal imperative languages is that expressions can't have side-effects and commands don't evaluate to a value, making the split between the two even more extreme. I wouldn't like this property in a language *to use*, but it's useful in a language *to analyze*.

Arithmetic is pretty much what you'd expect, with the four basic operators, numeric literals and variables:

  \[ \begin{align}
       aexp ::&= \text{x} \\
           &|\quad n \\
           &|\quad aexp + aexp  \\
           &|\quad aexp - aexp  \\
           &|\quad aexp * aexp  \\
           &|\quad aexp / aexp
      \end{align}
  \]

Here's how this looks translated to an algebraic data type:

```haskell
data AExp = Lit Int
          | Var Name
          | AExp :+: AExp
          | AExp :-: AExp
          | AExp :*: AExp
          | AExp :/: AExp deriving (Show, Eq)
```

`:+:`, `:-:`, `:*:` and `:/:` are *infix constructors*---in Haskell, starting an infix identifier with `:` is like starting a normal identifier with a capital letter. This isn't necessary but makes values of the type a bit easier to write by hand. For example, here's what the expression `a + (7 / b)` would look:

```haskell
Var "a" :+: (Lit 7 :/: Var "b")
```

Boolean expressions are similar. For simplicity, the language does not have boolean variables, so we only have literals and operators:

  \[ \begin{align}
       bexp ::&= \text{true} \\
           &|\quad \text{false} \\
           &|\quad aexp \le aexp \\
           &|\quad aexp = aexp \\
           &|\quad bexp \lor bexp \\
           &|\quad bexp \land bexp \\
           &|\quad !bexp
      \end{align}
  \]

Commands are a bit more interesting: apart from our normal control flow, we have `skip` to allow for empty blocks and `seq` (\(;\)) to sequence two commands in a row for blocks with multiple statements.

  \[ \begin{align}
       cmd ::&= \text{skip} \\
          &|\quad \text{x} \gets aexp & \text{assignment} \\
          &|\quad cmd ; cmd & \text{seq} \\
          &|\quad \text{if}\ bexp\ \text{then}\ cmd\ \text{else}\ cmd & \text{if} \\
          &|\quad \text{while}\ bexp\ cmd & \text{while} \\
     \end{align}
  \]

Commands and boolean expressions translate to algebraic types in the same way as arithmetic expressions; in my code, I called them `BExp` and `Cmd` respectively.

These types are important because they provide the *skeleton* for everything else we: operational semantics, writing an interpreter and compiling to a Z3 formula.

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
