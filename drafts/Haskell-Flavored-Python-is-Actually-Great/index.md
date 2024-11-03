---
title: Haskell-Flavored Python is Actually Great
author: Tikhon Jelvis
---

Python has evolved massively since I first saw the language. You can now write typed Python with immutable records, unions and pattern matching—and it's actually great.

I'm sure "Haskell-flavored Python" sounds like a boogey man that died-in-the-wool Pythonistas use to scare their kids, but it's actually a simple, effective and *natural* programming style with newer versions of Python. Python has been introducing functional programming features for years now, and the addition of [structural pattern matching][pattern-matching] in 3.10 is the final piece of the puzzle to make Python a credible functional language.

[pattern-matching]: https://docs.python.org/3/reference/compound_stmts.html#match

Python now has all the ingredients for algebraic data types à la Haskell or ML:

  - immutable dataclasses for defining record types
  - type hints including union types
  - pattern matching
  
Here's a Haskell data type for a simple language of arithmetic expressions:

``` haskell
data Expr = Literal Integer
          | Variable String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
   deriving (Show, Eq)
```

And here's how we can write the same type in Python 3.12:[^type-alias]

[^type-alias]: Python 3.12 introduced the type statement for type aliases; for older versions, we'd use `Exper: TypeAlias = ...` instead.:

``` python
from __future__ import annotations

from dataclasses import dataclass
from typing import NewTy

type Expr = Literal | Variable | Add | Sub | Mul | Div

Literal = NewType("Literal", int)

Variable = NewType("Variable", str)

@dataclass(frozen=True)
class Add:
    l: Expr
    r: Expr
    
# same for Sub, Mul and Div
```

A little bit more verbose, sure, but still clear. For more realistic examples where each dataclass stands on its own—including its own documentation and, potentially, methods—the extra verbosity practically disappears.

Once you define the types, you can pattern match on them and even get warning if you miss a constructor:

``` python
def eval(e: Expr, scope: Dict[Variable, int]) -> int:
    match e:
        case Variable(name):
            return scope.lookup(name) or 0
        case Literal(n):
            return n
        case Add(l, r):
            return eval(l) + eval(r)
        case Sub(l, r):
            return eval(l) - eval(r)
        case Mul(l, r):
            return eval(l) * eval(r)
        case Div(l, r):
            return eval(l) // eval(r)
```

This example is just meant to be an illustration.

You can also define union types that include preexisting Python types:

``` python
from __future__ import annotations

from typing import Dict, Sequence

type JSONValue = None | bool | str | int | float
type JSONObject = Dict[str, JSON]
type JSON = JSONValue | Sequence[JSON] | Dict[str, JSON]
```

and then write functions matching against them:

``` python
def process_json(j: JSON):
    match j:
        case {"name": str(name)}:
            print(f"Hello, {name}")
        case {"first": str(first), "last": str(last)}:
            print(f"Hello, {first} {last}")
        case invalid:
            raise ValueError(f"Unexpected JSON: {invalid}")
```

I've written some pretty fiddly JSON-processing code both with and without pattern matching, and being able to match on the JSON value made the code massively easier to both write and maintain.

Writing Python with type hints, immutable data classes and pattern matching is a totally different experience than writing "normal" Python. It still feels like Python, just with more support from the language—the code is clearer and easier to read at a glance, and the warnings you get from your types help you catch a lot of errors that would otherwise crop up at runtime.
