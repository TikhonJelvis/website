---
title: Structure your Errors
author: Tikhon Jelvis
published: 2020-12-13 00:31:29
modified: 2020-12-13 00:31:46
---

Recently, I've revisited how I represent errors in code.

I'm working on a command-line tool used across multiple teams and I want to keep its error messages consistent and readable. As the codebase has grown, I've moved from *ad hoc* error strings throughout my code to a structured error type.

![I never want to see this in my software!](./img/generic-error-message.png "An error 
popup with the message “An error occurred.”")

Useful error messages need to:

  * Contain the information and context to diagnose and fix the problem.
  * Provide an intuitive explanation for new users.
  * Format information consistently, in a way that's easy to scan at a glance.

We need 10–20 lines of code for each kind of error to generate error messages that fulfill these goals. In other projects, I would create error message strings directly in the code that first detected or raised an error, but that just does not scale here—it would add too much noise to my code! It's much harder to keep error messages consistent in style when they are so decentralized, and refactoring messages *en masse* becomes a nightmare.

To fix these problems, I've started using dedicated types for my errors. This took some up-front effort but has more than paid for itself over time; I've found this approach has a number of advantages on top of improving error messages and I'm going to lean towards this style in all my future projects.

<!--more-->

</div>

<div class="content">

## What are structured errors?

When I talk about structured errors, I'm talking about how we represent the information attached to our errors rather than the control flow mechanism (like exceptions or monads) we use to handle them. When an error occurs, we assemble an object describing the error using actual values from the code—not just strings—giving us a structured representation of the error's context. Extracting specific pieces of information from an error should not take any parsing or guesswork!

If a function could fail because an API it depends on returned an HTTP error code, a structured error from that function would be a project-specific `ApiError` value carrying several pieces of information:

  * Which API failed.
  * The HTTP code it failed with.
  * The HTTP request that resulted in the failed response.
  * The HTTP response itself.
  * Additional data needed to provide *context*, like which inputs to the function caused the failure.

Contrast this with some less structured alternatives:

  * We could bubble up the error from our HTTP library. 
  
    But this lacks context specific to our application, which makes debugging harder. If our code called the same API in multiple places, how would we know which call led to the error?

  * We could produce a string: `"API Foo v2 failed with HTTP 400."`.
  
    But this is missing useful information—we need more details about exactly how the HTTP call failed—and the information it has is hard to extract.
    
    How would we log or render the error in a different format? We would need to parse the details out of the string which would then break if we ever changed the text we're using—text that is ostensibly meant for humans to read!

  * We could ignore the HTTP error and let the code fail somewhere down the line. 
  
    I've see in this in real projects! This is how a logical API error turns into `undefined is not a function`. The only upside with this style is that you feel like a hero once you find where the error was *actually* coming from.

These alternatives might be easier to use up-front, but leaves us less information and flexibility in the future. It's a stock picture of technical debt: we're making our life marginally easier today in return for harder debugging and refactoring tomorrow.

At heart, this is a question of **code architecture**. Structured errors separate the code that produces errors from the code that handles them. 

When we encounter a condition in our code that produces error, we do not need to worry about what happens afterwards or how the error information will be used. Will it be logged? Will it be displayed to the user as text? Will it be displayed in a UI widget? Will it be caught and handled silently without alerting anyone? We don't need answers to any of these questions; instead, we raise an error with all the context that we can and let downstream code handle it.

### Error Messages

The reason I started exploring this approach was to produce detailed, readable and consistent error messages. The logic to render error messages for the user doesn't have to fight for space and attention with domain-specific code; instead, it can be into a self-contained unit.

Here's an example[^example] that would be overkill in the middle of normal code but works well when extracted to a function:

```haskell
renderApiError ∷ ApiError → Text
renderApiError e = [__i|
  API response from #{apiName e} (#{apiVersion e}) for customer
  #{customerId e} was missing required fields.

  Missing fields:
  #{bulletedList (missingFields e)}

  Raw response:
  #{encodeJson (apiResponse e)}
|]
```

[^example]: I can't share an example from my real code at the moment, but this captures the same idea. The quasiquoter comes from the [string-interpolate] package.

[string-interpolate]: https://hackage.haskell.org/package/string-interpolate

The API call that could produce this error is going to be a handful of lines at most, so having this sort of formatting code inline would overwhelm the code that actually does anything. Instead, we build a compact `ApiError` object at the call site and keep the extensive rendering code in some other part of the codebase. `ApiError` becomes an interface between the code that raises the error and the code for managing or displaying the error. Refactoring the domain code will not touch error rendering and vice-versa.

However, extracting rendering from the domain logic does not strictly need an error *object*. We could have a `renderApiError` function, use it at the API call site and raise an exception containing the resulting string. This physically separates error rendering and domain logic but still couples the two too tightly. How would we display the error in a UI? How would we keep statistics about API failures? What if we wanted to switch to a structured log format[^structured-logging]? What if a future application has a way to recover from this error gracefully, depending on the details? Extracting the rendering logic into a function does not help for any of these situations. An `ApiError` object, on the other hand, could be caught and handled in arbitrary ways by upstream code without *any* downstream changes.

[^structured-logging]: Structured logging has similar benefits to structured errors. I didn't want to go into too much detail about it—this blog post was getting too long as-is—but it could be a fun topic for a future post.

Like other sound architectural choices, clearly separating responsibilities—between *producing* and *consuming* errors—not only helps with our original goal of improving error messages but also makes our codebase simultaneously more flexible *and* easier to maintain. 

For example, I have found structured errors help with debugging. I can include more information with the structured error object than I would provide in a human-readable string: dataframes that are too large to print, connections to active database sessions, closures… whatever makes sense and works with my resource management code. (Look out for leaks!) The first step of debugging is to observe the system, and a structured error gives me a lot of information quickly whether I'm using a debugger or adding a `catch` with print statements.

I did not consider debugging when I was first writing this code—I started thinking about errors in terms of error messages, and then I focused on how responsibilities should logically be split in my code. Everything else flows from that.

Errors that are exposed outside of your own code become part of your public API, and keeping the errors structured makes the API more discoverable and flexible. When I started thinking in these terms, I also started *testing* errors—does calling my code in intentionally incorrect ways raise the error I expect? Turns out that writing tests is *also* easier with a structured error object than with a string, since I can assert exactly the equalities I need without parsing strings or coupling my tests to user-facing error messages.

All these advantages stem from a heuristic I've found useful for system design in general: try to keep data structured as long as possible and push operations that lose information towards the edges of your system. Information is fundamentally easier to destroy than to (re)create!

</div>
<div class="content">

## Structuring Errors in Haskell

So far, everything I've talked about is language-agnostic. Structured errors make as much sense in Python as they do in Haskell. The implementation details, however, vary wildly between language and Haskell has some unique considerations.

So: how do we structure errors in Haskell? Or, at least, how did I structure my errors in Haskell?

### An Error Type

The first step, in classical Haskell style, is to define a type.

Let's say we're implementing a small configuration language. What kind of errors might we encounter? Here are a few examples:

  * Parse errors, if a config file has invalid syntax.
  * File I/O errors if a file doesn't exist or we don't have permissions to read it.
  * Missing fields, if a file doesn't specify mandatory fields.
  * Unknown fields, if a file has unexpected fields.

As you implement the tool you'll probably find other kinds of errors, but this is a solid starting point. We can turn this list directly into an algebraic data type:

```haskell
data Error = ParseError Parsec.ParseError
           | IOError IOException
           | MissingField FilePath FieldName
           | UnknownField FilePath FieldName
```

Each of the constructors of `Error` captures the context for one kind of error, giving you enough information to handle the problem. Any functions you write that might cause errors would signal using `Error`[^monad-error]:

[^monad-error]: I used [`MonadError`][MonadError] in this example because that's what I use in my own code, but this works just as well with any other error mechanism including plain old `Either Error a`.

```haskell
parseConfig ∷ MonadError Error m ⇒ FilePath → Text → m Config
parseConfig source body = case Parsec.parse config source body of
  Left parseError → throwError (ParseError parseError)
  Right parsed    → pure parsed
```

If our input doesn't [`parse`][parse], we wrap the error Parsec gives us—a structured object itself—into our `Error` type. The errors Parsec provides are thorough so we don't need anything else, but if we did, we could add more fields to the `ParseError` constructor.

Next, we need a way to handle our errors. We're writing a command-line tool, so let's render the error as user-friendly text:

```haskell
renderError ∷ Error → Text
renderError = \case
  ParseError parseError → [__i|
    Failed to parse #{sourceName (sourcePos parseError)}:
    #{show parseError }
  |]
  IOError               → ...
  MissingField          → ...
  UnknownField          → ...
```

As long as you enable [`-Wincomplete-patterns`][incomplete-patterns]—which should really be on by default—the compiler will remind you to update `renderError` whenever you add new kinds of errors.

In the future, we could also add other rendering functions:

```haskell
htmlError ∷ Error → HTML.Element
htmlError = ...

jsonError ∷ Error → Aeson.Value
jsonError = ...
```

### Modularity

A simple `Error` type is a solid strategy for structuring errors in smaller Haskell programs and libraries but does not scale well to larger codebases.

A global, centralized error type is inherently anti-modular: `Error` has to know about *every single kind of error in your whole codebase*. Any time you write code that can fail in new ways, you have to update the `Error` module as well. We also start running into circular dependency issues: the `Error` type needs to have a field with a type defined in module `Foo`, but `Foo` wants to import `Error` for handling its own errors! Haskell does not support mutually recursive modules[^recursive-modules], and I've found that wanting them is usually a sign that my overall design is not factored well.

[^recursive-modules]: Technically, GHC *does* support mutually recurisve modules with [`.hs-boot` files][boot-files], but this is an awkward feature that I would never use in my code—it exists to solve a small number of otherwise unavoidable problems, mostly in GHC's own standard library.

So how can we make our `Error` type extensible?

The cleanest solution would be some kind of structural subtyping similar to [OCaml's polymorphic variants][polymorphic-variants][^structural-subtyping]. Unfortunately, we do not have anything like this built into Haskell, and my experience with libraries implementing extensible types has been uniformly poor[^poor-experience].

[^structural-subtyping]: Polymorphic variants are probably *the* single feature I miss the most in Haskell compared to OCaml. Extensible records and sum types—coupled with some kind of row polymorphism—are *the* number one feature I want added to Haskell, but I understand that both the design and implementation of row polymorphism in Haskell are substantially more difficult than they seem.

    Still, one can dream...

[^poor-experience]: I should note that I have *not* used extensible type libraries extensively, but I've evaluated a few in the past. In my experience, these libraries:

      * are far more awkward to use than "normal" algebraic data types
      * have complex type-level implementations that leak into their APIs and error messages
      * have inconsistent and generally poor performance

    While Haskell is theoretically expressive enough to provide extensible types as a library, I believe it is not possible to make them *efficient* or *ergonomic* without language-level support.

So the first thing I tried—the first thing I try whenever I need to make a Haskell type more flexible—was adding a type parameter:

```haskell
data Error a = ParseError Parsec.ParseError
             | IOError IOException
             | MissingField FilePath FieldName
             | UnknownField FilePath FieldName
             | OtherError a
```

The idea is that `a` would stand for module-specific error types. General functions like `parseConfig` would be polymorphic in `a`:

```haskell
parseConfig ∷ MonadError (Error a) m ⇒ FilePath → Text → m Config
parseConfig = ...
```

Functions in new modules with their own kinds of errors would specify their error type for `a`. For example, if we added support for reading config values from [Avro] records, we would write something like:

```haskell
avroConfig ∷ MonadError (Error AvroError) m
           ⇒ FilePath → Avro.Value → m Config
avroConfig = ...
```

`AvroError` would play the role of `Error` for the `AvroConfig` module specifically.

But how would this extend to multiple modules with their own error type? Would I have to add a parameter to each type (ie `AvroError a`) and chain them together like a type-level list? That sounds tedious and error-prone! Besides, what do we gain from specifying each error type in the type variable?

### Existential Types

Using a type variable did not seem like a fruitful design direction. Instead, I decided to make the "other error" variable existential[^existential]:

```haskell
data Error = ParseError Parsec.ParseError
           | ...
           | OtherError SomeError

data SomeError where
  SomeError ∷ e → SomeError
```


[^existential]: If you aren't familiar with existential types, I recommend reading [Mark Karpov's introduction][mark-karpov-existential-types].

    Both my code and Mark's post use the [GADTSyntax] extension for writing existential types. In older code and documentation, you might run into the older existential type syntax instead:

    ```haskell
    data SomeError = forall e. SomeError e
    ```

    While this is strictly a matter of style—both declarations define the same type—I believe the GADT-style syntax is *so much clearer* (and more flexible to boot) that the other syntax should be considered obsolete.

`SomeError` lets me wrap a value of *any* type into an `Error` without needing to manage any visible type variables. Going back to my Avro example, if I have an error `e ∷ AvroError`, I can turn that into an `Error` value with `OtherError (SomeError e)`. Functions in the `AvroConfig` module can now have types compatible with the rest of our codebase:

```haskell
avroConfig ∷ MonadError Error m ⇒ FilePath → Avro.Value → m Config
avroConfig = ...
```

In practice, I found it was also useful to include a human-readable tag telling me which module a specific error type came from:

```haskell
data Error = ParseError Parsec.ParseError
           | ...
           | OtherError ModuleName SomeError
```

This led to a simple pattern for adding new modules: each module defines a dedicated error type as well as a function to throw errors of that type wrapped into the base `Error` type:

```haskell
throw ∷ MonadError Error m ⇒ AvroError → m a
throw avroError =
  MonadError.throwError (OtherError "Avro" (SomeError avroError))
```

### Using Existential Errors

Now that I have a way to *throw* errors of different types, what can I do with them?

With the code I've shown so far? **Nothing**.

The problem is that `SomeError`, as written, takes values of *any* type with no way to know what type it contains! That is fundamental to how existential types work, but it means I need to place some restrictions on `SomeError` to make it useful.

For every operation I want to perform on errors—rendering to the user, debugging in the interpreter, logging—I need to know that whatever type is hidden inside `SomeError` supports that operation. In Haskell, the best option is to add typeclass constraints to `SomeError`; for example, if I needed to return errors as JSON, I could constrain `SomeError` to only accept arguments of type `e` if the type were an instance of [`Aeson.ToJSON`][ToJSON]:

```haskell
data SomeError where
  SomeError ∷ (Aeson.ToJSON e) ⇒ e → SomeError
```

Think about it this way: every function in our original, non-modular design now needs to handle multiple types in an extensible way. Otherwise, how would we deal with error types defined in future modules? Typeclasses are the mechanism Haskell provides to extend a function over an open-ended set of types.

So, we replace:

```haskell
renderError ∷ Error → Text
renderError = \case
  ParseError parseError → [__i|
    Failed to parse #{sourceName (sourcePos parseError)}:
    #{show parseError }
  |]
  ...
```

with:

```haskell
class RenderError e where
  renderError ∷ e → Text

instance RenderError Error where
  renderError = \case
    ParseError parseError → [__i|
      Failed to parse #{sourceName (sourcePos parseError)}:
      #{show parseError }
    |]
    ...
    OtherError moduleName error → [__i|
      #{show moduleName} error:
      #{renderError error}
    |]
```

A typeclass is more complex than a plain function but that is the price we pay for extensibility.

As a bonus, we can define instances for other types; this not only avoids code duplication but also helps keep the format of our error messages consistent across the entire codebase. This is not substantially different from writing distinct functions for each type (`renderCustomerId`, `renderAvro`… etc), but it better-reflects the conceptual organization of the code and saves us from juggling lots of type-specific function names.

Each capability errors should have requires a typeclass. In some cases we define the classes just for error handling; in other cases, we can just reuse existing classes. Here are a few more examples, aside from rendering as text or JSON:

  * `Show` for GHCi and debugging
  * `LogFormat` if we're using structured logging
  * `ToElement` if we're displaying errors in an HTML UI
  * whatever else you need to process errors

Typeclasses give us a modular way to consume multiple types of errors in multiple ways. Both adding new types of errors *and* adding new capabilities is reasonably easy. When we add a new error type, the compiler tells us what instances we need to implement to use `SomeError`; when we add a new typeclass, adding that constraint to `SomeError` will point us to all the existing types that need new instances.

### Catching Existential Errors

The last part of the puzzle came up when I went to write tests for my new exceptions.

Let's say I have a function in the `AvroConfig` module that I expect to raise a specific `AvroError`. How do I write a unit test to check this?

I could catch the error, pattern match on `OtherError` and convert the underlying error object to JSON, but this is deeply unsatisfying. The point of using structured errors was that I could use them as normal Haskell values for things like testing; converting to JSON defeats the whole purpose.

To solve this, I borrowed a trick from Haskell's own exception system in [Control.Exception][^simon-marlow-paper]. Our goal is to have a function that tries to extract a specific type of error from `SomeError`; if the `SomeError` is carrying a value of the right type we get `Just` that value and otherwise we get `Nothing`:

```haskell
fromError ∷ Error → Maybe e
```

Note how the type variable `e` only exists in the *return* value of `fromError` (just like `read` or `fromInteger`). This isn't a problem if we can infer a concrete type, but if `e` is ambiguous we might need to specify the type explicitly using [`TypeApplications`][TypeApplications]:

```haskell
fromError @AvroError err
```

This function now gives us a way to check—at runtime—whether the error hidden inside `SomeError` has a specific type, and, if it does, extract that value. This is all we need to write tests.

To implement `fromError`, we're going to rely on a bit of `Typeable` magic—a capability added to GHC for just this sort of use:

```haskell
class Typeable e ⇒ CatchableError e where
  fromError ∷ Error → Maybe e
  fromError (OtherError _ (SomeError e)) = Typeable.cast e
  fromError _                            = Nothing
```

Since we have a default implementation for the only method in this class, writing instances is as simple as:

```
data AvroError = ...

instance CatchableError AvroError
```

Finally, we need to add this class to the `SomeError` type.

Since `CatchableError` is pretty generic, it is also convenient to move every other constraint from `SomeError`—`Show`, `RenderError`, `ToJSON`... etc—to be a superclass of `CatchableError`. This is purely a stylistic choice and you could accomplish exactly the same effect with a type synonym instead.

These changes give us the following definition of `SomeError`:

```haskell
data SomeError where
  SomeError ∷ (CatchableError e) ⇒ e → SomeError
```

With all the `CatchableError` machinery in place, we can write tests for specific types of errors:

```haskell
testCase "check specific error" $$ do
  case failingOperation of
    Left err → case fromError @ModuleError err of
      Just got → assert (got == expected)
      Nothing  → fail "Wrong type of error!"
    Right res → fail "Expected error but got: " <> show res
```

We don't have to use this just for testing. The `fromError` mechanism lets us catch and handle errors however we like, which lets us write code that catches, inspects and even recovers from domain-specific errors.

### An Error Pattern

With all these pieces in place, we have a pattern for structured extensible errors in Haskell:

  1. Start by defining an `Error` type for your core code.
  2. When you need modularity, add an existentially typed `SomeError` type.
  3. In new modules, define module-specific `Error` types and `throw` functions.
  4. Add typeclasses for each operation you want to perform on your structured errors.
  5. Define a `CatchableError` class using `Typeable` so that you can extract errors of specific types at runtime.
  
I could imagine variations on this design: for example, we could make `SomeError` our central error type and not have any other constructors in `Error` at all. We could also start building more complex hierarchies of errors. This is not meant to be a prescriptive guide to error handling in Haskell; it's a reflection of how my code in particular evolved over time, and the core ideas that helped it work relatively well.

I did not start my project with this pattern in mind. A single centralized `Error` type served me perfectly well for a long time, and the other details evolved through a number of iterations. Getting all the pieces into place required some setup and complexity and I'm not sure this is the "best" design for extensible errors, but I've had a great experience using this pattern so far and will use a similar pattern on future projects.

[^simon-marlow-paper]: The design of `Control.Exception` goes back to a paper by Simon Marlow published in *Haskell '06*: [*An Extensible Dynamically-Typed Hierarchy of Exceptions*][haskell-06]


[TypeApplications]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications
[haskell-06]: https://simonmar.github.io/bib/papers/ext-exceptions.pdf
[Control.Exception]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Exception.html
[ToJSON]: https://hackage.haskell.org/package/aeson-1.5.4.1/docs/Data-Aeson.html#t:ToJSON
[mark-karpov-existential-types]: https://markkarpov.com/post/existential-quantification.html
[Avro]: https://avro.apache.org/
[polymorphic-variants]: https://caml.inria.fr/pub/docs/manual-ocaml/lablexamples.html#s%3Apolymorphic-variants
[incomplete-patterns]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Wincomplete-patterns
[MonadError]: http://hackage.haskell.org/package/mtl/docs/Control-Monad-Except.html#t:MonadError
[GADTSyntax]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-GADTSyntax
[parse]: https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html#v:parse
[boot-files]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#how-to-compile-mutually-recursive-modules
