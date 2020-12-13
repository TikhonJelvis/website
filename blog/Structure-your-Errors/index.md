---
title: Structure your Errors
author: Tikhon Jelvis
published: 2020-12-13 00:31:29
modified: 2020-12-13 00:31:46
---

Recently, I've revisited how I represent errors in code.

I'm working on a command-line tool used across multiple teams and I want to keep its error messages consistent and readable. As the codebase has grown, I've moved from *ad hoc* error strings throughout my code to a structured error type.

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

When I talk about structured errors, I'm talking about how we represent the information attached to our errors rather than the control flow mechanism (like exceptions or monads) we use to handle errors. When an error occurs, we assemble an object describing the error using actual values from the code—not just strings—giving us a structured representation of the error's context. Extracting specific pieces of information from an error should not take any parsing or guesswork!

For example, if a function could fail because an API it depends on returned an HTTP error code, a structured error from that function would be a project-specific `ApiError` value carrying several pieces of information:

  * Which API failed.
  * The HTTP code it failed with.
  * The HTTP request that resulted in the failed response.
  * The HTTP response itself.
  * Additional data needed to provide *context*, like which inputs to the function caused the failure.

Contrast this with some less structured alternatives:

  * We could bubble up the error from our HTTP library, but this would lack context and miss information specific to our application. Missing context can make debugging much harder if our application is calling the same API for different reasons!

  * We could produce a string: `"API Foo v2 failed with a 400 error code."`. But this is missing useful information—we need more details about exactly how the HTTP call failed—and the information it has is hard to extract. If we ever wanted to log or render the error in a new way we would have to parse details out of the string, which would then break if we ever rewrote the ostensibly human-readable string.

  * We could ignore the HTTP error and just let the code fail somewhere down the line. I've see in this in real projects! This is how an API error turns into `undefined is not a function`. The only upside with this style is that you feel like a hero once you figure out where the error was *actually* coming from.

These alternatives are easier up-front but limit our information and flexibility in the future. It's almost a stock picture of technical debt: we're making our life marginally easier today in return for harder debugging and refactoring tomorrow.

At heart, this is a matter of **code architecture**: structured errors let us separate the parts of the code are responsible for *producing* errors from the parts that *handle* errors. When we encounter a condition in our code that requires an error, we do not need to think about how the error information will be used. Will it be logged? Will it be displayed to the user as text? Will it be displayed in a UI widget? Will it be caught and handled silently without alerting anyone? We don't answer any of these questions; instead, we raise an error with all the context information that we can and let downstream parts of the code handle it as needed.

Like other architecture question, getting this right gives us other benefits that, at first glance, might seem unrelated.

The first upside I noticed—the reason I started down this path in the first place—is that structured errors made it easier to produce detailed, readable error messages. The logic to render error messages for the user doesn't have to fight for space and attention with domain-specific code; instead, it can be into a self-contained unit.

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

However, none of this strictly needs an error *object*. We could have a `render_api_error` function, use it at the API call site and raise an exception containing the resulting string. This physically separates the rendering logic from the API call site but still couples the two compared to the rest of the codebase. What happens if we want to display the error message in a UI later on? What if we have some way to recover from an error, depending on the details? What if we want to start keeping statistics about API failures? What if we want to switch to a structured binary log format[^structured-logging]? Doing any of these would require either rewriting the `render_api_error` function or parsing the resulting string. An `ApiError` object, on the other hand, could be caught and handled in arbitrary ways by upstream code without needing to change *anything* downstream.

[^structured-logging]: Structured logging has similar benefits to structured errors. I didn't want to go into too much detail about it—this blog post was getting too long as-is—but it could be a fun topic for a future post.

Apart from architectural flexibility, I also found structured errors helped with debugging. I can include more information with the structured error object than I would provide to the user—including values like functions that aren't even printable. When I'm trying to figure out what's causing an error, I get more information whether I'm using a debugger or adding a `catch` with print statements.

Errors that can be exposed outside of your own code implicitly become part of the code's public API, and keeping the errors structured makes the API more discoverable and easier to use. When I started thinking in these terms, I also started *testing* errors—does calling my code in intentionally incorrect ways raise the error I expect? Turns out that writing tests is *also* much easier with a structured error object than with a string, since I can assert exactly the equalities I need without needing to parse strings or coupling my tests to user-facing error messages.

All these advantages stem from a heuristic I've found useful for system design in general: try to keep data structured as long as possible and push operations that lose information towards the edges of your system. Information is fundamentally easier to destroy than to (re)create!

</div>
<div class="content">

## Structuring Errors in Haskell

So far, everything I've talked about is language-agnostic. Structured errors make as much sense in Python as they do in Haskell. The implementation details, however, vary wildly language-to-language and Haskell has some unique considerations.

So: how do we structure errors in Haskell?

### An Error Type

The first step, in classical Haskell style, is to define a type. Let's say we're implementing a small configuration language. What kind of errors might we encounter? Here are a few examples:

  * Parse errors, if a config file has invalid syntax.
  * File I/O errors if a file doesn't exist or we don't have permissions to read it.
  * Missing fields, if a config doesn't specify mandatory fields.
  * Unknown fields, if a config specifies fields our tool doesn't recognize.

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

If our input doesn't [`parse`][parse], we wrap the error Parsec gives us—a structured object itself—because we don't need to provide any other information. If we did need more information, we could add more fields to our `ParseError` constructor.

We also need a way to handle our errors. If we're writing a command-line tool, we would have a function to render the error as user-friendly text:

```haskell
renderError ∷ Error → Text
renderError = \case
  ParseError parseError → [__i|
    Failed to parse #{sourceName (sourcePos parseError)}:
    #{show parseError }
  |]
  IOError → ...
  MissingField → ...
  UnknownField → ...
```

As long as you enable [`-Wincomplete-patterns`][incomplete-patterns]—which should really be on by default—the compiler will remind you to update this rendering function whenever you add new kinds of errors. In the future, we could also add other rendering functions:

```haskell
htmlError ∷ Error → Text
htmlError = ...

jsonError ∷ Error → Aeson.Value
jsonError = ...
```

### Modularity

A simple `Error` type is a solid strategy for structuring errors in smaller Haskell programs and libraries, but it doesn't scale well to larger codebases. Having a single centralized type is inherently anti-modular: `Error` has to know about *every single kind of error in your whole codebase*, and any time you write new code that can fail in new ways, you have to update the `Error` module as well. As we do this, we also start running into circular dependency issues: the `Error` type needs to have a field with a type defined in module `Foo`, but `Foo` wants to import `Error` for handling its own errors! Circular dependencies are awkward in Haskell and I've found that they are usually a red flag that your code design is not modular in the specific ways your code needs.

So how can we make our `Error` type extensible?

The cleanest solution would be some kind of structural subtyping similar to [OCaml's polymorphic variants][polymorphic-variants][^structural-subtyping]. Unfortunately, we do not have anything like this built into the language, and my experience with libraries implementing extensible types has been uniformly poor[^poor-experience].

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

But how would this extend to multiple modules with their own error type? Would I have to add a parameter to each type (ie `AvroError a`) and chain them together like a type-level list? That sounds tedious and error-prone! Besides, what does specifying each error type in the type variable really get us?


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

The `SomeError` type lets me wrap a value of *any* type into an `Error` without needing to manage any visible type variables. Going back to my Avro example, if I have an error `e ∷ AvroError`, I can wrap that into our base `Error` type with `OtherError (SomeError e)`. Now functions in the `AvroConfig` module can have types compatible with the rest of our codebase:

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

This led me to a pretty simple pattern for adding new modules: each module defines a dedicated error type as well as a function to throw errors of that type, taking care of all the details to wrap it into an `Error` value:

```haskell
throw ∷ MonadError Error m ⇒ AvroError → m a
throw avroError =
  MonadError.throwError (OtherError "Avro" (SomeError avroError))
```

### Using Existential Errors

Now that I have a way to *throw* errors of arbitrary structured types, what can I do with them?

With the code I've shown so far? **Nothing**.

The problem is that `SomeError`, as written, can take values of *any* type—and then gives me no way to know what type it contains! That is fundamental to how existential types work, but it means I need to place some restrictions on `SomeError` to make it useful. For every operation I want to perform on errors—rendering to the user, debugging in the interpreter, logging—I need to know that whatever type is hidden inside `SomeError` supports that operation. In Haskell, the best option is to add typeclass constraints to `SomeError`; for example, if I needed to return errors as JSON from some API, I could constrain `SomeError` to only accept arguments of type `e` if the type implemented [`Aeson.ToJSON`][ToJSON]:

```haskell
data SomeError where
  SomeError ∷ (Aeson.ToJSON e) ⇒ e → SomeError
```

One way to think about this is that every single function that consumed `Error` in our original (non-modular) design needs to become a typeclass, because we need to handle values of specific errors added by unknown modules in the future as well as the base `Error` type. So

```haskell
renderError ∷ Error → Text
renderError = \case
  ParseError parseError → [__i|
    Failed to parse #{sourceName (sourcePos parseError)}:
    #{show parseError }
  |]
  ...
```

Would be replaced with:

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

A typeclass is more complex than a plain function but is also extensible. As a bonus, we can define instances for other common types like `Avro.Value`; this not only avoids code duplication but also helps keep the format of our error messages consistent across the entire codebase.

Apart from rendering to users or to external formats like JSON, we might need a few other classes:

  * `Show` for GHCi and debugging
  * `LogFormat` if we're using structured logging
  * `ToElement` if we're displaying errors in an HTML UI
  * whatever else you need to process errors

Typeclasses let us support multiple ways of consuming errors in a modular way. This makes it reasonably easy to *both* add new error types *and* add new ways of consuming errors. If we add a new error type, the compiler will tell us what instances we need to implement when we wrap the type with `SomeError`; if we add a new typeclass to consume errors, adding that constraint to `SomeError` will point us to all the existing error types across the codebase that need new instances.

### Catching Existential Errors

The last part of the puzzle came up when I went to write tests for my new exceptions. If I have some function in my `AvroConfig` module that I expect to raise a specific `AvroError`, how do I check that it's behaving correctly? I could catch the error, pattern match on `OtherError` and convert the underlying error object to JSON, but this is deeply unsatisfying. The point of using structured errors was that I could use them as normal Haskell values for things like testing; converting to JSON defeats the whole purpose.

To solve this, I borrowed a trick from Haskell's own exception system in [Control.Exception][^simon-marlow-paper]. Our goal is to have a function that tries to extract a specific type of error from `SomeError`; if the `SomeError` is carrying a value of the right type we get `Just` that value and otherwise we get `Nothing`:

```haskell
fromError ∷ Error → Maybe e
```

Note how the type variable `e` only exists in the *return* value of `fromError` (just like `read` or `fromInteger`). This isn't a problem if we can infer a concrete type when using `fromError`, but if the context leaves `e` ambiguous, we might need to specify the type explicitly using [`TypeApplications`][TypeApplications]:

```haskell
fromError @AvroError err
```

To implement this `fromError` type, we're going to rely on a bit of `Typeable` magic—a capability added to GHC for just this sort of use:

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

Finally, we need to add this class to the `SomeError` type. Since `CatchableError` is pretty generic, I also found it was convenient to move every other constraint I had on `SomeError`—`Show`, `RenderError`, `ToJSON`... etc—to be a superclass of `CatchableError`. This is purely a stylistic choice and you could accomplish exactly the same kind of consolidation with a type constraint synonym instead.

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

This mechanism isn't just there to make tests cleaner—it also lets us catch and handle any type of error. We can write code to catch, inspect and recover from errors that were introduced in domain-specific modules.

### An Error Pattern

With all these pieces in place, we have a pattern for structured extensible errors in Haskell:

  1. Start by defining an `Error` type for your core code.
  2. When you need modularity, add an existentially typed `SomeError` type.
  3. In new modules, define module-specific `Error` types and `throw` functions.
  4. Add typeclasses for each operation you want to perform on your structured errors.
  5. Define a `CatchableError` class using `Typeable` so that you can extract errors of specific types at runtime.

I did not actually start writing my code with this approach in mind. A single centralized `Error` type served me perfectly well for a long time, and the final pattern evolved after a number of iterations. Getting all the pieces into place required some setup and complexity and I'm not sure this is the "best" design for extensible errors in Haskell, but I've had a great experience using this pattern so far and will use a similar pattern on future projects.

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
