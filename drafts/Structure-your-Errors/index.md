---
title: Structure your Errors
author: Tikhon Jelvis
---

Recently, I've revisited how I represent errors in code.

I'm working on a command-line tool used across multiple teams and I want to keep its error messages consistent and readable. As the codebase has grown, I've moved from *ad hoc* error strings throughout my code to a structured error type.

Useful error messages need to:

  * Contain the information and context to diagnose and fix the problem.
  * Provide an intuitive explanation for new users.
  * Format information consistently, in a way that's easy to scan at a glance.

We need 10–20 lines of code for each kind of error to generate error messages that fulfill these goals. In other projects, I would create error message strings directly in the code that first detected or raised an error, but that just does not scale here—it would add too much noise to my code! It's much harder to keep error messages consistent in style when they are so decentralized, and refactoring messages *en masse* becomes a nightmare.

To fix these problems, I've started using dedicated types for my errors. This took some up-front effort but has more than paid for itself over time; I've found this approach has a number of advantages on top of improving error messages and I'm going to lean towards this style in all my future projects.

</div>

<div class="content">

## What are structured errors?

When I talk about structured errors, I'm not talking about the mechanism we use to handle errors (like exceptions, monads or return types), but rather about how we handle the information attached to an error. When an error occurs, we assemble an object describing the error using actual values from the code—not just strings. Extracting specific pieces of information from an error should not take any parsing or guesswork!

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

At heart, this is a matter of **code architecture**: structured errors let us separate the parts of the code are responsible for *producing* errors from the parts that *handle* errors. When we encounter a condition in our code that requires an error, we do not need to think about how that error will eventually be used. Will it be logged? Will it be displayed to the user as text? Will it be displayed in a UI widget? Will it be caught and handled silently without alerting anyone? Instead, we raise an error with all the context information that we can and let downstream parts of the code handle it as needed.

Like so many matters of code architecture, once we get it right, we end up with a surprising amount of practical consequences that, at first glance, might seem unrelated.

The first upside I noticed—the reason I started down this path in the first place—is that structured errors made it easier to produce detailed, readable error messages. The logic to render error messages for the user doesn't have to fight for space and attention with domain-specific code; instead, it can be into a self-contained unit.

Here's a Python example that would be overkill in the middle of normal code but works well when extracted to a function:

```python
def render_api_error(error):
    return f"""
API response from {error.api_name} {error.api_version} for customer {error.customer_id} was missing required fields.

Missing fields:
{error_formatting.bulleted_list(error.missing_fields)}

Raw response
{json.dumps(error.api_response, idnent=4, sort_keys=True)}

... etc ...
"""
```

The API call that could produce this error is going to be a handful of lines at most, so having this sort of formatting code inline would overwhelm the code that actually does anything. Instead, we build a compact `ApiError` object at the call site and keep the extensive rendering code in some other part of the codebase. `ApiError` becomes an interface between the code that raises the error and the code for managing or displaying the error. Refactoring the domain code will not touch error rendering and vice-versa.

However, none of this strictly needs an error *object*. We could have a `render_api_error` function, use it at the API call site and raise an exception containing the resulting string. This physically separates the rendering logic from the API call site but still couples the two compared to the rest of the codebase. What happens if we want to display the error message in a UI later on? What if we have some way to recover from an error, depending on the details? What if we want to start keeping statistics about API failures? Doing any of these would require either rewriting the `render_api_error` function or parsing the resulting string. An `ApiError` object, on the other hand, could be caught and handled in arbitrary ways by upstream code without needing to change *anything* downstream.

Ultimately, this is an example of a heuristic I've found useful for system design in general: try to keep data as structured as possible as long as possible and push operations that lose information towards the edges of your system. Information is fundamentally easier to destroy than to (re)create!

</div>
<div class="content">

## Structuring Errors in Haskell

So far, everything I've talked about is language-agnostic. Structured errors make as much sense in Python as they do in Haskell. The implementation details, however, vary wildly language-to-language, and Haskell has some unique considerations of its own.

So: how do we structure errors in Haskell?

### An Error Type

The first step, in classical Haskell style, is to define a type. Let's say we're implementing a small configuration language. What kind of errors might we encounter? Here are a few examples:

  * Parse errors, if a config file has invalid syntax.
  * File I/O errors if a file doesn't exist or we don't have permissions to read it.
  * Missing fields, if a config doesn't specify mandatory fields.
  * Unkown fields, if a config specifies fields our tool doesn't recognize.

As you implement the tool you'll probably find other kinds of errors, but this is a good starting point. We can turn this list directly into an algebraic data type:

```haskell
data Error = ParseError Parsec.ParseError
           | IoError IOException
           | MissingField FilePath FieldName
           | UnknownField FilePath FieldName
```

<!-- TODO: can I add links *inside* code blocks? -->

Each of the constructors of `Error` captures one *kind* of errors and gives you enough information to handle the error in different ways. Any functions you write that could fail would use this type to signal errors:

```haskell
parseConfig :: MonadError Error m ⇒ FilePath → Text → m Config
parseConfig source body = case Parsec.parse config source body of
  Left parseError → throwError (ParseError parseError)
  Right parsed    → pure parsed
```

Here we call Parsec's [`parse`][parse] function, inspect the result and raise an `Error` if it did not parse correctly. Our `ParseError` constructor does not have any information besides the error Parsec generated because Parsec's error format is structured enough for our use; if we needed to include extra information, we would add fields to the `ParseError` constructor.

I used `MonadError` in this example because that's what I do in my own code, but this works just as well with any other error mechanism including plain old `Either Error a`.

[MonadError]: http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#t:MonadError
[parse]: https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html#v:parse
