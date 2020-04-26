---
title: Structure Your Errors
author: Tikhon Jelvis
---

Recently, I've revisited how I represent errors in code. I'm working on a command-line tool that's used by people across multiple teams and I want to ensure the error messages are useful and productive. Error messages should:

  * Contain all the information and context a user needs to diagnose and fix the error.
  * Provide enough explanatory text for less-experienced users of the tool.
  * Format this information in a way that's easy to scan at a glance.

Every single error the tool can produce needs a fair amount of code to meet these goals, which means that building error messages as strings right in my domain code does not work—my core logic would get swamped by string formatting code, it would be hard to keep error messages consistent throughout the whole codebase and refactoring error messages *en masse* would be a nightmare.

With this in mind, I made the decision to consistently use **structured errors** throughout the codebase. Instead of passing around errors as strings, I would have a custom type to represent errors that could be turned into a string down the line. Structured errors require some up-front work and planning, but I've found that the effort more than paid for itself as the codebase has grown and evolved.

Structured errors definitely helped me produce consistent, effective error messages, but I also discovered other advantages as the project progressed. Now that I have practical experience with structured errors, I'm going to use the same techniques across my projects in different languages.

The project that first pushed me to think about structured errors is written in Haskell, but the ideas themselves are entirely language-agnostic. I'm going start by covering structured errors and the tradeoffs involved in a way that applies to almost any programming language; after that, I'll dive into the pattern I've settled on for structuring my errors in Haskell code specifically. Other languages will have their own patterns for achieving the same ends, but chances are the details will look pretty distinct from Haskell!

</div>

<div class="content">

## What are structured errors?

Most languages provide *some* way of signaling errors: runtime errors, checked exceptions, error types, rejected promises... Except for limited systems like C-style error codes, all of these methods let us provide additional data that will be available when we handle the error down the line.

When I talk about structured errors, I'm not talking about the mechanism we use to *handle* errors (like exceptions, monads or return types), but rather about how we structure this additional data.

A program is using **structured errors** if:

  * There is a domain-specific abstraction—like a class or an algebraic data type—that describes different kinds of errors the program can encounter.
  * Instances of the error abstraction carry *actual values* from the code.

For example, if a function could fail because an API it depends on returned an HTTP error code, a structured error from that function would be a project-specific `ApiError` carrying several pieces of information:

  * Which API failed.
  * The HTTP code it failed with.
  * The HTTP request that resulted in the failed response.
  * The HTTP response itself.
  * Additional data needed to provide *context*, like which inputs to the function caused the failure.

The goal of structuring all this error information is to let code downstream either handle and recover from the error or at least fail in a user-friendly way.

Contrast this with some less-structured alternatives. We could let the error from our HTTP library bubble up unchanged; this would still be structured to *some* extent, but it would not be specific to our project and would be missing information about the *context* of the API call. We could produce a string message (`"API Foo v2 failed with a 400 error code."`) but this contains even *less* information and the only way to extract details (Which API failed? What was the error?) would be to parse the string. We could even simply ignore the error and let our code fail somewhere downstream, giving us something like JavaScript's famous `undefined is not a function` error. All of these options carry *less* information that a structured error would, and make it substantially harder to either recover from the error or fail in a user-friendly way.

The last example—letting our code fail down the line—illustrates an important principle of structured errors: merely structuring error information is not enough; we also need to *raise the error at the right time*. This requires some measure of defensive programming; when something fails, we need to detect it early enough to supply all the right context with our error. If we call an API whose response has to fulfill a particular contract, we need to check that contract right after calling the API so that we can report *which API* failed on *which request*; if we instead fail on some internal invariant check later in our code, we won't have the context that the failure was caused by a specific API call violating its contract.

### Why?

Writing with structured errors isn't free. Instead of building error messages on the spot, we have to build a structured value that will eventually get turned into a string message anyway. We have to program defensively, writing extra code to watch for error conditions. We can't let exceptions from libraries and language operations bubble up because they are not structured within the context of our application. We end up with more code and more moving parts than simpler approaches to error handling.

So why should we pay this cost?

At heart, this is a matter of **code architecture**: structured errors let us separate the parts of the code are responsible for *producing* errors from the parts that *handle* errors. When we encounter a condition in our code that requires an error, we do not need to think about how that error will eventually be used. Will it be logged? Will it be displayed to the user as text? Will it be displayed in a UI widget? Will it be caught and handled silently without alerting anyone? Instead, we raise an error with all the context information that we can and let downstream parts of the code handle it as needed.

Like so many matters of code architecture, once we get it right, we end up with a surprising amount of practical consequences that, at first glance, might seem unrelated.

#### Readable Error Messages

The first upside I noticed—the reason I started down this path in the first place—is that structured errors made it much easier to produce detailed, readable error messages. The logic to render error messages for the user doesn't have to fight for space and attention with my domain logic; instead, it can be removed to some kind of self-contained unit by itself. I could write a bunch of code to render the components of an error into a detailed multi-line message without adding a bunch of noise to the main parts of my code.

Here's a Python example that would be overkill in the middle of normal code, but works well when extracted to a function:

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

The API call that could produce this error is probably a single line itself, so having this sort of formatting code inline would overwhelm the code that actually does anything. With structured errors, we just build an `ApiError` object and format the code separately. <!-- TODO: awkward wording in this paragraph -->

Error rendering ends up decoupled from the rest of the code, with a clear interface defined by the structure of the error. Refactoring the domain code will not touch error rendering at all and vice-versa. To do this effectively, the structure of the error should be designed based on what information it makes sense to *provide*, even if you won't necessarily display all that information to the user. Your error rendering code can always ignore information on the error value if it doesn't need to be part of the user-facing error message.

#### Different Formats

It's worth noting that you don't need fully structured errors to accomplish the previous Python example. Instead of having an `ApiError` object, you could have `render_api_error` take a bunch of arguments directly and call it *before* raising the error from your domain code:

```
def render_api_error(api_name, missing_fields, ...):
    ...
```

Ad-hoc error-formatting functions called directly from your code are less crisp in terms of code organization, but give you the same substantial advantages: the code to render error messages is separated out of your domain code.

Structured errors, however, go further. We can render errors into *multiple* formats and add new formats without touching the domain code at all.

#### Refactoring

</div>
<div class="content">

## Structuring Errors in Haskell

So far, everything I've talked about is language-agnostic. Structured errors make as much sense in Python as they do in Haskell. However, the *way* you implement structured errors is going to differ substantially language-to-language.

How would we go about structuring our errors in Haskell specifically?

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
