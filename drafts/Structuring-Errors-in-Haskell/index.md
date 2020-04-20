---
title: Structure Your Errors
author: Tikhon Jelvis
---

<!-- Intro: command-line tool, readable errors... etc -->

# What are structured errors?

Most languages provide *some* way of signaling errors: runtime errors, checked exceptions, error types, rejected promises... With the exception of limited systems like C-style error codes, all of these methods let us provide additional data with our error that will be available to some error handler down the line. When I talk about structured errors, I'm not talking about the mechanism we use to *handle* errors, but rather about how we express—how we *structure*—this additional data.

A program is using **structured errors** if:

  * There is a domain-specific abstraction—like a class or at type—that describes different kinds of errors the program can encounter.
  * Instances of the error abstraction carry *actual values* from the code.

For example, if a function could fail because an API it depends on returned an HTTP error code, a structured error from that function would be a project-specific `ApiError` carrying several pieces of information:

  * Which API failed.
  * The HTTP code it failed with.
  * The HTTP request that resulted in the failed response.
  * The HTTP response itself.
  * Additional data needed to provide *context*, like which inputs to the function caused the failure.

Less structured alternatives range from a raw HTTP error—structured, but not domain-specific and missing context—to a generic exception with a message that just says `"API error"` to an unrelated error triggered later in the code (`undefined is not a function`). Using `assert` statements, building string error messages right in your code, letting key and field lookups fail and bubbling up exceptions raised by library functions are all patterns that result in unstructured exceptions.

The goal is to carry all the information you need to either report or handle an error in a format that preserves the structure of that information from your normal code. Doing this effectively requires some measure of defensive programming; when something fails, we need to detect it early enough to supply all the right context with our error. If we call an API whose response has to fulfill a particular contract, we need to check that contract right after calling the API so that we can report *which API* failed on *which request*; if we instead fail on some internal invariant check later in our code, we won't have the context that the failure was caused by a specific API call violating its contract.

## Why?

Writing with structured errors isn't free. Instead of building error messages on the spot, we have to build a structured value that will probably just get turned into a string message at the end. We have to program defensively, writing extra code to watch for error conditions. We can't let exceptions from libraries and language operations bubble up automatically because they are not structured within the context of our application. We end up with more code and more moving parts than simpler approaches to error handling.

So why should we pay this cost? At heart, this is a matter of **code architecture**: structured errors let us separate the parts of the code are responsible for *producing errors* from the parts that *handle* errors. When we encounter a condition in our code that requires an error, we do not need to think about how that error will eventually be used. Will it be logged? Will it be displayed to the user as text? Will it be displayed in a UI widget? Will it be caught and handled silently without alerting anyone? Instead, we raise an error with all the context information that we can and let downstream parts of the code handle it as needed.

<!-- TODO: Detail specific uses from outline. -->

</div>
<div class="content">

# Structuring Errors in Haskell

So far, everything I've talked about is language-agnostic. Structured errors make as much sense in Python as they do in Haskell. However, the *way* you implement structured errors is going to differ substantially language-to-language.

How would we go about structuring our errors in Haskell specifically?

## An Error Type

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
