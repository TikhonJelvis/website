---
title: Semantic Version Control
author: Tikhon Jelvis
---

<div class="content">

# Cow: Semantic Version Control

Cow is a tool to compare and merge source code more intelligently. I worked on this project with [Ankur Dave](http://ankurdave.com). It is completely free and open source, with the code available on [GitHub](https://github.com/TikhonJelvis/Cow).

Watch a [brief screencast](cow-out.ogv) giving a demo of the system's basic capabilities. 

</div>

<div class="content">

# Description

Cow is a tool for diffing and merging code. The core idea is to treat code as code rather than text. This means that we first parse the files and then analyze the parse trees.

Currently we have a proof-of-concept implementation. The basics work and have been tested on some very small examples. However, the performance is completely untenable and the program has not been tested on real-world code. There are some well-known ways to improve the performance asymptotically, so all this is just a matter of work.

There is also no UI. We have a couple of different visualization systems---as shown in the video---but nothing usable for real work.

</div>
<div class="content">

# Features

## Syntactic Diff

Parsing the files and operating on the AST gives us a "syntactic" diff: we operate at the level of syntax rather than just text. This by itself has some nice properties: the diff is whitespace/formatting agnostic and it is accurate at the token level rather than a word/line level.

However, for this to work, we need a parser for each language we want to support. If all our features were purely syntactic, needing a parser would not be worth it! (We could probably get away with just having a lexer, but even that would not be terribly useful.)

## Semantic Diff

Happily, we do more than just look at syntax. As the name "Semantic Version Control" implies, we try to analyze the meaning of the code and guess what high-level actions the programmer performed in editing it.

Of course, at its core, all this analysis is trying to read the programmer's mind: we try to guess what the user *meant* to do based on how the text changed. This means that we can never be 100% accurate. However, even if we don't guess the programmer's intentions correctly, the analysis can still be useful.

### Move Detection

Currently, the main semantic feature is move detection. If you move a function or block of code, we can usually detect this as one action rather than some additions in one place and some deletions in another. We can detect moves even if you make changes to the block of code---as long as its still pretty close, we'll see it. As an example, consider changing:

```javascript
function foo(a, b) {
  function bar(c, d) {
    return c * d;
  }

  return a + bar(b, 10);
}
```

to:

```javascript
function foo(a, b) {
  return a + bar(b, 10);
}

function bar(a, b) {
  return a * b;
}
```

We would detect this change as moving `bar` outside of `foo`, even though `bar` itself was changed (its arguments got renamed). Moreover, this allows us to detect the *overlapping* changes: since we know that `bar` was moved, we can also compare the two moved versions and find that the arguments got renamed. So we could find different changes in the code even though the text they affect is actually the same!

### Scope Analysis

We also do some analysis to figure out which variables are the same. This lets us identify when two `i`s are the same and when they're not, even if the code is heavily nested.

The analysis itself is already implemented but is not currently being used for anything. However, it will have a very simple use in the near future: identifying renames. If you go through your code and rename some variable `x` to `y`, it would be great to consolidate all those changes into one high-level action rather than highlighting every single line that changed.

This would be particularly useful when merging files: if you rename a variable, you almost definitely want it renamed on *every* line it appears. So turning the rename into a single atomic action would make merging easier and reduce the number of mistakes you can make.

## Merging

Apart from using our semantic information to find the differences between two programs, we can also do three-way merges and conflict resolution.

Since we have additional information about the code, we can actually resolve conflicts that *overlap*. For example, imagine a file where one person moves a function and another person renames some variables. Since we detect both actions at a higher level than just changes in text, we can see that they do not actually conflict---both can be applied without breaking anything.

Now, this is probably a little bit too heuristic-driven to be used in a fully automated fashion. However, it can make interactive merging *much* easier between files. Being able to look at semantic actions like renames and moves rather than individual changes to regions of text should be both easier and less error-prone than using a normal merging tool.

</div>