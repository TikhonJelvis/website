---
title: Writing Code To Be Read at a Glance
author: Tikhon Jelvis
---

In software engineering circles, there's a common adage: code is read more than it is written. This is a useful insight that pushes us to writing readable code, but I feel it's only part of the story. Just as code is read more than written, **it is skimmed more than it is read**. This idea should influence code style just as much as the original.

The reason code is read more than it is written is related to a simple reality of most software projects: more time and effort is spent on *maintaining* code than on *writing* it. You write code once and then return to it multiple times, fixing bugs, adding features, refactoring. To do this, of course, you have to change the existing code---and to change it you have to read and understand it.

But for every part of the code you need to return to in depth, there are four or five other parts that merely affect it. You don't have to understand these fully since you're not modifying them; you're content understanding *what they are supposed to do*. To get there, you have to find the specific part of the code you need to change. Again, this requires quickly scanning through large parts of the project---not trying to understand exactly what's going on in each section.

All this adds up to a similar multiplicative relationship: just as you'll end up reading multiple times for each piece of code you write, you'll end up skimming multiple times for each piece you read. This means that writing code you can understand *at a glance* is at least as important as writing code that you can read at all.

</div>
<div class="content">

## Shape

What does writing code that's easy to read at a glance actually involve? To me, it comes down to thinking about the **"shape"** of my code. The structure of the code should give you a quick idea of what it's supposed to do.

An immediate consequence of caring out the shape of your code is that related code *looks related* and unrelated code *looks unrelated*---regardless of implementation. This is an efficient way to guide your reader in a way that doesn't force them to read your code deeply. You can reinforce this by keeping related grouped close together.

### Verbosity

Verbosity and boilerplate obscure the shape of your code. This doesn't mean you should *never* use verbose names in your code, just that you should be restrained and tasteful. If an identifier comes from far away---a function from another module, perhaps---giving it a descriptive name can outweigh the downsides.

But it's rarely worth it for local identifiers or anything that's repeated a lot; those benefit highly from short names. This explains why some languages like Haskell tend to use a lot of one-letter variable names: variables which only exist for a line or two absolutely *do not* need a long name and the long name would just be distracting.

In general, I prefer to make code compact as long as it doesn't clash with other priorities like making external functions easier to understand.

### Plumbing

Some of your code is the "meat" of your expression, the parts that matter in this instance. The rest is more like plumbing---important to keep everything working but less significant in any *particular* instance. Type conversions and control flow (ie `map`) are perfect examples of this. Being able to quickly differentiate which is which is incredibly useful for understanding code faster since you can often simply ignore plumbing when you're skimming code. This is where custom infix operators are incredibly useful: they are distinct from normal identifiers while also giving your code some additional structure by naturally breaking the expression up into groups.

After a while, these plumbing operators start to fade into the background when you're scanning through code. This makes it easy to see similarities between code that might be doing the same thing in different contexts. As a simple example consider the `Applicative` operators in Haskell: they let us apply functions over values in some functor (like `Maybe` and `IO`) in a way that's immediately reminiscent of normal function application:

```haskell
f a b c             -- normal function application
f <$$> a <*> b <*> c -- over an Applicative
```

This style also lets us see how operators naturally group code together:

```haskell
f (a + b) (c * d)
f <$$> a + b <*> c * d
```

### Information Content

One perspective I've found incredibly useful is to think about what what the minimum amount of information an expression *must* contain. A polynomial in a single variable, for example, only really needs its coefficients. A web route (which we'll examine in detail later) needs the route, any variables it takes and probably the variables' types.

Everything else is fundamentally different from this core, essential information. It might still be necessary---we need plumbing to structure our code and specify exactly what we're *doing* with all that essential information---but it's still important to make the distinction. Often, a substantial amount of this inessential code is just boilerplate. I try to minimize this as much as possible, although it can be useful as a mnemonic and to give our information a bit more structure.

The point is not to make an immediate conclusion based on what is and isn't essential but consider it. It should influence how you organize and present your code. As a general guide though, I try to eliminate more and more of the inessential code as I repeat a particular kind of expression more and more. In extreme cases, a table layout might be the most readable option if you have a whole bunch of structured rows of code repeating.

</div>
<div class="content">

## Math Notation

A great example in my eyes is mathematical notation. Compare the following two ways of writing the same expression:

![A paragraph about integrating a polynomial compared to normal notation for the same integral.](integral-notation.png)

This is, perhaps, a bit of an exaggerated example, but it illustrates the general gist I want to convey.

The integral notation might be *unfamiliar* at first, but it's wonderfully efficient. You can tell what the expression represents *immediately* thanks to the integral sign and the general equation layout. Imagine squinting until you see the general outline of *any* integral expression---that's what I mean when I say "shape of the code".

It's easy to quickly identify parts of the equation to figure out what's going on: the limits are distinct from the equation itself and distinct from the variable of integration (ie \(dx\)). The polynomial itself continues the same theme: \(+\) as an operator gives structure to the polynomial, emphasizing its nature as a *set of terms*. In a sense, the \(+\) is just plumbing that gets out of our way so that we can identify the content *specific* to this polynomial (namely the coefficients and degree).

The paragraph, on the other hand, has the advantage of being readable by anyone---even if they aren't familiar with the special notation for integrals. But it has a fatal flaw: you *have* to read it. Every time. We can glance quickly over the numbers but we wouldn't understand what they *are*---limits or coefficients of a polynomial---we'd have to read the sentence word by word. Similarly, we can't even tell that this is an integral of a polynomial without reading.

If this paragraph was surrounded by other paragraphs, we wouldn't be able to tell it apart from other prose. I regularly skim through papers by skipping the explanations and looking at just the equations until I find what I need---I wouldn't be able to do that without the special notation.

This is why I am not a big fan of the Ruby and CoffeeScript style that tries to make code read like English. Just like reading math in paragraph form, *reading code is the last thing I want to do!*

### Unfamiliarity

Is unfamiliarity a problem? It can be. But I think it's fair to have people learn the notation along with the concept. If it's the first time they're looking at that sort of code, they'll have to do some additional reading anyhow to understand what's going on at all. That documentation can just describe the notation as well, which will pay off every single time the person encounters the same concept again.

This is a specific instance of a general pattern: you have a significant up-front effort to reap an ongoing reward. People often don't like this, but it's a benefit in the long term---the cost is \(O(1)\) and the benefit \(O(n)\).

</div>
<div class="content">

## Routing

Let's look at a case study that's closer to what many programmers work on---routing for web applications. This is another somewhat extreme example because in a web application of any size there tend to be *a lot* of routes with more or less the same structure. The benefits of being able to skim through your routes to find the one you want are larger than for most other kinds of code.
