---
title: Visualizing Laziness: Fibonacci
author: Tikhon Jelvis
---

<div class="content">

# Visualizing Laziness

Recently, I've been playing around with different ways to visualize laziness. I've found it really helps me when thinking about exactly what goes on with some Haskell code.

A common example of how laziness is elegant is a memoized version of Fibonacci:

```haskell
fib n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

The trick is that we define a list of all the Fibonacci numbers *that depends on itself*. This then takes care of memoizing exactly how much we need.

At each point, the list has its calculated elements as well as a thunk corresponding to the rest of `zipWith (+)`. The neat bit here is that the arguments are just pointers---back to elements of the same list! It looks like this:

<div id="fibs-animation" class="figure">
<ul class="animation">
  <li> <img src="fib-frames/frame0.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames/frame1.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames/frame2.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames/frame3.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames/frame4.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames/frame5.png" alt="The fibs list in memory." />
  </li>
</ul>
  <style type="text/css">
  #fibs-animation ul li {
    text-align : left;
  }
  </style>
  <script type="text/javascript">
    animate("#fibs-animation");
  </script>
  <p> `zipWith f` applies `f` to the first elements of both lists then recurses on their tails. In this case, the two lists are actually just pointers into the same list!</p>
</div>

However, things are actually even better than this. Since we have no other references to the `fibs` list, GHC's garbage collector can reclaim unused elements as soon as we're done with them. So with GC, the actual execution looks more like this:

<div id="fibs-gc-animation" class="figure">
<ul class="animation">
  <li> <img src="fib-frames-gc/frame0.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames-gc/frame1.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames-gc/frame2.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames-gc/frame3.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames-gc/frame4.png" alt="The fibs list in memory." />
  </li>
    <li> <img src="fib-frames-gc/frame5.png" alt="The fibs list in memory." />
  </li>
</ul>
  <style type="text/css">
  #fibs-gc-animation ul li {
    text-align : left;
  }
  </style>
  <script type="text/javascript">
    animate("#fibs-gc-animation");
  </script>
  <p> More memory efficient: we only ever store a constant number of past results.</p>
</div>

So really, the `fibs` list is not so much a data structure as a clever loop with two variables used to calculate the next value at each step. We just evaluate as much of this loop as we need to return the nth number.

## Dynamic Programming

I originally made these visualizations for a blog post about dynamic programming that I'm still working on. But I think they're pretty interesting by themselves, and I'll probably try making more for other sorts of interesting lazy patterns later.

Of course, dynamic programming is usually done with arrays rather than lists. Happily, we can make this quite elegant with laziness too!

The basic trick is to take a recursive function and replace the recursive calls with indexes into an array. Then we define the array with calls to the function itself! We can write `fib` in this style as well, although it's much less efficient for this particular problem:

```haskell
fib' max = go max
  where go 0 = 0
        go 1 = 1
        go n = fibs ! (n - 1) + fibs ! (n - 2)
        fibs = Array.listArray (0, max) [go x | x <- [0..max]]
```

It's a function defined in terms of an array which is defined in terms of the function! Very circular.

In memory, it looks something like this:

![The array of sub-problems for `fib 5`.](fib-array.png)

Each entry in the array (except the first two) is a thunk for `go n`, where `go n` is just `+` with pointers back into the array. These pointers make the dependencies inherent in the problem more explicit: the code has *explicit* dependencies between cells.

This is quite nice because it makes all the dependencies in our dynamic programming problem explicit. Instead of manually reflecting the dependencies by mutating an array, we've specified them directly in our recursion. All the bookkeeping is then done for us by the lazy evaluation rules. Apart from making the code higher-level and clearer, it also prevents a bunch of possible mistakes: we can't access a cell before we've written to it and we can't overwrite an existing value: after all, we can only evaluate thunks.

This is a good example of using laziness to **structure** mutation, much in the same way structured programming *structures* jumps. By being more explicit about *what* we want and less about *how*, we can restrict ourselves in a way that prevents many possible bugs.

</div>