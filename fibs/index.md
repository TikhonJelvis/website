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

I originally made these visualizations for a blog post about dynamic programming that I'm still working on. But I think they're pretty interesting by themselves, and I'll probably try making more for other sorts of interesting lazy patterns later.

</div>