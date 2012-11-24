---
title: Card Games Library
author: Tikhon Jelvis
---

<div id="games">
  <div class="controls">
    <ul>
      <li id="Klondike-control" class="active"> <a href="#"> Klondike </a> </li>
      <li id="War-control"> <a href="#"> War </a> </li>
    </ul>
  </div>
  <div id="Klondike">
  </div>
  <div id="War">
  </div>
</div>

<div class="content">

# Card Game Library

Note: this seems to only work in Chrome. Since this was just a hackathon project, we did not bother supporting any other browsers.

For the spring 2011 Berkeley [CSUA hackathon](http://www.huffingtonpost.com/marissa-louie/for-the-win-at-the-berkel_b_844749.html) two friends and I designed and implemented a library for easily creating JavaScript card games. The goal behind the library was simple: the library's user should be able to focus only on the rules of the game without worrying about the underlying HTML, CSS and JavaScript.

I worked primarily on the abstractions in the library as well as the Klondike game. So the files I mostly wrote were {Card, Deck, Hand, Solitaire}.js. [Jonathan Ewart](https://sites.google.com/site/jonathanewart/) worked on the actual UI code and the game of War and [Valerie Woolard](http://valeriewoolard.com/) worked on the graphics and visual design. 

We ended up getting 2nd place at the hackathon and winning Kindles, which was nice.

<div class="hideable">

The Klonide source:  [<a href="#" class="hide-control"> show </a>]

<div class="hide" style="display:none">

```javascript
$klondike$
```

[<a href="#" class="hide-control"> show </a>]

</div>
</div>

You can get the full source on [Google Code](http://code.google.com/p/blarg).

</div>