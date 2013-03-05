---
title: Mazes
author: Tikhon Jelvis
---

<div class="content">

# Maze Generation

A while back, I was fascinated with mazes. In particular, I was interested in how I could generate mazes programmatically and how these randomly generated mazes behave.

Here is an example maze:

<div class="maze"></div>
</div>

<div class="content">

## Background

To play around with this, I wrote a really simple little JavaScript program to draw simple mazes using Canvas. Earlier, in high school, I had written a similar program in Java, using Swing. However, this was before I knew anything about version control or backups. Happily, writing the code in JavaScript was much more pleasant than writing it in Java.

The algorithm I use to generate the mazes is actually extremely simple: it's just a randomized depth-first search. I create a grid of walls and knock the walls out each time I move from cell to cell, randomly choosing neighboring cells I haven't visited yet. If I get into a position where I've visited all the neighbors, I backtrack, which is what makes this essentially a DFS.

## Statistics

Now that I have a program that could generate mazes, I wanted to know more about how it behaves. My main question was simple: what's the average length of a solution to the maze? (A solution is just a path from one corner to its opposite.) I had literally *no* idea of how to approach this problem analytically, although I did spent a fair amount of thought on it.

Happily, I'm a computer programmer---if I'm not smart enough to solve the problem, I can just use brute force! So I wrote a simple little script that generated a whole bunch of random mazes and plotted their solution lengths on a little graph. Here is such a graph:

<div class="figure">
<div class="maze-graph-controls"><div>
Samples: <input type="text" id="maze-samples" value="1000"></input>
Maze size: W<input type="text" value="10" id="maze-width"></input>         H<input type="text" value="10" id="maze-height"></input>
<input type="button" id="maze-recalculate" value="recalculate"></input>
</div>
</div>
<div class="maze-graph"></div></div>

Even with relatively few (1000) sample points, it's clear there is some sort of non-normal distribution. You can get a much smoother graph by increasing the number of sample points; however, this can take a little while to calculate. It is using completely unoptimized JavaScript, after all. You can also change the size of each sample maze, although much bigger sizes will take significantly longer to calculate.

So I now had a good approximation of the average solution length of a maze! This is where I learned another lesson: numeric methods are extremely unsatisfying. Sure, I had an answer, but it was just that---an answer. I got no real insight by running this experiment. It was still a fun exercise, but an analytic solution would have been much better. 

I still haven't figured out how to solve this problem without random sampling. After a little while I just gave up and never got back to it. If you how to solve it, please email me!

</div>