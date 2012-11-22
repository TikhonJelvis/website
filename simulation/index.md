---
title: 2D Collision Simulation
author: Tikhon Jelvis
---

<div class="content">

# 2D Collision Simulation

You can now get the code off [Github](https://github.com/TikhonJelvis/simulation)!

## Intro

This is a java program that simulates collisions between polygons. I wrote this for physics during the second semester of my junior year in high school. Although I worked with three other people, only one other helped with the programming; the other two only worked on other parts of the project necessary for the class (the powerpoint, the paper...etc).

I worked on the program itself with a friend, [Jacob Taylor](http://stanford.edu/~jacobt). He primarily worked on the actual physics, while I worked on the interface (written in Swing) and putting everything together. In putting everything together, I developed the over-arching design, which uses the listener pattern to separate the data from the presentation. Everything has to be a pattern in Java, after all.

## Powerpoint

As part of the project, we had to make and present a powerpoint presentation on the simulation. This presentation goes over the main points of the whole project, including parts that are not really related to the actual program (the physical experiment, for example). The presentation is available in pdf and ppt.

</div>

<div class="content">

# Overview

While in essence the simulation is a very simple program, it does
have quite a few features. I'm not going to list all of them right now, but here's a screenshot that illustrates its complexity.

![All the different windows for interacting with the simulation.](img/overview.png)

We support any sort of polygon as well as circles. You can add shapes when everything is paused, but you can also add them interactively. You can even drag and throw shapes around while the simulation is active, which turns out to be pretty fun. All the parameters of the world and each shape---things like mass, friction, bounciness and gravity---can be changed on the fly for some potentially amusing effects.

Everything becomes much more fun---but also less realistic---if you set bounciness very high and density very low. 