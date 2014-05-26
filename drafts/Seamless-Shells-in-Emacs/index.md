---
title: Seamless Shells in Emacs
author: Tikhon Jelvis
---

I am an avid Emacs user, almost to the point of self-parody. I don't run Emacs from my shell---I run my shell from Emacs. And yet, there is reason to my madness. I've worked out a convenient workflow for managing multiple shells, often for multiple projects or even over multiple machines, while **minimizing context switches**.

Everything is in Emacs, so I can easily switch between normal and shell buffers. All my usual navigation and editing commands work for shell output, so it's very easy to inspect and interact with large amounts of output. All my little tweaks and custom functions translate to my terminal. Using TRAMP, opening a shell on a remote machine is little different from opening a file there, with Emacs taking care of all the SSH details in the background.

I manage all of this through a handful of really simple Emacs functions and external programs. In essence, it's a shell force multiplier not unlike tmux: over time, I've found myself managing dozens of shells in all sorts of configurations, practically without noticing. It's that easy.

Here, I'm going to go through the various ways I use Emacs shells and introduce the few custom components I've written to support them. I'll also cover the shortcomings and little bugs I haven't bothered fixing---I would love help and pull requests on these!