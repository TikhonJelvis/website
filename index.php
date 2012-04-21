<?php
$homePath = "."; 
include "head.php";
?>
<link rel="stylesheet" type="text/css" href="css/index.css" />

<title>
  Tikhon Jelvis
</title>

</head>

<body>

<?php
include 'header.php'
?>

<!-- This is the div where the actual contents of the page will go. -->
<div class="content" id="news">
<h1 class="header">
News
</h1>
<p>
I was on the team that got second place in this semester's CSUA hackathon (team
blarg)! The project we made can be found
on <a href="http://code.google.com/p/blarg">Google code</a>; you can read about
the 
event <a href="http://www.huffingtonpost.com/marissa-louie/for-the-win-at-the-berkel_b_844749.html"
title="An article about the hackathon.">here</a>. Apart from glory, we also all
got Amazon Kindles!
</p>
</div>
<div class="content" id="languages">
<h1 class="header">
Programming Languages
</h1>
<p>
  Here are the programming languages I'm familiar with, roughly in
  order of preference:
</p>
<ul class="languages">
  <li class="haskell">
    <a href="http://en.wikipedia.org/wiki/Haskell_(programming_language)">
      <img src="img/haskell.png" alt="Haskell" title="Haskell" />
    </a>
  </li>
  <li class="javascript">
    <a href="http://en.wikipedia.org/wiki/JavaScript">
      <img src="img/javascript.png" alt="JavaScript" title="JavaScript" />
    </a>
  </li>
  <li class="elisp">
    <a href="http://en.wikipedia.org/wiki/Emacs_Lisp">
      <img src="img/elisp.png" alt="Emacs Lisp" title="Emacs Lisp" />
    </a>
  </li>
  <li class="scheme">
    <a href="http://en.wikipedia.org/wiki/Scheme_(programming_language)">
      <img src="img/scheme.png" alt="Scheme" title="Scheme" />
    </a>
  </li>
  <li class="c">
    <a href="http://en.wikipedia.org/wiki/C_(programming_language)">
      <img src="img/c.png" alt="C" title="C" />
    </a>
  </li>
  <li class="prolog">
    <a href="http://en.wikipedia.org/wiki/Prolog_(programming_language)">
      <img src="img/prolog.png" alt="Prolog" title="Prolog" />
    </a>
  </li>
  <li class="tex">
    <a href="http://en.wikipedia.org/wiki/TeX">
      <img src="img/tex.png" alt="TeX" title="TeX" />
    </a>
  </li>
  <li class="java">
    <a href="http://en.wikipedia.org/wiki/Java_(programming_language)">
       <img src="img/java.png" alt="Java" title="Java" />
    </a>
  </li>
  <li class="python">
    <a href="http://en.wikipedia.org/wiki/Python_(programming_language)">
      <img src="img/python.png" alt="Python" title="Python" />
    </a>
  </li>
  <li class="perl">
    <a href="http://en.wikipedia.org/wiki/Perl">
      <img src="img/perl.png" alt="Perl" title="Perl" />
    </a>
  </li>
</ul>
</div>
<div class="content" id="main">
<h1 class="header">
Welcome
</h1>
<div class="caption">
  <a href="img/me_at_casa_mila_med.jpg" title="Click for full size">
    <img src="img/me_at_casa_mila_med.jpg" alt="A nice picture of me in Spain"
/> 
  </a>
  <p>
    A gratuitous picture of me on the roof of Casa Mil&agrave; in
    Barcelona. You can barely make out the Sagrada Fam&iacute;lia in the
    background. 
 </p>
</div>
<p>
I am a freshman Electrical Engineering and Computer Sciences (EECS)
major at the University of California, Berkeley. I am particularly
interested in computer science and programming. Additionally, I am
casually interested in digital typography, fencing and ping pong.
</p>
<h2 class="header">
EECS
</h2>
<p>
EECS stands for "Electrical Engineering and Computer Sciences", which
is really a bit of a mouthful, so everybody just calls it "EECS"
(pronounced "eeks"). This major is a superset of what most schools
refer to as "computer engineering"&mdash;an EECS major can get a
"computer engineering"-style education, but can also study either EE
or CS almost exclusively.
</p>
<p>
I am a big fan of the EECS major because of its extreme
flexibility&mdash;I not only have freedom within EE and CS, but I also
have freedom without it: I am free, and have enough time, to take
classes just from interest without worrying unduly about graduation
requirements. Additionally, I think this major is particularly well
suited for people who, like me, are primarily interested in
CS&mdash;electrical engineering may be boring, but some is necessary,
and I will probably get a more thorough grounding in it than I would
in a pure CS major.
</p>
<h2 class="header">
Programming
</h2>
<p>
I have been programming on my own for a relatively long time now. My
very first exposure to actual programming was in seventh grade,
towards the latter half of 2004, when I started learning
JavaScript. The state of JavaScript at that time was very different
from now; I remember commenting out the actual code inside the
<span class="code">script</span> tag because older browsers did not
acknowledge script tags at all&mdash;a positively ancient practice now!
</p>
<p>
Ever since learning JavaScript on my own, I have continued to
program. Initially, I was concentrating almost entirely on
mechanics&mdash;I was having enough difficulty doing anything at all,
so I didn't even consider trying to finish something coherent. Later
on, I began to work on increasingly large projects; however, thanks to
my poor practices and lack of back ups, most of my very early projects
have disappeared irretrievably.
</p>
<p>
As time went on, my technical grasp of programming improved, as did my
practices. This means that I have a couple relatively large, coherent and
somewhat well-written projects from the latter half of high school.
</p>
<h3 class="header">
  Simulation
</h3>
<p>
I took my first CS class during junior year of high school; here I
learned Java, my first compiled language. I wrote several projects
during this time, but lost the first couple on a flash drive. I
learned to keep back ups after that! The last project of junior year
was the most interesting. I was assigned a <em>very</em> open-ended
project for physics, so decided, along with some friends, to write a
program for it.
</p>
<p>
We ended up making a fairly nice 2D polygon collision simulation with a
thorough UI that, while weak on user experience and not very intuitive, is
relatively useable and rather capable.
</p>
<p>
This project, even now, is the my largest by lines of code&mdash;it has
somewhere around 8500 lines written by two people. Of course, one of the main
reasons for this is Java: not exactly the most concise language. That said,
this was a fairly complicated project; <a href="simulation" title="User
Guide">here</a> is a user guide for it. It can be found on
<a href="http://www.github.com/TikhonJelvis/simulation">github</a> as well.
</p>
<h2 class="header">
Other Projects
</h2>
<p>
I have finished several projects since the collision simulation. Right now,
some projects can be accessed via the menu above: <a href="chess" title="Maptac
Chess"> chess </a> and <a href="draw" title="Draw with JavaScript!">
drawing </a>. However, this is nowhere near an exhaustive list: I mean to put
up a proper project page, but have yet to do so. 
</p>
<p>
All my projects from now on should be up
on <a href="http://www.github.com/TikhonJelvis" title="My github
page.">github</a>. I also mean to move my old projects to github (they are
mostly on Google code right now), but I will probably never get around to it. 
</p>
<h2 class="header">
This Site
</h2>
<p>
This site is currently hosted on the EECS instructional Unix server at
inst.eecs.berkeley.edu. Both the xhtml and css should be standards compliant;
feel free to complain to me if it isn't! 
</p>
</div>

<?php
include 'footer.php'
?>

</body>

</html>
