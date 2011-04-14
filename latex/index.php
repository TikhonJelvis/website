<?php
if (!$homePath) {
  $homePath = "..";
}
?>

<?php
include "../head.php";
?>

<title>
LaTeX
</title>

</head>

<body>

<?php
include "../header.php";
?>

<!-- The actual content goes here: -->
<div class="content">
<h1 class="header">
<?php latex() ?>
</h1>
<p>
<?php latex() ?> is a document preparation system, designed
particularly for scientific and mathematical papers; however, it is
very versatile, and can be used to typeset a wide variety of
documents. More information can be found on the project's page, found
<a href="http://www.latex-project.org">here</a>.
</p>
<h2 class="header">
  Examples
</h2>
<p>
One of <?php latex() ?>'s particular strengths is its separation of
content and style&mdash;this makes it very easy to change the style
without affecting the content and vice-versa.
</p>
<p>
In an effort to be useful, I have included here a set of <?php latex()
?> documents that can be used as references. For now, there are only
some class notes I took during my first semester (when I still went to most
of my lectures!) here.
</p>
<ul>
  <li>
    CS61A (first half of semester
    only): <a href="examples/cs61a.ltx"><?php latex()
    ?></a>, <a href="examples/cs61a.pdf">pdf</a>
  </li>
  <li>
    Math 1A: <a href="examples/math1a.ltx"><?php latex()
    ?></a>, <a href="examples/math1a.pdf">pdf</a>
  </li>
</ul>
<p>
Naturally, given my normal attention to quality and general diligence,
these notes should <em>not</em> be trusted from an academic
standpoint. However, I hope that they illustrate both the quality of
documents produced by <?php latex() ?> and the practicality of using
it to take notes in class.
</p>

<p>
Here is a copy of my current r&eacute;sum&eacute;. This is my first draft, and I am
not entirely happy with it, but I really like some things I did
here&mdash;particularly the contact information. It was largely
inspired by various r&eacute;sum&eacute;s I found online, particularly
<a href="http://www.mcnabbs.org/andrew/linux/latexres/" 
   title="Andrew McNabb resume.">Andrew McNabb's</a>. I looked at his
source and borrowed several bits, but mostly rewrote everything
myself.Feel free to use this 
r&eacute;sum&eacute; as a template for your own:
</p>
<ul>
  <li>
    R&eacute;sum&eacute;: <a href="examples/firstDesign.ltx"><?php latex() ?></a>, 
    <a href="examples/firstDesign.pdf">pdf</a>
  </li>
</ul>
<p>
  In the near future, this is going to get redone completely.
</p>
<h2 class="header">
  Useful Links
</h2>
<p>
Here are some useful links for more information about <?php latex()
?>:
</p>
<ul>
  <li>
    The <?php latex() ?>
    site: <a href="http://www.latex-project.org">www.latex-project.org</a>
  </li>
  <li>
    The <?php latex() ?> Wikipedia
    article: <a href="http://en.wikipedia.org/wiki/LaTeX">
      en.wikipedia.org/wiki/LaTeX 
    </a> 
  </li>
  <li>
    The Comprehensive <?php tex() ?> Archive Network (CTAN): 
    <a href="http://www.ctan.org">www.ctan.org</a>
  </li>
  <li>
    A (Not So) Short Introduction to <?php latex() ?>: 
    <a href="http://www.ctan.org/tex-archive/info/lshort/english/">
      www.ctan.org/tex-archive/info/lshort/english/
    </a>
  </li>
  <li>
    MathJax, a JavaScript <?php latex() ?> rendering engine:
    <a href="http://www.mathjax.org">www.mathjax.org</a>
  </li>
</ul>
</div>

<?php
include "../footer.php";
?>

</body>

</html>
