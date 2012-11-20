<?php $homePath = '..'; include '../head.php'; ?>

<title>
  Cow: Semantic Version Control
</title>

</head>
<body>
  <?php include '../header.php'; ?>

  <div class="content">
    <h1 class="header">
      Cow: Semantic Version Control
    </h1>
    <p>
      Cow is a tool to compare and merge source code more
      intelligently.  I worked on this project
      with <a href="http://ankurdave.com">Ankur Dave</a>. It is
      completely free and open source, with the code available
      on <a href="http://github.com/TikhonJelvis/Cow"> GitHub </a>.
    </p>
    <p>
      Watch a brief <a href="cow-out.ogv">screencast</a> giving a demo
      of the system's basic capabilities.
    </p>
  </div>
  <div class="content">
    <h2 class="header">
      Description
    </h2>
    <p>
      Normal diff and merge tools operate on strings&mdash;they treat
      each file as plain text and compare that. However, source code
      is more than just plain text: it has some inherent, underlying
      structure and meaning that could be used to find and resolve
      differences more effectively.
    </p>
    <p>
      Existing tools just throw all this additional information
      away. This has some advantges in the general case&mdash;it works
      for any text at all. However, this is distinctly sub-optimal for
      working with well-formed programs, which happens to be exactly
      what these tools are most often used for!
    </p>
    <p>
      Cow is an attempt to take advantage of the structure and even
      meaning of source code when comparing and merging programs. At
      its heart, the idea is simple: Cow parses the programs and
      compares the ASTs. After this, it also does some additional
      analysis like matching up subtrees and identifying variables'
      scopes. 
    </p>
    <p>
      After getting all this information about the programs, it can
      compare them at a higher level. Since it parsed the programs
      first, it is not affected by differences in whitespace. It can
      also detect semantic actions like moves and variable renames. It
      can even deal with overlayed changes&mdash;it finds moved functions
      even if those functions were somewhat modified.
    </p>
    <p>
      This also holds for conflict resolution. The system can separate
      conflicts that overlap. This means that it can resolve a greater
      number of conflicts automatically and can make interactive
      merging much simpler.
    </p>
    <p>
      This project is a work in progress (albeit on hiatus at the
      moment). Currently the basic features&mdash;diffing trees,
      finding matching subtrees, scope analysis and so on&mdash;all
      work. However, the performance is entirely
      untenable. Additionally, there is no real UI which makes it
      entirely unusable. However, both problems can be rectified with
      a bit of work and a bit of cleverness, so a fully working
      version of the program should emerge in the near future.
    </p>
  </div>

  <?php include '../footer.php'; ?>
</body>
</html>

