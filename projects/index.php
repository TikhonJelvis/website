<?php $homePath = '..'; include '../head.php'; ?>

<link rel="stylesheet" type="text/css" href="css/projects.css" />

<title>
  Projects
</title>

</head>
<body>
  <?php include '../header.php'; ?>

  <div class="content">
    <h1 class="header">
      Projects
    </h1>
  </div>

  <div id="projects" class="content">
    <ul class="navigation">
      <li class="tpl">
        <?php echo "<a href=\"$homePath/tpl\">" ?>
          tpl
        </a>
      </li>
      <li>
        <?php echo "<a href=\"$homePath/cards\" title=\"Card Game Library\">" ?>
          Cards
        </a>
      </li>
      <li>
        <?php echo "<a href=\"$homePath/chess\" title=\"Maptac Chess\">" ?>
          Chess
        </a>
      </li>
      <li>
        <?php echo "<a href=\"$homePath/draw\" title=\"Draw stuff with JavaScript!\">" ?>
          Drawing
        </a>
      </li>
      <li>
        <?php echo "<a href=\"$homePath/simulation\">"; ?>
          Simulation
        </a>
      </li>
    </ul>
  </div>

  <div id="after-projects" class="content">
    <p>
      I have worked on a bunch of side projects in my spare time. The
      projects vary drastically, but have one thing in common: they
      were all fun to work on. 
    </p>
    <p>
      Some of the projects were solo efforts, but many of the others
      would not have been possible without help from some eminently
      competent friends. Overall, these were the more fun
      projects. I've linked to the websites of my collaborators for
      every project; they are all worth visiting.
    </p>
  </div>

<?php include '../footer.php' ?>
</body>
</html>
