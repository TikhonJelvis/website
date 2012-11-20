<?php $homePath = '..'; include '../head.php'; ?>

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
        <?php echo "<a href=\"$homePath/cow\">" ?>
          Cow
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

  <div class="content">
    <p>
      I often work on random programming projects in my spare
      time. This is a collection of my projects that seem at least
      moderately interesting. I haven't put <em>all</em> of my
      projects here yet. Also, some of the ones I have put on here are
      still works in progress: nothing is guaranteed to work!
    </p>
    <p>
      I wrote some of these projects on my own. However, most of the
      particularly fun ones were done with others' help. I've tried to
      link to my collaborators' sites on the appropriate projects;
      they are all worth visiting.
    </p>
  </div>

<?php include '../footer.php' ?>
</body>
</html>



