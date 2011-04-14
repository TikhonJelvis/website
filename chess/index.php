<?php
$homePath = "..";

include "../head.php";
?>
<title>
Maptac Chess
</title>

<link rel="stylesheet" type="text/css" href="css/chess.css" />
<link rel="stylesheet" type="text/css" href="css/board.css" />

<script src="js/prototype-1.6.0.2.js" type="text/javascript"></script>
<script src="js/scriptaculous.js" type="text/javascript"></script>
<script src="js/chessUtil.js" type="text/javascript"></script>
<script src="js/chess.js" type="text/javascript"></script>
<script src="js/chess-ai.js" type="text/javascript"></script>

</head>

<body id="body" onload="onld()">

<?php

include '../header.php';

?>

<div id="holder">
</div>
<div class="content">
<div id="disclaimer">
  <h1 class="header">
      Maptac Chess
  </h1>
  <h2 class="header">
     Details
  </h2>
  <p>
   You can see the moves made in the game by clicking the left tab and
   the captured pieces by clicking the right tab.
  </p>
  <p>
   This game was a project I worked on for fun during my second
   semester of senior year in high school. The idea was to learn how
   to use JavaScript in the browser while making something actually
   <em>useful</em>. The game did, ultimately, prove useful&mdash;I
   played several games against my friend on a smartboard at school,
   something that I can't really do any more since smartboards seem to
   be exclusively in the domain of elementary and high school.
  </p>		  
  <p>
   The AI for this game was created by <a
   href="http://www.stanford.edu/~jacobt">Jacob Taylor</a>. He based
   this on an AI he had written earlier in LISP; the project page for
   that AI can be found <a
   href="http://code.google.com/p/multi-game-ai/">here</a>. Once he
   had ported the AI to JavaScript, integrating it into the game was
   trivial.
  </p>
  <p>
   Being a program entirely written in JavaSript, xhtml and CSS, it
   has always been inherently open source. However, the code has also
   formally been released under an MIT license, and has a project page
   on Google code found <a
   href="http://code.google.com/p/maptac-chess">here</a>.
  </p>
</div>
</div>

<?php

include '../footer.php';

?>
</body>
</html>
