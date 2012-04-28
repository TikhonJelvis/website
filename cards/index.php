<?php
$homePath = "..";

include "../head.php";
?>
<title>
Card Games Library
</title>

<link rel="stylesheet" href="css/cards.css" type="text/css"/>
<link rel="stylesheet" href="css/jquery-ui-1.8.11.custom.css" type="text/css"/>

<script src="js/jquery-1.5.2.min.js" type="text/javascript"></script>
<script src="js/jquery-ui-1.8.11.custom.min.js" type="text/javascript"></script>
<script src="js/Card.js" type="text/javascript"></script>
<script src="js/Hand.js" type="text/javascript"></script>
<script src="js/Deck.js" type="text/javascript"></script>
<script src="js/Board.js" type="text/javascript"></script>
<script src="js/Solitaire.js" type="text/javascript"></script>

</head>

<body id="body">

<?php

include '../header.php';

?>

<div id="game-container">
  <div id="Klondike">
  </div>
</div>

<div class="content">
  <div id="disclaimer">
    <h1 class="header">
      Card Game Library
    </h1>
    <p>
      For the spring 2011
      Berkeley <a href="http://www.huffingtonpost.com/marissa-louie/for-the-win-at-the-berkel_b_844749.html">CSUA
      hackathon</a> two friends and I designed and implemented a
      library for easily creating JavaScript card games. The goal
      behind the library was simple: the library's user should be able
      to focus only on the rules of the game without worrying about
      the underlying HTML, CSS and JavaScript.
    </p>
    <p>
      All in all our goal was met: with our library, you could
      easily implement games like War or Klondike Solitaire without
      worrying about pesky underlying details. The Klondike game, for
      example, is less than 100 lines! War is even shorter. 
    </p>
    <p>
      I worked primarily on the abstractions in the library as well as
      the Klondike game. So the files I mostly wrote were {Card, Deck,
      Hand,
      Solitaire}.js. <a href="https://sites.google.com/site/jonathanewart/">Jonathan
        Ewart</a> worked on the actual UI code and the game of War and
      <a href="http://valeriewoolard.com/">Valerie Woolard</a> worked
      on the graphics and visual design. We ended up getting 2nd place
      in the hackathon.
    </p>
    <div class="hideable">
      <p>
        The Klondike source: [<a href="#" class="hide-control"> show </a>]
      </p>
      <div class="hide" style="display:none">
        <?php include "klondike.js.php" ?>
        <p>
          [<a href="#" class="hide-control"> show </a>]
        </p>
      </div>
    </div>
  </div>
</div>

<?php

include '../footer.php';

?>
</body>
</html>
