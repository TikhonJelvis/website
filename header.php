<?php
if (!$homePath) {
  $homePath = "";
}
?>

<?php
function latex() {
   echo <<<'EOD'
	     <span class="latex">
	       <span class="L">L</span>
	       <span class="A">A</span>
	     </span>
EOD;
   tex();
}

function tex() {
   echo <<<'EOD'
	     <span class="tex">
	       <span class="T">T</span>
	       <span class="E">E</span>
	       <span class="X">X</span>
	     </span>
EOD;
}
?>

<!-- This contains both the header and the navigation. -->
<div id="top">

<!-- This is the title with my glorious name on it. -->
<div id="header">
<div>
<div id="headerBackground">
<img src="<?php echo "$homePath/"?>img/blueHeader.png" alt="Tikhon Jelvis" />
</div>
</div>
</div>

<div id="navigation">
<div>
<ul>
<li>
<?php
echo "<a href=\"$homePath\">"
?>
Home
</a>
</li>
<li class="tpl">
<?php
echo "<a href=\"$homePath/tpl\">"
?>
tpl
</a>
</li>
<li>
<?php
echo "<a href=\"$homePath/cards\" title=\"Card Game Library\">"
?>
Cards
</a>
</li>
<li>
<?php
echo "<a href=\"$homePath/chess\" title=\"Maptac Chess\">"
?>
Chess
</a>
</li>
<li>
<?php
echo "<a href=\"$homePath/draw\" title=\"Draw stuff with
JavaScript!\">"
?>
Drawing
</a>
</li>
<li>
<?php
echo "<a href=\"$homePath/simulation\">";
?>
Simulation
</a>
</li>
<li>
<?php
echo "<a href=\"$homePath/latex\" title=\"All your typesetting needs!\">\n";
latex();
?>
</a>
</li>
</ul>
</div>
</div>

</div>
