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
<ul class="navigation">
<li>
<?php
echo "<a href=\"$homePath\">"
?>
About
</a>
</li>
<li>
  <?php
     echo "<a href=\"$homePath/projects\">"
  ?>
    Projects
  </a>
</li>
<li>
  <a href="https://github.com/TikhonJelvis" title="My projects">GitHub</a>
</li>
<li>
  <a href="http://careers.stackoverflow.com/tikhonjelvis">Resume</a>
</li>
</ul>
</div>
</div>

</div>
