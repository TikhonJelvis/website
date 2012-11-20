<?php
$homePath = '..';

include '../head.php';
?>
<title>
  Color Grapher
</title>

<script src="js/jquery.js" type="text/javascript"></script>
<script src="js/drawing.js" type="text/javascript"></script>

<link rel="stylesheet" type="text/css" href="css/drawing.css" />

</head>

<body>
<?php

include '../header.php';
       
?>

<div id="container" class="content">
  <h1 class="header">
    Color Grapher
  </h1>
<div id="help">
  <p>
    <span class="hide">[<a href="#" class="hidehelp"> hide </a>]</span>
  </p>
    <h2 class="header">
      Instructions
    </h2>
	<p>
	  To draw a picture, write a javascript function in the text
	  box below. This function will be called for every pixel and
	  will be given four arguments: <span class="code">x</span>, 
	  <span class="code">y</span>, <span class="code">d</span> and
	  <span class="code">a</span>. The first two correspond to the
	  cartesian coordinates of the point; the latter two are the
	  polar coordinates with <span class="code">d</span> being the
	  distance to the center and <span class="code">a</span> being
	  the angle.
	</p>
	<p>
	  The origin is the center of the drawing area; the angle is
	  in radians, going counter-clockwise from the
	  x-axis. Basically, this works exactly as it would in a math
	  class. 
	</p>
	<p>
	  In addition to the pixel's position, the function will have 
	  <span class="code">height</span>
	  and <span class="code">width</span> accessible. These
	  correspond to the height and width of the drawing area; if
	  the area is resized, they will change.
	</p>
	<p>
	  The function should return
	  a <span class="code">Color</span>. This can be done by
	  calling <span class="code">return new Color(r, g, b);</span>
	  at the very end of the function
	  where <span class="code">r</span>, <span class="code">g</span>
	  and <span class="code">b</span> correspond to the red, green
	  and blue channels of the color respectively.
	</p>
	<p>
	  For convenience, all of the properties of the 
	  <span class="code">Math</span> class are available without
	  needing to reference <span class="code">Math</span>. That
	  is, instead of calling <span class="code">Math.cos</span>,
	  you can just use <span class="code">cos</span>.
	</p>	  	  
	<p>
	  Here is a simple example of a function that can be used
	  here, which creates an interesting pattern:
	</p>
	<pre>
function (x, y, d, a) {
  var r = cos(round(x / 10, 0)) * 256;
  var g = sin(round(y / 10, 0)) * 256;
  var b = random() * d;

  return new Color(r, g, b);
} </pre>
          
      </div>
      <p>
	[<a href="#" class="hidehelp"> hide </a>]
      </p>
      <p>
	This only works on browsers that support pixel manipulation
	using canvas.
      </p>
      <div id="code">
	<textarea id="codeInput" rows="25" cols="80"></textarea>
	<br />
        <div class="controls">
          <div class="buttons">
	    <input id="render" type="button" value="Draw!" />
	    <input id="open" type="button" value="Open" />
          </div>
          <div class="dimensions">
	    <label for="width"> width: </label>
            <input id="width" class="dimension" type="text" value="400" maxlength="6" />
	    <label for="height"> height: </label>
	    <input id="height" class="dimension" type="text" value="400" maxlength="6" />
          </div>
        </div>
      </div>

      <div id="display">
      </div>
    </div>

      <?php
	 include '../footer.php';
      ?>
  </body>
</html>
