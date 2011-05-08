<?php
$homePath = ".."; 
include "$homePath/head.php";
?>

<title>
  2D Collision Simulation
</title>

</head>

<body>

<?php
include "$homePath/header.php";
?>

<div class="content" id="main">
<h1 class="header">
  2D Physics Collision Simulation
</h1>
<p>
  You can now get the code off 
  <a href="http://www.github.com/TikhonJelvis/simulation">Github</a>!
</p>
<h2 class="header">
  Intro
</h2>
<p>
  This is a java program that simulates collisions between
  polygons. I wrote this for physics during the second semester of my
  junior year in high school. Although I worked with three other
  people, only one other helped with the programming; the other two
  only worked on other parts of the project necessary for the class
  (the powerpoint, the paper...etc).
</p>
<p>
  I worked on the program itself with a friend, namely Jacob
  Taylor. His page can be
  found <a href="http://www.stanford.edu/~jacobt">here</a>. He
  primarily worked on the actual physics, while I worked on the
  interface (written in Swing) and putting everything together. In
  putting everything together, I developed the over-arching design,
  which uses the listener pattern to separate the data from the
  presentation. 
</p>
<h2 class="header">
  Powerpoint
</h2>
<p>
  As part of the project, we had to make and present a powerpoint
  presentation on the simulation. This presentation goes over the main
  points of the <em>whole</em> project, including parts that are not
  really related to the actual program (the physical experiment, for
  example). The presentation is available
  in <a href="simulation.pdf">pdf</a>
  and <a href="simulation.ppt">ppt</a>.
</p>
</div>
<div class="content" id="guide">
<h1 class="header">
  Guide
</h1>
<h2 class="header">
  Overview
</h2>
<p>
  While in essence the simulation is a very simple program, it does
  have quite a few features available that need to be covered. Here I
  will go over the various windows and menus that can be used to
  interact with the system.
</p>
<div class="caption">
  <a href="img/overview.png" title="Click for full size">
    <img src="img/overview.png" alt="A picture of all of the
					       different windows in
					       the simulation." />
  </a>
  <p>
    This is a picture of all of the different windows for interacting
    with the simulation.
  </p>
</div>
<h2 class="header">
  Main Window
</h2>
<p>
  The main window has a view of the simulation along with a collection
  of tools and options. When this window is closed, the program exits.
</p>
<div class="caption">
  <a href="img/main.png" title="Click for full size">
    <img src="img/main.png" alt="The main window of the
    simulation." />
  </a>
  <p>
    This is the main window of the simulation.
  </p>
</div>
<h3 class="header">
  Tools
</h3>
<p>
  On this window you can interact with the simulation directly. The
  toolbar on the left-hand side has a selection of tools:
</p>
<table>
  <tr>
    <td>
      Icon
    </td>
    <td>
      Name
    </td>
    <td>
      Function
    </td>
  </tr>
  <tr>
    <td>
      <a href="img/dragIcon.png" title="Click for full size">
	<img src="img/dragIcon.png" alt="The drag tool icon." />
      </a>
    </td>
    <td>
      Drag Tool
    </td>
    <td>
      Allows you to drag objects in the simulation around. This only
      works when the simulation is playing; you drag objects by
      applying a force to the point that you click when dragging the
      object. The heavier the object, the slower it will react to
      being dragged. Using this tool is surprisingly
      entertaining. Additionally, even when using some other tool, you
      can drag shapes in the main view by using the middle mouse button.
    </td>
  </tr>
  <tr>
    <td>
      <a href="img/circleIcon.png" title="Click for full size">
	<img src="img/circleIcon.png" alt="The circle tool icon." />
      </a>
    </td>
    <td>
      Circle Tool
    </td>
    <td>
      Allows you to add circles to the simulation by clicking and
      dragging. Shapes can be added both when the simulation is paused
      and when it is playing. 
    </td>
  </tr>
  <tr>
    <td>
      <a href="img/rectangleIcon.png" title="Click for full size">
	<img src="img/rectangleIcon.png" alt="The rectangle tool icon." />
      </a>
    </td>
    <td>
      Rectangle Tool
    </td>
    <td>
      Allows you to add rectangles to the simulation by clicking and
      dragging. This works the same way as the circle tool.
    </td>
  </tr>
  <tr>
    <td>
      <a href="img/polygonIcon.png" title="Click for full size">
	<img src="img/polygonIcon.png" alt="The polygon tool icon." />
      </a>
    </td>
    <td>
      Polygon Tool
    </td>
    <td>
      Allows you to add arbitrary polygons to the simulation. The
      first time this is used, the polygon pad will pop up and you
      have to define the polygon to draw. After this, it will draw the
      polygon defined there (except it allows you to change the
      scale). You can go back to the pad at any time to change the
      polygon being drawn.
    </td>
  </tr>
  <tr>
    <td>
      <a href="img/polygonPadIcon.png" title="Click for full size">
	<img src="img/polygonPadIcon.png" alt="The polygon pad icon."
	/> 
      </a>
    </td>
    <td>
      Polygon Pad
    </td>
    <td>
      Allows you to specify what sort of polygon the polygon tool
      should add. You can change this polygon at any time.
    </td>
  </tr>
</table>
<h3 class="header">
  Menus
</h3>
<p>
  The <span class="uiElement">file</span> menu has two options: one
  for resetting the simulation (removing all objects) and another for
  exiting the program.
</p>
<p>
  The <span class="uiElement">tools</span> menu has all of the tools
  that the sidebar does as well as several other options:
</p>
<table>
  <tr>
    <td>
      Name
    </td>
    <td>
      Description
    </td>
  </tr>
  <tr>
    <td>
      <span class="uiElement">Explore</span>
    </td>
    <td>
      Opens the shape explorer. 
    </td>
  </tr>
  <tr>
    <td>
      <span class="uiElement">Defaults</span>
    </td>
    <td>
      Opens the defaults editor.
    </td>
  </tr>
  <tr>
    <td>
      <span class="uiElement">Walls</span>
    </td>
    <td>
      Lets you toggle some or all of the walls.
    </td>
  </tr>
</table>
<p>
  The shape explorer and defaults editor are covered later on. The
  walls are immobile objects that surround the default visible area
  help contain the objects within the visible area.
</p>
<p>
  The <span class="uiElement">presets</span> menu has a collection of
  interesting preset configurations that show off some of the
  simulation's capabilities. Currently, there are three:
</p>
<table>
  <tr>
    <td>
      Name
    </td>
    <td>
      Description
    </td>
  </tr>
  <tr>
    <td>
      <span class="uiElement">Falling Circles</span>
    </td>
    <td>
      Starts spawning circles every couple of seconds. The circles are
      close to each other in color and fall from above the default
      view.
    </td>
  </tr>
  <tr>
    <td>
      <span class="uiElement">Rectangular Collisions</span>
    </td>
    <td>
      Has three pairs of colliding rectangles where each rectangle is
      at a different angle. This was created to show how the
      simulation corresponds with the physical experiment which was
      also part of this project.
    </td>
  </tr>
  <tr>
    <td>
      <span class="uiElement">Shooting Circle</span>
    </td>
    <td>
      This shows off a neat trick: four circles of increasing size and
      mass are lined up on top of one another and dropped; when they
      hit the floor a large part of the energy is transferred to the
      smallest circle which bounces up really high. 
    </td>
  </tr>
</table>
<p>
  The <span class="uiElement">help</span> menu only contains the about
  window. Here is a picture of the about window:
</p>
<div class="caption">
  <a href="img/about.png" title="Click for full size">
    <img src="img/about.png" alt="The about window of the
    simulation." />
  </a>
  <p>
    This window gives the version number and lists the contributors to
    the simulation.
  </p>
</div>  

<h3 class="header">
  Right-Click Menu
</h3>
<p>
  In the main view, each shape has a right-click menu. This menu
  currently has three options:
</p>
<table>
  <tr>
    <td>
      Icon
    </td>
    <td>
      Name
    </td>
    <td>
      Function
    </td>
  </tr>
  <tr>
<!-- folder, dragIcon and delete -->
    <td>
      <a href="img/trackIcon.png" title="Click for full size.">
	<img src="img/trackIcon.png" alt="The tracking icon." />
      </a>
    </td>
    <td>
      Track
    </td>
    <td>
      Creates a window that tracks the given object. This window is
      another view into the same simulation. The window is described
      later on.
    </td>
  </tr>
  <tr>
    <td>
      <a href="img/folder.png" title="Click for full size.">
	<img src="img/folder.png" alt="The folder icon." />
      </a>
    </td>
    <td>
      Explore
    </td>
    <td>
      Opens up or brings up the explorer window and sets it to the
      right-clicked shape. The explorer window is described later on.
    </td>
  </tr>
  <tr>
    <td>
      <a href="img/delete.png" title="Click for full size.">
	<img src="img/delete.png" alt="The delete icon." />
      </a>
    </td>
    <td>
      Delete
    </td>
    <td>
      Deletes the right-clicked shape.
    </td>
  </tr>
</table>

<h3 class="header">
  Status Bar
</h3>
<p>
  At the bottom of the main window is a status bar. This is just used
  to passively display some simple information about the current state
  of the simulation. Currently, there are two pieces of information
  here: the number of shapes (<span class="uiElement">Shape
  count</span>) and whether the simulation is playing or not. All of
  this information updates live.
</p>
<h2 class="header">
  Control Panel
</h2>
<p>
  The panel located on the right side of the main window lets you
  control the entire simulation. At the very top there are three buttons:
</p>
<table>
  <tr>
    <td>
      Icon
    </td>
    <td>
      Name
    </td>
    <td>
      Function
    </td>
  </tr>
  <tr>
    <td>
      <a href="img/step.png" title="Click for full size.">
	<img src="img/step.png" alt="The step button." />
      </a>
    </td>
    <td>
      Step Forward
    </td>
    <td>
      Steps the simulation forward one step. The exact size of the
      step depends on the accuracy.
    </td>
  </tr>
  <tr>
    <td>
      <a href="img/play.png" title="Click for full size.">
	<img src="img/play.png" alt="The play button." />
      </a>
    </td>
    <td>
      Play
    </td>
    <td>
      Starts the simulation playing.
    </td>
  </tr>
  <tr>
    <td>
      <a href="img/stop.png" title="Click for full size.">
	<img src="img/stop.png" alt="The stop button." />
      </a>
    </td>
    <td>
      Pause
    </td>
    <td>
      Pauses the simulation.
    </td>
  </tr>
</table>
<p>
  Apart from these three buttons there are also two tabs.
</p>
<h3 class="header">
  Options Tab
</h3>
<p>
  The options tab lets you control several aspects of the
  simulation. There are three sliders:
</p>
<table>
  <tr>
    <td>
      Name
    </td>
    <td>
      Function
    </td>
  </tr>
  <tr>
    <td>
      Framerate
    </td>
    <td>
      This controls the <em>maximum</em> framerate of the
      simulation. This can be used to slow it down for closer
      inspection. If it takes more time to calculate the next step
      than is available at a given framerate setting, the simulation
      will animate more slowly.
    </td>
  </tr>
  <tr>
    <td>
      Accuracy
    </td>
    <td>
      Controls the accuracy of the simulation. The lower the accuracy,
      the more simulation time will pass between each frame.
    </td>
  </tr>
  <tr>
    <td>
      Gravity
    </td>
    <td>
      Controls the force of gravity in the simulation. If gravity is
      not zero, then it affects all objects in the simulation. Gravity
      can be set to a negative value; this will result in gravity
      pulling up rather than down. For some reason, this seems to
      amuse everyone.
    </td>
  </tr>
</table>

<h3 class="header">
  Camera Tab
</h3>
<p>
  The camera tab lets you control the main view of the
  simulation. There are two controls in this tab: a mini-map of the
  entire simulation and a zoom slider.
</p>
<p>
  The zoom slider works exactly as expected&mdash;you can use it to
  zoom the main view in and out to see more detail or more of the
  simulation. 
</p>
<p>
  The mini-map shows the entire simulation, with the part viewed in
  the main view highlighted in blue. The entire simulation is defined
  as either the area viewed in the main view or the area needed to
  contain all of the shapes in the simulation, whichever is
  bigger. This means that if you have no shapes outside the main view,
  the mini-map will just show that; however, if you have shapes that
  are outside of the main view, the mini-map will show a large enough
  area to see <em>all</em> of the shapes.
</p>
<p>
  The mini-map also allows panning. You can move the main view around
  by dragging the highlighted area around. However, this is not
  implemented very well and tends to break, especially if combined
  with zooming in and out. One day, when I am really bored, I will sit
  down and fix this, but it is not a very high priority immediately.
</p>

<h2 class="header">
  Shape Explorer
</h2>
<p>
  The shape explorer lets you go through the shapes in a convenient
  tree, delete them and edit their various propertied. It looks like this:
</p>
<div class="caption">
  <a href="img/explore.png" title="Click for full size.">
    <img src="img/explore.png" alt="The shape explorer window."  />
  </a>
  <p>
    This window lets you browse through all of the shapes and edit
    them.
  </p>
</div>
<p>
  On the lefthand side of the window, there is a tree view of all of
  the shapes. The shapes are divided by type; there are four types:
  circles, rectangles, polygons and walls. Each of the shapes has an
  icon that updates along with the simulation, so when the simulation
  is playing the icons change as their shapes rotate. 
</p>
<p>
  The tree view is used to select the shape to edit or view in the
  righthand side of the window. Additionally, the selected shape may
  be deleted using the delete key.
</p>
<p>
  On the righthand side, there are two tabs. The view tab follows the
  shape, drawing all of the shapes near it. All the other shapes are
  rendered in gray-scale; only the shape being tracked is in
  color. The edit tab lets you edit certain properties. You can edit
  the mass and density&mdash;these are linked, so when you change one,
  the other changes automatically. You can also edit the bounciness
  and friction, as well as the color of the shape. There are no real
  units here.
</p>
<p>
  The color picker that is used to select the new color for the shape
  was not written by me; it is a standard Java Swing component. I only
  changed the icon and the title.
</p>
<div class="caption">
  <a href="img/colorPicker.png" title="Click for full size.">
    <img src="img/colorPicker.png" alt="The color picker window." />
  </a>
  <p>
    This window is standard in Java Swing and was not written by me.
  </p>
</div>

<h2 class="header">
  Track Window
</h2>
<p>
  The track window follows a particular shape, giving a different view
  of the simulation.
</p>
<div class="caption">
  <a href="img/track.png" title="Click for full size">
    <img src="img/track.png" alt="The track window tracking a shape." />
  </a>
  <p>
    The view in this window follows the shape that's colored.
  </p>
</div>
<p>
  This is a special window, since you can create as many of them as
  you want. You can even have multiple track windows tracking
  the <em>same</em> shape!
</p>
<p>
  The shape that's being tracked is in color; all of the other shapes
  are completely desaturated. When the window is resized, instead of
  showing more or less of the simulation, it changes the scale. This
  means that changing the size of the window is really the same as
  zooming.
</p>
<p>
  The view of the simulation, following a shape, is actually exactly
  the same as found in the shape explorer. This shows how it is very
  easy to reuse code as long as the original architecture was sound.
</p>
<p>
  The title of the window reflects the type of shape it is following
  (again, the types are: circle, rectangle, polygon and wall). The
  little window icon, just like the tree icons in the shape explorer,
  updates along with the simulation, changing as the shape it's
  tracking rotates.
</p>
<h2 class="header">
  Defaults Editor
</h2>
<p>
  The defaults editor lets you change the properties of all newly
  created objects.
</p>
<div class="caption">
  <a href="img/defaults.png" title="Click for full size.">
    <img src="img/defaults.png" alt="The window for editing default
    values." />
  </a>
  <p>
    You can use this to change the properties of newly created shapes.
  </p>
</div>
<p>
  There are two tabs here. The first one lets you the default density,
  bounciness and friction of shapes. The density affects the mass of
  each shape; the bounciness and friction affect how much energy is
  preserved in each collision.
</p>
<p>
  The other tab lets you change the properties of the walls. Unlike
  the first tab, the changes in this one affect the walls
  immediately. The only thing you can change here is the colors of the
  walls. Additionally, the walls can be edited just like any other
  shape in the shape explorer.
</p>
<h2 class="header">
  Polygon Pad
</h2>
<p>
  This window lets you specify what shape the polygon tool adds to the
  simulation. 
</p>
<div class="caption">
  <a href="img/polygonPad.png" title="Click for full size.">
    <img src="img/polygonPad.png" alt="The Polygon Pad" />
  </a>
  <p>
    You can draw a polygon here by clicking on points you want!
  </p>
</div>
<p>
  The first time you use the polygon tool, this window will pop up. It
  can also be accessed from its toolbar button or
  the <span class="uiElement">tools</span> menu.
</p>
<p>
  This tool is used to draw polygons. You can draw a polygon by
  clicking wherever you want a vertex. The vertexes are connected in
  the order they are entered. The dotted line represents the edge that
  connects the last vertex added to the first one.
</p>
<p>
  If you create a polygon that you are not happy with, you can erase
  it by pressing <span class="uiElement">ESC</span>. Unfortunately,
  there is no way to move the points that are already there around
  once they are drawn.
</p>
<p>
  The polygon pad will be brought up by the polygon tool whenever no
  polygon is defined there. You need at least three points to define a
  polygon: if less than three points are entered, you will not be able
  to add any polygons and clicking the polygon tool will bring up the
  polygon pad window.
</p>
</div>

<?php
include "$homePath/footer.php";
?>

</body>

</html>
