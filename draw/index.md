---
title: Color Grapher
author: Tikhon Jelvis
---

<div id="container" class="content">

# Color Grapher

<div id="help">

<span class="hide">[<a href="#" class="hidehelp"> hide </a>]</span>

## Instructions

To draw a picture, write a javascript function in the text box below. This function will be called for every pixel and will be given four arguments: `x`, `y`, `d` and `a`. The first two correspond to the Cartesian coordinates of the point; the latter two are the polar coordinates with `d` being the distance to the center and `a` being the angle.

The origin is the center of the drawing area; the angle is in radians, going counter-clockwise from the x-axis. Basically, this works exactly as it would in a math class.

In addition to the pixel's position, the function will have `height` and `width` accessible. These correspond to the height and width of the drawing area; if the area is resized, they will change.

The function should return a `Color`. This can be done by calling `return new Color(r, g, b);` at the very end of the function where `r`, `g` and `b` correspond to the red, green and blue channels of the color respectively. 

For convenience, all of the properties of the `Math` class are available without needing to reference `Math`. That is, instead of calling `Math.cos`, you can just use `cos`.

Here is a simple example of a function that can be used here, which creates an interesting pattern: 

```javascript
function (x, y, d, a) {
  var r = cos(round(x / 10, 0)) * 256;
  var g = sin(round(y / 10, 0)) * 256;
  var b = random() * d;

  return new Color(r, g, b);
}
```

</div>

[<a href="#" class="hidehelp"> hide </a>]

This only works on browsers supporting pixel manipulation using canvas.

$controls$

</div>