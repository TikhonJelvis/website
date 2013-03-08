/* Lets you draw stuff programatically! */
$(document).ready(function (e) {
  // Toggle the text when the link is clicked:
  $(".hide-control").click(function (e) {
    $("#help").toggle();
    $(".hide-control").html(/hide/.test($(".hide-control").html()) ? "show" : "hide");
  });

  function createOutput() {
    var width  = $("#width").val() * 1,
        height = $("#height").val() * 1,
        canvas = draw($("#code textarea").val(), width, height),
        cont   = $("<div>").addClass("output");

    cont.append("Click to close.")
        .append(canvas)
        .width(width)
        .height(height + 20)
        .click(function (e) {
          cont.remove();
        });

    return cont;
  }

  // Draw the picture when the button is clicked.
  $("#render").click(function (e) {
    $("body").append(createOutput());
  });

  $("#open").click(function (e) {
    var out = createOutput();
    
    open(out.find("canvas")[0].toDataURL());

    out.remove();
  });
});

/** Represents a color in the rgba space. The default alpha is 255. This
 *  forces its arguments to be in the interval [0, 256) by using abs and
 *  modulus.
 */
function Color(r, g, b, a) {
  if (typeof a == "undefined") {
    a = 255;
  }
  this.r = r = Math.abs(r) % 256;
  this.g = g = Math.abs(g) % 256;
  this.b = b = Math.abs(b) % 256;
  this.a = a = Math.abs(a) % 256;
  
  /** Returns an array containing the rgba values of the color. */
  this.toArray = function () {
    return [r, g, b, a];
  };

  /** Returns a string representation of the color in the form
   *  "rgba(r, g, b, a)".
   */
  this.toString = function () {
    return "rgba(" + r + ", " + g + ", " + b + ", " + a + ")";
  };
}

/** Returns a canvas with the given code drawn on it. Width and height
 *  are 400 by default. If the code is a function, that function will
 *  be called for each pixel; if code is a string it will be evalled
 *  with Math before being called.
 */
function draw(code, width, height) {
  width  = width  || 400;
  height = height || 400;
  
  // Some useful functions for dealing with colors:
  /** Returns the color which is the average of the two given colors,
   *  by averaging the corresponding channels of each color.
   */
  function avg(c1, c2) {
    return new Color((c1.r + c2.r) / 2, (c1.g + c2.g) / 2, (c1.b + c2.b) / 2);
  }

  /** Multiplies the r, g and b of a color by the given number. */
  function scale(color, ratio) {
    return new Color(color.r * ratio, color.g * ratio, color.b * ratio, color.a * ratio);
  }

  /** Returns the color that is proportionally between the two given colors */
  function gradient(start, end, ratio) {
    ratio = Math.abs(ratio) % 1;
    
    return new Color((end.r - start.r) * ratio + start.r,
                     (end.g - start.g) * ratio + start.g,
                     (end.b - start.b) * ratio + start.b);
  }
  
  var center = {
    x : width / 2,
    y : height / 2
  };
  
  if (typeof code == "string") {
    code = "with (Math) { func = " + code + "}";
    eval(code);
  } else {
    var func = code;
  }

  var out = $("<canvas>");
  out.attr({
    width : width,
    height : height
  });

  var context = out[0].getContext("2d"),
      data    = context.createImageData(width, height);
  
  /** Set the pixel at (x, y) to the given Color. */
  data.setPixel = function (x, y, color) {
    color = color.toArray();

    var index = (x + y * this.width) * 4;

    for (var i = 0; i < 4; i++) {
      this.data[index + i] = color[i];
    }
  };

  for (var x = 0; x < data.width; x++) {
    for (var y = 0; y < data.width; y++) {
      var xo = x - (data.width / 2), // x with respect to the origin
          yo = y - (data.height / 2), // y, same as xo
          d  = Math.sqrt(xo*xo + yo*yo),
          a  = Math.atan2(yo, xo);

      data.setPixel(x, y, func(xo, yo, d, a));
    }
  }

  context.putImageData(data, 0, 0);

  return out;
}