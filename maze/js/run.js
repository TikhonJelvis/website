jQuery(function ($) {
  generateMaze();
  generateStats(1000, 10, 10);

  $("#maze-recalculate").click(function () {
    generateStats($("#maze-samples").val(),
                  $("#maze-width").val(),
                  $("#maze-height").val());
  });
});

function generateMaze() {
  var maze     = new Maze(58, 58),
      canvas   = jQuery('<canvas id="maze" width="407" height="407">').appendTo('.maze'),
      controls = jQuery('<div>').appendTo('.maze'),
      solve    = jQuery('<input type="button" value="solve">').appendTo(controls),
      reset    = jQuery('<input type="button" value="reset">').appendTo(controls),
      context  = canvas[0].getContext('2d'),
      gradient = context.createLinearGradient(0, 0, 401, 401);

  gradient.addColorStop(0, "#000044");
  gradient.addColorStop(0.8, "#3366FF");

  maze.draw("maze", 7, {wall : gradient, background : "#FFBB88"});

  solve.click(function () { maze.drawSolution("maze"); });
  reset.click(function () {
    maze = new Maze(58, 58);
    maze.draw("maze", 7, {wall : gradient, background : "#FFBB88"});
  });
}

function generateStats(number, width, height) {
  var holder   = jQuery(".maze-graph").empty(),
      plotDiv  = jQuery("<div>").css("width", 500).css("height", 250),
      statsDiv = jQuery('<div class="maze-stats">');

  holder.append(plotDiv).append(statsDiv);

  var data     = getData(number, width, height, 1),
      plotData = [];

  for (var i in data.raw) {
    if (data.raw.hasOwnProperty(i)) plotData.push([i * 1, data.raw[i] * 1]);
  }

  var plot = jQuery.plot(plotDiv, [
    {
      data   : plotData,
      label  : "Solution length",
      lines  : { show : false },
      points : { show : true }
    },
    {
      data  : [[data.mean, 0], [data.mean, data.max]],
      label : "Mean"
    }
  ]);

  statsDiv.append("Mean: " + data.mean.toFixed(2) + "<br />");
  statsDiv.append("Standard deviation: " + data.stdDev.toFixed(2));
}