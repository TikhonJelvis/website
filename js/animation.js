/** A really simple utility for creating animations with buttons to
 *  play/pause and step through them. I figure it will be useful for
 *  illustrating algorithms and the like.
 *
 *  All the frames of the animation should have the 'animation-frame'
 *  class and probably be positioned on top of each other. Only one
 *  will be shown at a time; it's probably good for most of the to
 *  start with `display : none'.
 *
 *  The config parameter can specify whether to start playing by
 *  default and the speed to switch frames:
 *
 *  config : { playing : bool, speed : int }
 */
function animate(content, config) {
  var $ = jQuery;

  // config paramters:
  config = config || {};
  var playing = config.playing || false,
      speed   = config.speed   || 420;

  var container = $(content),
      frames    = container.find(".animation li"),
      currFrame = 0,
      interval  = null;

  frames.hide();
  frames.first().show();

  // Control widgets: step, play/pause... etc
  var controls = $("<div class='animation-controls'>"),
      back     = $("<button class='step-animation'>⇤</button>"),
      forward  = $("<button class='step-animation'>⇥</button>"),
      play     = $("<button class='play-animation'>▶</button>");

  controls.append(back).append(play).append(forward);

  container.append(controls);

  back.click(stepBackward);
  play.click(togglePlaying);
  forward.click(stepForward);
   
  function stepForward() {
    $(frames[currFrame]).hide();
    
    currFrame = (currFrame + 1) % frames.size();
    $(frames[currFrame]).show();  
  }

  function stepBackward() {
    $(frames[currFrame]).hide();
    
    currFrame = currFrame - 1;
    currFrame = currFrame < 0 ? frames.size() - 1 : currFrame;
    $(frames[currFrame]).show();
  }

  function togglePlaying() {
    if (playing) {
      play.text("▶");
      clearInterval(interval);
    } else {
      play.text("❚❚");
      interval = setInterval(stepForward, speed);
    }

    playing = !playing;
  }  
}
