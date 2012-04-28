$.noConflict();
jQuery(document).ready(function ($) {
  // Toggle the text when the link is clicked:
  $(".hideable").each(function (index, element) {
    var buttons = $(element).find(".hide-control");
    buttons.click(function (e) {
      if (/hide/.test(buttons.html())) {
        $(element).find(".hide").hide();
        buttons.html("show");
      } else {
        $(element).find(".hide").show();
        buttons.html("hide");
      }
      
      return false;
    });
  });
});