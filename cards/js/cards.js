// This file is just for the tab effect on the page; it is not part of
// the card game library!

$(function () {
  $("#games .controls ul li").click(function (e) {
    var tabId = $(this).attr("id").replace(/-control/, "");
    switchTab(tabId);
    
    return false;
  });
});

var noWar = true;
function switchTab(tabId) {
  var other = tabId == "Klondike" ? "War" : "Klondike";

  if (tabId == "War" && noWar) {
    noWar = false;
    War();
  }

  $("#" + other).hide();
  $("#" + tabId).show();

  $("#" + other + "-control").removeClass("active");
  $("#" + tabId + "-control").addClass("active");
}