Shiny.addCustomMessageHandler("open-url-new-tab", function(url) {
  window.open(url, "_blank");
});

Shiny.addCustomMessageHandler("open-url-here", function(url) {
  window.open(url, "_self");
});
