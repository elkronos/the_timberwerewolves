// On a narrow screen the figures, code blocks and tables scroll sideways.
// While one of them overflows it gets a tab stop, so the keyboard can reach
// it and the arrow keys can scroll it.
(function () {
  function update() {
    var regions = document.querySelectorAll("figure, pre, .scroll");
    Array.prototype.forEach.call(regions, function (el) {
      if (el.scrollWidth > el.clientWidth + 1) el.setAttribute("tabindex", "0");
      else el.removeAttribute("tabindex");
    });
  }
  window.addEventListener("resize", update);
  window.addEventListener("load", update);
  // a table inside a closed <details> has no width until it is opened
  document.addEventListener("toggle", update, true);
  update();
})();
