// The moon in the corner follows the reader through one lunar month.
// Each section names the phase it starts at in its data-phase attribute:
// new at the top of the page, full at "The result", new again at the end.
// The same reading of the page marks the current section in the list of
// contents and turns the "Contents" button on once that list has gone by.
(function () {
  var lit = document.getElementById("dial-lit");
  var pillLit = document.getElementById("pill-lit");
  var pill = document.getElementById("pill");
  var contents = document.getElementById("contents");
  var sections = Array.prototype.slice.call(document.querySelectorAll("main > section"));
  if (!lit || !sections.length) return;

  var SIZE = 56, R = SIZE / 2;

  // Same geometry as geom_moon() and moon_svg() in the R code.
  function path(phase) {
    var ph = ((phase % 1) + 1) % 1;
    var edge = Math.cos(2 * Math.PI * ph);
    var waxing = ph <= 0.5;
    var limb = "A " + R + " " + R + " 0 0 " + (waxing ? 1 : 0) + " " + R + " " + SIZE;
    var sweep = waxing ? (edge < 0 ? 1 : 0) : (edge > 0 ? 1 : 0);
    var term = "A " + (Math.abs(edge) * R).toFixed(2) + " " + R + " 0 0 " + sweep + " " + R + " 0";
    return "M " + R + " 0 " + limb + " " + term + " Z";
  }

  var starts = sections.map(function (s) { return parseFloat(s.getAttribute("data-phase")); });
  var links = sections.map(function (s) {
    return contents ? contents.querySelector('a[href="#' + s.id + '"]') : null;
  });
  var LOOK = 0.35;     // the dial reads the page this far down the window

  // Which section the reader is in, and how far through the month that is.
  function place(y) {
    var tops = sections.map(function (s) { return s.offsetTop; });
    // the largest y the page can reach, so the month ends as the page does
    var end = document.documentElement.scrollHeight - window.innerHeight * (1 - LOOK);
    if (y <= tops[0]) return { section: 0, phase: starts[0] };
    for (var i = 0; i < tops.length; i++) {
      var last = i + 1 === tops.length;
      var next = last ? end : tops[i + 1], to = last ? 1 : starts[i + 1];
      if (y < next) {
        return { section: i, phase: starts[i] + (to - starts[i]) * (y - tops[i]) / Math.max(next - tops[i], 1) };
      }
    }
    return { section: tops.length - 1, phase: 1 };
  }

  var current = -1;
  var waiting = false;
  function update() {
    waiting = false;
    var at = place(window.scrollY + window.innerHeight * LOOK);
    var d = path(at.phase);
    lit.setAttribute("d", d);
    if (pillLit) pillLit.setAttribute("d", d);
    if (at.section !== current) {
      if (links[current]) links[current].removeAttribute("aria-current");
      if (links[at.section]) links[at.section].setAttribute("aria-current", "true");
      current = at.section;
    }
    // the button appears once the list of contents has scrolled out of sight
    if (pill && contents) {
      pill.classList.toggle("is-on", contents.getBoundingClientRect().bottom < 0);
    }
  }
  function request() {
    if (!waiting) { waiting = true; window.requestAnimationFrame(update); }
  }

  window.addEventListener("scroll", request, { passive: true });
  window.addEventListener("resize", request);
  window.addEventListener("load", request);
  update();
})();
