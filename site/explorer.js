// The interactive chart: one team's moon effect season by season, and all
// thirty teams side by side. The numbers come from assets/explorer-data.js,
// which R/site.R writes from the results. No library is needed.
//
// Both charts are drawn at the width of the page, one SVG unit to a pixel, so
// the text stays the same size on a phone and nothing scrolls sideways.
(function () {
  "use strict";
  var D = window.MOON_DATA;
  var root = document.getElementById("explorer");
  if (!D || !root) return;

  var SVG = "http://www.w3.org/2000/svg";
  var Z = 1.959964;                    // half-width of a 95% interval, in standard errors
  var ALL = D.seasons.length;          // position of the all-seasons value in each row
  var MINUS = "−", DASH = "–";
  var NARROW = 640;                    // below this width the charts use the compact layout
  // The labels below say "ten seasons" and "thirty teams". R/site.R stops the
  // build if the data ever holds other counts, so the words cannot go stale.
  var state = { team: D.teams.indexOf(D.focus), outcome: "net", measure: "window" };

  // ---- small helpers ---------------------------------------------------------
  function make(ns, name, attrs, text) {
    var node = ns ? document.createElementNS(ns, name) : document.createElement(name);
    for (var key in attrs || {}) node.setAttribute(key, attrs[key]);
    if (text !== undefined) node.textContent = text;
    return node;
  }
  function svg(name, attrs, text) { return make(SVG, name, attrs, text); }
  function html(name, attrs, text) { return make(null, name, attrs, text); }

  function signed(v, digits) {
    var s = Math.abs(v).toFixed(digits === undefined ? 1 : digits);
    if (Number(s) === 0) return s;
    return (v < 0 ? MINUS : "+") + s;
  }
  function value(team, i) {
    var est = D.est[state.measure][state.outcome][team][i];
    var se = D.se[state.measure][state.outcome][team][i];
    return { est: est, lo: est - Z * se, hi: est + Z * se };
  }
  function range(v) { return signed(v.lo) + " to " + signed(v.hi); }
  function record(team, i) { return D.record[team][i][0] + DASH + D.record[team][i][1]; }
  function isFocus(team) { return D.teams[team] === D.focus; }

  // Width of a piece of text as the chart will draw it. The chart has to be on
  // the page already for the browser to measure it.
  function textWidth(chart, cls, text) {
    var probe = svg("text", { "class": cls, x: 0, y: -100 }, text);
    chart.appendChild(probe);
    var width = probe.getComputedTextLength();
    chart.removeChild(probe);
    return width;
  }
  function widest(chart, cls, texts) {
    return texts.reduce(function (most, t) { return Math.max(most, textWidth(chart, cls, t)); }, 0);
  }

  // The axes stay put whatever the team, the outcome and the measure, so one
  // view can be compared with the next by eye. Each chart gets one range, wide
  // enough for every interval it will ever draw.
  function reach(from, to, step) {
    var most = 0;
    Object.keys(D.measures).forEach(function (m) {
      Object.keys(D.outcomes).forEach(function (o) {
        D.est[m][o].forEach(function (row, t) {
          for (var i = from; i < to; i++) most = Math.max(most, Math.abs(row[i]) + Z * D.se[m][o][t][i]);
        });
      });
    });
    return Math.ceil(most / step) * step;
  }
  var seasonReach = reach(0, ALL, 5), teamReach = reach(ALL, ALL + 1, 1);

  // The longest season records any team has, such as 10–1. Their width sets the
  // size of every record, so the text does not change size from team to team.
  var longRecords = (function () {
    var seen = {}, most = 0;
    D.teams.forEach(function (_, t) {
      for (var i = 0; i < ALL; i++) { var r = record(t, i); seen[r] = r.length; most = Math.max(most, r.length); }
    });
    return Object.keys(seen).filter(function (r) { return seen[r] === most; });
  })();

  // Gridlines at whole steps either side of zero, so zero always has a line.
  function ticks(top, step) {
    var out = [];
    for (var v = -Math.floor(top / step) * step; v <= top + 1e-9; v += step) out.push(v);
    return out;
  }

  var axisTitle = {
    net: "Change in margin, in points",
    offense: "Change in points scored",
    defense: "Change in points allowed"
  };

  // ---- controls ----------------------------------------------------------------
  root.textContent = "";
  var controls = html("form", { "class": "controls" });
  controls.addEventListener("submit", function (e) { e.preventDefault(); });

  var teamLabel = html("label", { "class": "choice" });
  teamLabel.appendChild(html("span", { "class": "legend" }, "Team"));
  var select = html("select", { id: "explorer-team" });
  D.teams.forEach(function (name, t) { select.appendChild(html("option", { value: t }, name)); });
  select.addEventListener("change", function () { state.team = Number(select.value); update(); });
  teamLabel.appendChild(select);
  controls.appendChild(teamLabel);

  function choices(key, legend, options) {
    var set = html("div", { "class": "choice", role: "radiogroup", "aria-label": legend });
    set.appendChild(html("span", { "class": "legend" }, legend));
    var box = html("div", { "class": "options" });
    Object.keys(options).forEach(function (id) {
      var label = html("label");
      var input = html("input", { type: "radio", name: "explorer-" + key, value: id });
      input.checked = state[key] === id;
      input.addEventListener("change", function () { state[key] = id; update(); });
      label.appendChild(input);
      label.appendChild(html("span", null, options[id]));
      box.appendChild(label);
    });
    set.appendChild(box);
    return set;
  }
  controls.appendChild(choices("outcome", "Outcome", D.outcomes));
  controls.appendChild(choices("measure", "Measured", D.measures));
  root.appendChild(controls);

  // The selected team's chart stays in view while the reader goes down the
  // list of thirty, so choosing a row shows its seasons straight away.
  var pinned = html("div", { "class": "pinned" });
  var seasonsTitle = html("h4", { "aria-live": "polite" });
  var seasonsBox = html("div", { "class": "chart" });
  pinned.appendChild(seasonsTitle);
  pinned.appendChild(seasonsBox);
  root.appendChild(pinned);
  var summary = html("p", { "class": "hint", "aria-live": "polite" });
  root.appendChild(summary);

  var teamsTitle = html("h4", null, "All thirty teams, ten seasons together");
  var teamsHint = html("p", { "class": "hint" }, "Select a row to see that team's seasons. ");
  [["focus", "Timberwolves"], ["picked", "Selected team"], ["other", "Other teams"]].forEach(function (k) {
    var item = html("span", { "class": "key" });
    item.appendChild(html("i", { "class": "swatch is-" + k[0] }));
    item.appendChild(document.createTextNode(k[1]));
    teamsHint.appendChild(item);
    teamsHint.appendChild(document.createTextNode(" "));
  });
  var teamsBox = html("div", { "class": "chart" });
  [teamsTitle, teamsHint, teamsBox].forEach(function (n) { root.appendChild(n); });

  var details = html("details");
  details.appendChild(html("summary", null, "Show the selected team's numbers as a table"));
  var tableBox = html("div", { "class": "scroll" });
  details.appendChild(tableBox);
  root.appendChild(details);

  // ---- tooltip -------------------------------------------------------------------
  // the table under the charts carries the same numbers for screen readers
  var tip = html("div", { "class": "tip", "aria-hidden": "true" });
  tip.hidden = true;
  root.appendChild(tip);
  function showTip(evt, lines) {
    tip.textContent = "";
    lines.forEach(function (line, i) { tip.appendChild(html(i ? "span" : "strong", null, line)); });
    tip.hidden = false;
    moveTip(evt);
  }
  function moveTip(evt) {
    var box = root.getBoundingClientRect();
    var x = evt.clientX - box.left + 14, y = evt.clientY - box.top + 16;
    x = Math.max(0, Math.min(x, box.width - tip.offsetWidth - 4));
    // above the pointer when the window has no room below it
    if (evt.clientY + 16 + tip.offsetHeight > window.innerHeight - 4) {
      y = evt.clientY - box.top - tip.offsetHeight - 12;
    }
    tip.style.left = x + "px";
    tip.style.top = y + "px";
  }
  function hideTip() { tip.hidden = true; }
  function hover(node, lines) {
    node.addEventListener("pointerenter", function (e) { showTip(e, lines()); });
    node.addEventListener("pointermove", moveTip);
    node.addEventListener("pointerleave", hideTip);
  }

  // ---- the heading above the first chart and the line of text under it ----------------
  function drawSummary(seasonNames) {
    var team = state.team, all = value(team, ALL);
    seasonsTitle.textContent = D.teams[team] + ", season by season";
    var text = "All ten seasons together: " + signed(all.est) + " points (" + range(all) + ").";
    if (state.measure === "window") {
      text += " Under each season is the record inside the window, " + record(team, ALL) + " in all.";
    }
    if (state.outcome === "defense") text += " Fewer points allowed is better defense.";
    if (seasonNames[0].length < 5) text += " " + seasonNames[0] + " is the " + D.seasons[0] + " season.";
    summary.textContent = "";
    summary.appendChild(html("i", { "class": "swatch is-band" }));
    summary.appendChild(document.createTextNode(text));
  }

  // ---- chart 1: the selected team, season by season --------------------------------
  function drawSeasons(W, narrow) {
    var withRecord = state.measure === "window";
    var m = { l: narrow ? 30 : 40, r: 6, t: 28, b: withRecord ? 50 : 30 };
    var ih = narrow ? 150 : 230, iw = W - m.l - m.r, H = m.t + ih + m.b;
    var top = seasonReach, step = top > 20 ? 10 : 5, slot = iw / ALL;
    var y = function (v) { return m.t + ih * (top - v) / (2 * top); };
    var x = function (i) { return m.l + slot * (i + 0.5); };
    var team = state.team, all = value(team, ALL);
    var kind = isFocus(team) ? " is-focus" : " is-picked";

    var chart = svg("svg", {
      viewBox: "0 0 " + W + " " + H, width: W, height: H, role: "img",
      "aria-label": D.teams[team] + ", " + D.outcomes[state.outcome].toLowerCase() + ", " +
        D.measures[state.measure].toLowerCase() + ", season by season. The table under the charts has the numbers."
    });
    seasonsBox.textContent = "";
    seasonsBox.appendChild(chart);

    chart.appendChild(svg("text", { "class": "axis-title", x: 0, y: 13 }, axisTitle[state.outcome]));
    ticks(top, step).forEach(function (v) {
      if (v !== 0) chart.appendChild(svg("line", { "class": "grid", x1: m.l, x2: m.l + iw, y1: y(v), y2: y(v) }));
      chart.appendChild(svg("text", { "class": "tick", x: m.l - 8, y: y(v) + 4, "text-anchor": "end" }, signed(v, 0)));
    });

    // all ten seasons together: a band for the interval and a dashed line for the
    // estimate, with the zero line drawn over the band so it never disappears
    chart.appendChild(svg("rect", { "class": "band", x: m.l, width: iw, y: y(all.hi), height: Math.max(y(all.lo) - y(all.hi), 1) }));
    chart.appendChild(svg("line", { "class": "zero", x1: m.l, x2: m.l + iw, y1: y(0), y2: y(0) }));
    chart.appendChild(svg("line", { "class": "pooled", x1: m.l, x2: m.l + iw, y1: y(all.est), y2: y(all.est) }));

    // season names in the longest form that fits: 2016-17, then 16-17, then ’17
    var forms = [
      function (s) { return s; },
      function (s) { return s.slice(2); },
      function (s) { return "’" + s.slice(-2); }
    ];
    var form = forms[forms.length - 1];
    for (var f = 0; f < forms.length; f++) {
      if (widest(chart, "tick", D.seasons.map(forms[f])) + 8 <= slot) { form = forms[f]; break; }
    }

    // on the narrowest phones the records are set a little smaller, so that
    // neighbors such as 7–5 and 0–10 keep a gap between them
    var recordStyle = "";
    if (withRecord) {
      var need = widest(chart, "tick record", longRecords);
      if (need + 6 > slot) recordStyle = "font-size:" + Math.max(9, (narrow ? 12 : 13) * (slot - 6) / need).toFixed(1) + "px";
    }

    D.seasons.forEach(function (season, i) {
      var s = value(team, i);
      var group = svg("g", { "class": "season" + kind });
      group.appendChild(svg("line", { "class": "interval", x1: x(i), x2: x(i), y1: y(s.hi), y2: y(s.lo) }));
      group.appendChild(svg("circle", { "class": "dot", cx: x(i), cy: y(s.est), r: 6 }));
      group.appendChild(svg("text", { "class": "tick", x: x(i), y: m.t + ih + 20, "text-anchor": "middle" }, form(season)));
      if (withRecord) {
        group.appendChild(svg("text", { "class": "tick record", x: x(i), y: m.t + ih + 39, "text-anchor": "middle", style: recordStyle }, record(team, i)));
      }
      var hit = svg("rect", { "class": "hit", x: x(i) - slot / 2, width: slot, y: m.t, height: ih + m.b });
      hover(hit, function () {
        var lines = [D.teams[team] + ", " + season, signed(s.est) + " points (" + range(s) + ")"];
        if (withRecord) lines.push("Record inside the window: " + record(team, i));
        return lines;
      });
      group.appendChild(hit);
      chart.appendChild(group);
    });
    return D.seasons.map(form);
  }

  // ---- chart 2: every team, all seasons together -------------------------------------
  function drawTeams(W, narrow) {
    var est = D.est[state.measure][state.outcome];
    var order = D.teams.map(function (_, t) { return t; }).sort(function (a, b) { return est[b][ALL] - est[a][ALL]; });
    var row = narrow ? 24 : 22;

    var chart = svg("svg", {
      role: "img",
      "aria-label": "All thirty teams ranked by " + D.outcomes[state.outcome].toLowerCase() + ", " +
        D.measures[state.measure].toLowerCase() + ", over ten seasons. " + D.teams[state.team] + " is selected."
    });
    teamsBox.textContent = "";
    teamsBox.appendChild(chart);

    // nicknames on a narrow screen, to leave the plot some room; the name
    // column is as wide as the longest name set in bold
    var label = narrow ? D.short : D.teams;
    var names = Math.ceil(widest(chart, "name strong", label));
    var m = { l: names + 14, r: narrow ? 44 : 54, t: 6, b: 46 };
    var iw = W - m.l - m.r, ih = row * order.length, H = m.t + ih + m.b;
    var top = teamReach, step = top > 6 ? 2 : 1;
    while (step < top && iw * step / (2 * top) < 30) step *= 2;       // keep the tick labels apart
    var x = function (v) { return m.l + iw * (v + top) / (2 * top); };
    chart.setAttribute("viewBox", "0 0 " + W + " " + H);
    chart.setAttribute("width", W);
    chart.setAttribute("height", H);

    ticks(top, step).forEach(function (v) {
      chart.appendChild(svg("line", { "class": v === 0 ? "zero" : "grid", x1: x(v), x2: x(v), y1: m.t, y2: m.t + ih }));
      chart.appendChild(svg("text", { "class": "tick", x: x(v), y: m.t + ih + 18, "text-anchor": "middle" }, signed(v, 0)));
    });
    chart.appendChild(svg("text", { "class": "axis-title", x: m.l + iw / 2, y: H - 8, "text-anchor": "middle" }, axisTitle[state.outcome]));

    order.forEach(function (team, k) {
      var s = value(team, ALL), cy = m.t + row * (k + 0.5);
      var cls = "team" + (isFocus(team) ? " is-focus" : "") + (team === state.team ? " is-picked" : "");
      var group = svg("g", { "class": cls });
      group.appendChild(svg("rect", { "class": "row", x: 0, width: W, y: cy - row / 2, height: row }));
      group.appendChild(svg("text", { "class": "name", x: m.l - 14, y: cy + 4, "text-anchor": "end" }, label[team]));
      group.appendChild(svg("line", { "class": "interval", x1: x(s.lo), x2: x(s.hi), y1: cy, y2: cy }));
      group.appendChild(svg("circle", { "class": "dot", cx: x(s.est), cy: cy, r: 5 }));
      group.appendChild(svg("text", { "class": "value", x: W - 4, y: cy + 4, "text-anchor": "end" }, signed(s.est)));
      hover(group, function () {
        var lines = [D.teams[team], signed(s.est) + " points (" + range(s) + ")"];
        if (state.measure === "window") lines.push("Record inside the window: " + record(team, ALL));
        return lines;
      });
      group.addEventListener("click", function () { state.team = team; update(); });
      chart.appendChild(group);
    });
  }

  // ---- the same numbers as a table -------------------------------------------------------
  function drawTable() {
    var team = state.team, withRecord = state.measure === "window";
    var table = html("table"), head = html("tr");
    ["Season", "Estimate", "95% interval"].concat(withRecord ? ["Record in the window"] : []).forEach(function (h) {
      head.appendChild(html("th", { scope: "col" }, h));
    });
    table.appendChild(head);
    for (var i = 0; i <= ALL; i++) {
      var s = value(team, i), tr = html("tr");
      tr.appendChild(html("td", null, i < ALL ? D.seasons[i] : "All ten seasons"));
      tr.appendChild(html("td", null, signed(s.est)));
      tr.appendChild(html("td", null, range(s)));
      if (withRecord) tr.appendChild(html("td", null, record(team, i)));
      table.appendChild(tr);
    }
    tableBox.textContent = "";
    tableBox.appendChild(table);
  }

  // ---- draw everything, and again when the page changes width ------------------------------
  var drawnAt = 0;
  function update() {
    hideTip();
    var W = Math.max(280, Math.floor(root.clientWidth));
    var narrow = W < NARROW;
    drawnAt = W;
    root.classList.toggle("is-narrow", narrow);
    select.value = String(state.team);
    drawSummary(drawSeasons(W, narrow));
    drawTeams(W, narrow);
    drawTable();
  }
  function resized() {
    if (Math.max(280, Math.floor(root.clientWidth)) !== drawnAt) update();
  }

  root.classList.add("is-live");
  update();
  if (window.ResizeObserver) new ResizeObserver(resized).observe(root);
  else window.addEventListener("resize", resized);
  // the labels are measured, so measure them again once the page's own font has loaded
  if (document.fonts && document.fonts.ready) document.fonts.ready.then(update);
})();
