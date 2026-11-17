# The look shared by every figure: colors, font, theme, and geom_moon().

# Colors. The gold and blue were checked with a color-vision simulator on
# the midnight surface: they stay apart under protanopia, deuteranopia and
# tritanopia, and each has at least 3:1 contrast with the background.
moon_pal <- list(
  surface   = "#0b1632",   # night sky, the background of figures and site
  ink       = "#eeeadb",   # titles, values
  ink2      = "#b4b9cc",   # axis titles, labels
  muted     = "#8089a6",   # tick labels, captions
  grid      = "#1b2747",   # gridlines
  axis      = "#33416b",   # zero line
  gold      = "#b88c27",   # the Wolves, and the real moon
  blue      = "#4b8de0",   # the league
  gray      = "#6b7594",   # context: fake moons, the other 29 teams
  band      = "#14204a",   # the full-moon window
  moon_lit  = "#eeeadb",
  moon_dark = "#243056",
  moon_rim  = "#4a5a8a"
)

# Archivo ships with the repo (site/fonts), so figures look the same on any
# machine. Without the systemfonts package the default sans is used.
moon_font <- function() {
  dir <- file.path("site", "fonts")
  if (!requireNamespace("systemfonts", quietly = TRUE) ||
      !file.exists(file.path(dir, "Archivo-Regular.ttf"))) return("sans")
  family <- "Archivo Moon"
  if (!family %in% systemfonts::registry_fonts()$family) {
    systemfonts::register_font(
      family,
      plain      = file.path(dir, "Archivo-Regular.ttf"),
      bold       = file.path(dir, "Archivo-SemiBold.ttf"),
      italic     = file.path(dir, "Archivo-Italic.ttf"),
      bolditalic = file.path(dir, "Archivo-Bold.ttf"))
  }
  family
}

theme_moon <- function(base_size = 13) {
  p <- moon_pal
  ggplot2::theme_minimal(base_size = base_size, base_family = moon_font()) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = p$surface, colour = NA),
      panel.background   = ggplot2::element_rect(fill = p$surface, colour = NA),
      text               = ggplot2::element_text(colour = p$ink2),
      plot.title         = ggplot2::element_text(colour = p$ink, face = "bold",
                                                 size = ggplot2::rel(1.25),
                                                 margin = ggplot2::margin(b = 4)),
      plot.subtitle      = ggplot2::element_text(colour = p$ink2, lineheight = 1.15,
                                                 margin = ggplot2::margin(b = 14)),
      plot.caption       = ggplot2::element_text(colour = p$muted, hjust = 0,
                                                 size = ggplot2::rel(0.8),
                                                 margin = ggplot2::margin(t = 12)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin        = ggplot2::margin(18, 22, 14, 18),
      panel.grid.major   = ggplot2::element_line(colour = p$grid, linewidth = 0.3),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(colour = p$muted),
      axis.title         = ggplot2::element_text(colour = p$ink2, size = ggplot2::rel(0.9)),
      axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y       = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      axis.ticks         = ggplot2::element_blank(),
      strip.text         = ggplot2::element_text(colour = p$ink, face = "bold", hjust = 0,
                                                 size = ggplot2::rel(0.95)),
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(colour = p$ink2),
      legend.margin      = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin  = ggplot2::margin(0, 0, 4, -6),
      legend.key         = ggplot2::element_rect(fill = NA, colour = NA)
    )
}

# ---- geom_moon ------------------------------------------------------------------
#
# Draws the moon as it looks at a given phase (0 new, 0.25 first quarter,
# 0.5 full, 0.75 last quarter), seen from the northern hemisphere: the lit
# side is on the right while waxing and on the left while waning.
#
#   aes(x, y, phase)   position and phase
#   size               diameter in millimetres, so the disc stays round
#                      whatever the axis scales are
GeomMoon <- ggplot2::ggproto(
  "GeomMoon", ggplot2::Geom,
  required_aes = c("x", "y", "phase"),
  default_aes  = ggplot2::aes(size = 6, alpha = 1),
  draw_key     = ggplot2::draw_key_point,
  draw_panel   = function(data, panel_params, coord) {
    pts <- coord$transform(data, panel_params)
    arc <- seq(-pi / 2, pi / 2, length.out = 48)          # bottom to top of the disc
    discs <- lapply(seq_len(nrow(pts)), function(i) {
      r    <- pts$size[i] / 2
      ph   <- pts$phase[i] %% 1
      side <- if (ph <= 0.5) 1 else -1                     # which limb is lit
      edge <- cos(2 * pi * ph)                             # terminator: +1 new, -1 full
      cx   <- grid::unit(pts$x[i], "npc")
      cy   <- grid::unit(pts$y[i], "npc")
      lit_x <- side * r * c(cos(arc), edge * cos(rev(arc)))
      lit_y <- r * c(sin(arc), sin(rev(arc)))
      grid::gList(
        grid::circleGrob(cx, cy, r = grid::unit(r, "mm"),
                         gp = grid::gpar(fill = moon_pal$moon_dark, col = moon_pal$moon_rim,
                                         lwd = 0.8, alpha = pts$alpha[i])),
        grid::polygonGrob(cx + grid::unit(lit_x, "mm"), cy + grid::unit(lit_y, "mm"),
                          gp = grid::gpar(fill = moon_pal$moon_lit, col = NA,
                                          alpha = pts$alpha[i])))
    })
    do.call(grid::grobTree, unlist(discs, recursive = FALSE))
  }
)

geom_moon <- function(mapping = NULL, data = NULL, ..., size = 6, inherit.aes = FALSE) {
  ggplot2::layer(geom = GeomMoon, mapping = mapping, data = data, stat = "identity",
                 position = "identity", inherit.aes = inherit.aes, show.legend = FALSE,
                 params = list(size = size, ...))
}

# Nine moons for an axis that runs from new moon through full and back to new,
# measured in days from the full moon.
moon_axis <- function(synodic_days, y) {
  phase <- seq(0, 1, by = 1 / 8)
  data.frame(x = (phase - 0.5) * synodic_days, y = y, phase = phase)
}

# Format a number with a real minus sign and an explicit plus. Zero gets no sign.
signed <- function(x, digits = 1) {
  s <- formatC(abs(x), format = "f", digits = digits)
  ifelse(as.numeric(s) == 0, s, ifelse(x < 0, paste0("\u2212", s), paste0("+", s)))
}

# Axis breaks at whole multiples of `step`, labelled with signs.
step_breaks <- function(step) function(limits) {
  seq(ceiling(limits[1] / step) * step, floor(limits[2] / step) * step, by = step)
}
tick_signed <- function(v) signed(v, digits = 0)

# A win-loss record with an en dash: 55\u201336.
record <- function(wins, losses) paste0(wins, "\u2013", losses)

save_figure <- function(plot, name, cfg, width = 10, height = 6) {
  dir.create(cfg$dir_figures, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(cfg$dir_figures, paste0(name, ".png"))
  ragg::agg_png(path, width = width, height = height, units = "in", res = 200,
                background = moon_pal$surface)
  print(plot)
  grDevices::dev.off()
  invisible(path)
}
