# The twelve figures on the site. Each function takes data or result tables
# and returns a ggplot; make_figures() saves them all as PNGs.
#
# Each figure has two layouts. The wide one is for a computer screen. With
# narrow = TRUE the same figure is laid out for a phone: half the width at the
# same type size, with wrapped titles and labels moved to where they fit.

MOON_LABELS <- c(
  moon_cos  = "New moon to full moon",
  moon_sin  = "First quarter to last quarter",
  near_full = "Outside to inside the full-moon window",
  moon_dist = "Moon at its nearest to its farthest")

TERM_LABELS <- c(
  league  = "League: points per team, all 30 teams",
  offense = "Wolves: points scored, beyond the league effect",
  defense = "Wolves: points allowed, beyond the league effect (lower is better defense)",
  net     = "Wolves: margin (scored minus allowed)")

NUMBER_WORDS <- c("none", "one", "two", "three", "four", "five", "six", "seven",
                  "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                  "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
number_word <- function(n) if (n <= 20) NUMBER_WORDS[n + 1] else as.character(n)

# "Minnesota Timberwolves" becomes "Timberwolves". A team's nickname is its
# last word, except Portland's.
nickname <- function(team) {
  ifelse(grepl("Trail Blazers$", team), "Trail Blazers", sub("^.* ", "", team))
}

days_axis <- function(narrow = FALSE) ggplot2::scale_x_continuous(
  breaks = c(-14, -7, 0, 7, 14), limits = c(-15.8, 15.8),
  labels = if (narrow) c("14 days\nbefore", "7 before", "Full moon", "7 after", "14 days\nafter")
           else c("14 days before", "7 before", "Full moon", "7 after", "14 days after"),
  expand = ggplot2::expansion(mult = 0))

# ---- 1. The Wolves' games by days from a full moon ----------------------------------
fig_games <- function(w, counts, cfg, narrow = FALSE) {
  p <- moon_pal
  d <- data.frame(day = round(w$days_from_full), win = w$win)
  d <- d[order(d$day, -d$win), ]
  d$stack  <- stats::ave(rep(1, nrow(d)), d$day, d$win, FUN = cumsum)
  d$y      <- ifelse(d$win == 1, d$stack, -d$stack)
  d$result <- factor(ifelse(d$win == 1, "Win", "Loss"), levels = c("Win", "Loss"))
  top      <- max(d$stack) + 4.5

  ggplot2::ggplot(d, ggplot2::aes(day, y)) +
    ggplot2::annotate("rect", xmin = -cfg$near_full_days, xmax = cfg$near_full_days,
                      ymin = -Inf, ymax = top - 2.6, fill = p$band) +
    ggplot2::geom_hline(yintercept = 0, colour = p$axis, linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(fill = result), shape = 21, colour = p$surface,
                        stroke = if (narrow) 0.35 else 0.5, size = if (narrow) 2.3 else 3) +
    # the record inside the window, printed in the window, under its losses
    ggplot2::annotate("text", x = 0, y = -max(d$stack) + 1.2, colour = p$ink,
                      size = if (narrow) 3.9 else 4.4, fontface = "bold", family = moon_font(),
                      label = record(counts$near_wins, counts$near_losses)) +
    geom_moon(ggplot2::aes(x, y, phase = phase),
              data = moon_axis(cfg$synodic_days, top), size = if (narrow) 5.5 else 8) +
    ggplot2::scale_fill_manual(values = c(Win = p$gold, Loss = p$gray)) +
    days_axis(narrow) +
    ggplot2::scale_y_continuous(breaks = seq(-20, 20, 10), labels = abs,
                                limits = c(-max(d$stack) - 1, top + 1.6)) +
    ggplot2::labs(
      title = fit_title(sprintf("%d Timberwolves games by days from a full moon", nrow(w)), narrow),
      subtitle = fit_text(sprintf(paste0(
        "Each dot is one game from %s to %s, a win above the line or a loss below it.\n",
        "The Wolves are %s inside the shaded full-moon window and %s outside it."),
        season_label(min(cfg$seasons)), season_label(max(cfg$seasons)),
        record(counts$near_wins, counts$near_losses),
        record(counts$far_wins, counts$far_losses)), narrow),
      x = NULL, y = "Games") +
    theme_moon(narrow = narrow) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# ---- 2. How the moon becomes numbers ---------------------------------------------------
fig_moon_variables <- function(cfg, narrow = FALSE) {
  p <- moon_pal
  days <- seq(-cfg$synodic_days / 2, cfg$synodic_days / 2, length.out = 400)
  d <- rbind(
    data.frame(days = days, value = cos(2 * pi * days / cfg$synodic_days),
               variable = "moon_cos: how full the moon is"),
    data.frame(days = days, value = sin(2 * pi * days / cfg$synodic_days),
               variable = "moon_sin: before or after the full moon"))
  colours <- stats::setNames(c(p$gold, p$blue), unique(d$variable))

  ggplot2::ggplot(d, ggplot2::aes(days, value)) +
    ggplot2::annotate("rect", xmin = -cfg$near_full_days, xmax = cfg$near_full_days,
                      ymin = -1.12, ymax = 1.12, fill = p$band) +
    ggplot2::annotate("text", x = 0, y = -1.04, label = "near_full\nwindow",
                      colour = p$ink2, size = 3.6, lineheight = 0.95, vjust = 0,
                      family = moon_font()) +
    ggplot2::geom_hline(yintercept = 0, colour = p$axis, linewidth = 0.4) +
    ggplot2::geom_line(ggplot2::aes(colour = variable), linewidth = 0.9) +
    geom_moon(ggplot2::aes(x, y, phase = phase),
              data = moon_axis(cfg$synodic_days, 1.4), size = if (narrow) 5.5 else 8) +
    ggplot2::scale_colour_manual(values = colours) +
    days_axis(narrow) +
    ggplot2::scale_y_continuous(breaks = c(-1, 0, 1), labels = tick_signed,
                                limits = c(-1.12, 1.55)) +
    ggplot2::labs(
      title = fit_title("One lunar month as three variables", narrow),
      subtitle = fit_text(sprintf(paste0(
        "moon_cos peaks at the full moon. moon_sin is negative while the moon waxes and positive while\n",
        "it wanes. near_full is 1 inside the shaded window, %s nights wide, and 0 outside it."),
        number_word(round(2 * cfg$near_full_days))), narrow),
      x = NULL, y = "Value of the variable") +
    theme_moon(narrow = narrow) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   # two long legend entries sit side by side on the wide figure alone
                   legend.direction = if (narrow) "vertical" else "horizontal")
}

# ---- 3. The Wolves by named phase ----------------------------------------------------------
# The wide layout runs the eight phases left to right. The narrow one turns
# the chart on its side, so each phase gets a row and its name fits beside it.
fig_phases <- function(phase_wolves, narrow = FALSE) {
  p <- moon_pal
  d <- phase_wolves
  d$x     <- as.integer(d$phase)
  d$moon  <- (d$x - 1) / 8
  d$label <- if (narrow) sprintf("%s\n%d games", d$phase, d$n)
             else sprintf("%s\n%d games", gsub(" ", "\n", d$phase), d$n)
  low     <- min(d$lo) - 1.5
  misses  <- sum(d$lo > 0 | d$hi < 0)
  verdict <- if (misses == 0) "All eight include zero."
             else sprintf("%s of the eight miss zero.", tools::toTitleCase(number_word(misses)))
  phases  <- if (narrow) ggplot2::scale_x_reverse(breaks = d$x, labels = d$label,
                                                  expand = ggplot2::expansion(add = 0.6))
             else ggplot2::scale_x_continuous(breaks = d$x, labels = d$label,
                                              expand = ggplot2::expansion(add = 0.6))

  plot <- ggplot2::ggplot(d, ggplot2::aes(x, mean)) +
    ggplot2::geom_hline(yintercept = 0, colour = p$axis, linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lo, ymax = hi), colour = p$gold,
                            linewidth = 0.7) +
    ggplot2::geom_point(colour = p$gold, size = 3.4) +
    ggplot2::geom_text(ggplot2::aes(label = signed(mean)),
                       data = d[d$phase %in% c("New", "Full"), ],
                       nudge_x = if (narrow) 0.38 else 0.3, colour = p$ink, size = 4,
                       family = moon_font()) +
    geom_moon(ggplot2::aes(x, y, phase = moon), data = transform(d, y = low),
              size = if (narrow) 7 else 9) +
    phases +
    ggplot2::scale_y_continuous(breaks = step_breaks(2), labels = tick_signed) +
    ggplot2::labs(
      title = fit_title("The Wolves against expectation, phase by phase", narrow),
      subtitle = fit_text(paste0(
        "The Wolves' average margin in each phase, minus the margin I predicted from opponent and schedule.\n",
        "The lines are 95% intervals. ", verdict), narrow),
      x = NULL, y = "Points above or below my prediction") +
    theme_moon(narrow = narrow)
  if (!narrow) return(plot + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()))
  plot + ggplot2::coord_flip() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = p$ink2, lineheight = 0.95))
}

# ---- Shared layout for the dot-and-interval figures --------------------------------------
#
# Rows are grouped under headings. `d` needs columns group, label, est, lo, hi
# and colour (the mark's color), plus an optional `note` printed at the right end of the interval.
# Rows are drawn top to bottom in the order given.
#
# Row labels sit beside their rows. On the narrow layout a figure with long
# labels asks for them `above` each row instead, at the left edge of the panel.
interval_plot <- function(d, x_title, step = 2, narrow = FALSE, above = FALSE) {
  p <- moon_pal
  above  <- narrow && above
  pitch  <- if (above) 2 else 1                   # distance from one row to the next
  groups <- unique(d$group)
  heads  <- data.frame(group = groups, y = NA_real_)
  y <- numeric(nrow(d)); at <- 0
  for (k in seq_along(groups)) {
    if (nzchar(groups[k])) {
      # a heading that wraps onto a second line takes that much more room
      more <- 0.9 * (lengths(strsplit(groups[k], "\n", fixed = TRUE)) - 1)
      heads$y[k] <- at - more / 2
      at <- at - 1 - more - (if (above) 0.75 else 0)
    }
    rows <- which(d$group == groups[k])
    y[rows] <- at - (seq_along(rows) - 1) * pitch
    at <- at - length(rows) * pitch - 0.7
  }
  d$y  <- y
  span <- range(c(d$lo, d$hi, 0))
  left <- span[1] - diff(span) * 0.02
  if (is.null(d$note)) d$note <- NA_character_
  headroom <- if (above && !nzchar(groups[1])) 1.5 else 0.6

  plot <- ggplot2::ggplot(d, ggplot2::aes(est, y)) +
    ggplot2::geom_vline(xintercept = 0, colour = p$axis, linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lo, xmax = hi, colour = colour),
                            linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(colour = colour), size = 3.2) +
    ggplot2::geom_text(ggplot2::aes(x = left, y = y, label = group),
                       data = heads[nzchar(heads$group), ],
                       hjust = 0, colour = p$ink, fontface = "bold", size = 3.9,
                       lineheight = 0.95, family = moon_font()) +
    ggplot2::geom_text(ggplot2::aes(x = hi, label = note), data = d[!is.na(d$note), ],
                       hjust = -0.12, colour = p$ink, size = 3.6, family = moon_font()) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_y_continuous(breaks = d$y, labels = if (above) NULL else d$label,
                                expand = ggplot2::expansion(add = c(0.8, headroom))) +
    ggplot2::scale_x_continuous(breaks = step_breaks(step), labels = tick_signed) +
    ggplot2::labs(x = fit_text(x_title, narrow, width = if (above) 56 else 40, balance = TRUE),
                  y = NULL) +
    theme_moon(narrow = narrow) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = p$ink2))
  if (!above) return(plot)
  plot + ggplot2::geom_text(ggplot2::aes(x = left, y = y + 0.75, label = label), hjust = 0,
                            colour = p$ink2, size = 3.7, family = moon_font())
}

# ---- 4. The sixteen estimates ------------------------------------------------------------------
fig_estimates <- function(terms, narrow = FALSE) {
  heads <- TERM_LABELS
  # the longest heading takes two lines on the narrow layout
  if (narrow) heads["defense"] <- sub(" (", "\n(", heads["defense"], fixed = TRUE)
  d <- data.frame(
    group  = unname(heads[terms$term]), label = unname(MOON_LABELS[terms$moon]),
    est = terms$swing, lo = terms$swing_lo, hi = terms$swing_hi,
    colour = ifelse(terms$term == "league", moon_pal$blue, moon_pal$gold),
    note = ifelse(terms$p < 0.05, sprintf("p = %.3f", terms$p), NA),
    stringsAsFactors = FALSE)
  d <- d[order(match(d$group, heads)), ]
  flagged <- sum(terms$p < 0.05)
  interval_plot(d, "Change in points from the low end of each moon variable to the high end",
                narrow = narrow, above = TRUE) +
    ggplot2::labs(
      title = fit_title(sprintf("%s estimates, %s with p below 0.05",
                                tools::toTitleCase(number_word(nrow(terms))), number_word(flagged)),
                        narrow),
      subtitle = fit_text(sprintf(paste0(
        "Each row is one moon variable's effect with its 95%% interval. Blue rows are the whole league, and gold\n",
        "rows are the Wolves' difference from the league. I printed p-values under 0.05, before correcting for %d tests."),
        nrow(terms)), narrow))
}

# ---- 5. The real full-moon record among fake ones --------------------------------------------------
fig_fake_records <- function(fake_records, counts, narrow = FALSE) {
  p <- moon_pal
  real  <- counts$near_wins / (counts$near_wins + counts$near_losses)
  best  <- fake_records[which.max(fake_records$win_rate), ]
  share <- mean(fake_records$win_rate >= real)
  h     <- graphics::hist(fake_records$win_rate, breaks = seq(0.30, 0.70, by = 0.02), plot = FALSE)
  bars  <- data.frame(x = h$mids, n = h$counts)
  top   <- max(bars$n)
  # The wide figure has room for both notes to the right of their marks. On
  # the narrow one they read toward the left, over the low bars of the tail.
  moon_y <- top * (if (narrow) 1.17 else 1.12)
  best_y <- top * (if (narrow) 0.39 else 0.22)
  best_label <- if (narrow) "Best fake window:\n%s on a cycle\nof %.0f days"
                else "Best fake window: %s\non a cycle of %.0f days"

  ggplot2::ggplot(bars, ggplot2::aes(x, n)) +
    ggplot2::geom_col(fill = p$gray, width = 0.011) +
    ggplot2::annotate("segment", x = real, xend = real, y = 0, yend = moon_y - top * 0.09,
                      colour = p$gold, linewidth = 0.9) +
    geom_moon(ggplot2::aes(x, y, phase = phase),
              data = data.frame(x = real, y = moon_y, phase = 0.5), size = if (narrow) 7 else 9) +
    ggplot2::annotate("text", x = real + (if (narrow) -0.022 else 0.014), y = moon_y,
                      hjust = if (narrow) 1 else 0, colour = p$ink,
                      size = 4, family = moon_font(), lineheight = 0.95,
                      label = sprintf("The real full moon\n%s", record(counts$near_wins, counts$near_losses))) +
    ggplot2::annotate("segment", x = best$win_rate, xend = best$win_rate, y = top * 0.035,
                      yend = best_y - top * 0.02, colour = p$muted, linewidth = 0.3) +
    ggplot2::annotate("text", x = if (narrow) 0.716 else best$win_rate, y = best_y,
                      hjust = if (narrow) 1 else 0.5, vjust = 0,
                      colour = p$ink2, size = 3.6, family = moon_font(), lineheight = 0.95,
                      label = sprintf(best_label, record(best$wins, best$games - best$wins),
                                      best$period)) +
    ggplot2::scale_x_continuous(labels = function(v) paste0(round(v * 100), "%"),
                                breaks = seq(0.3, 0.7, 0.1), limits = c(0.3, 0.72)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.02)),
                                limits = c(0, top * (if (narrow) 1.32 else 1.25))) +
    ggplot2::labs(
      title = fit_title(sprintf("%d fake full-moon windows, and the real one", nrow(fake_records)),
                        narrow),
      subtitle = fit_text(sprintf(paste0(
        "The Wolves' win rate inside each fake full-moon window. In %.1f%% of them\n",
        "the Wolves did as well as their real %s, or better."), 100 * share,
        record(counts$near_wins, counts$near_losses)), narrow),
      x = "Wolves win rate inside the window", y = "Fake windows") +
    theme_moon(narrow = narrow) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# ---- 6. The model's estimate among fake moons, by cycle length ------------------------------------
fig_fake_estimates <- function(fake_screen, fake_summary, fake_injected, cfg, narrow = FALSE) {
  p <- moon_pal
  d <- data.frame(period = fake_screen$period, swing = 2 * fake_screen$net)
  real  <- 2 * fake_summary$estimate[fake_summary$term == "net"]
  inj   <- 2 * fake_injected$estimate[fake_injected$term == "net"]
  band  <- stats::quantile(d$swing, c(0.025, 0.975))
  edge  <- 62.5                  # where the fake moons stop; labels sit to the right
  note  <- function(y, label, colour, size) list(
    ggplot2::annotate("segment", x = cfg$synodic_days, xend = edge * 1.04, y = y, yend = y,
                      colour = p$muted, linewidth = 0.3),
    ggplot2::annotate("text", x = edge * 1.06, y = y, label = label, hjust = 0, colour = colour,
                      size = size, lineheight = 0.95, family = moon_font()))
  # shorter notes for the narrow layout, where the margin beside the dots is an inch wide
  real_label <- if (narrow) "The real\nmoon: %s" else "The real moon: %s"
  inj_label  <- if (narrow) "After I added\na %g-point\nswing: %s"
                else "The real moon after I added a\n%g-point swing to the Wolves'\nscoring: %s"

  ggplot2::ggplot(d, ggplot2::aes(period, swing)) +
    ggplot2::annotate("rect", xmin = 14.4, xmax = edge, ymin = band[[1]], ymax = band[[2]],
                      fill = p$band) +
    ggplot2::annotate("segment", x = 14.4, xend = edge, y = 0, yend = 0, colour = p$axis,
                      linewidth = 0.4) +
    ggplot2::geom_point(shape = 21, fill = p$gray, colour = p$surface,
                        stroke = if (narrow) 0.35 else 0.5, size = if (narrow) 1.9 else 2.6) +
    note(real, sprintf(real_label, signed(real)), p$ink, if (narrow) 3.6 else 4) +
    note(inj, sprintf(inj_label, 2 * cfg$injected_effect, signed(inj)), p$ink2, 3.6) +
    ggplot2::annotate("point", x = cfg$synodic_days, y = inj, shape = 21,
                      size = if (narrow) 3.4 else 4.2, stroke = 1.1, colour = p$gold,
                      fill = p$surface) +
    geom_moon(ggplot2::aes(x, y, phase = phase),
              data = data.frame(x = cfg$synodic_days, y = real, phase = 0.5),
              size = if (narrow) 6 else 8) +
    ggplot2::scale_x_log10(breaks = c(15, 20, 30, 45, 60),
                           limits = c(14.4, if (narrow) 128 else 112),
                           expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_y_continuous(breaks = step_breaks(2), labels = tick_signed,
                                limits = c(min(d$swing) - 0.3, max(d$swing, inj) + 0.8)) +
    ggplot2::labs(
      title = fit_title("The same model on moons that do not exist", narrow),
      subtitle = fit_text(sprintf(paste0(
        "Each gray dot is one fake moon's effect on the Wolves' margin, low end to high end. I tried each cycle\n",
        "at two starting points. The band covers the middle 95%% of the dots, from %s to %s points."),
        signed(band[[1]]), signed(band[[2]])), narrow),
      x = "Length of the cycle, in days",
      y = fit_text("Change in the Wolves' margin, in points", narrow, width = 30, balance = TRUE)) +
    theme_moon(narrow = narrow) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = if (narrow) 0.3 else 0.36))
}

# ---- 7. The same question for all 30 teams ---------------------------------------------------------
fig_every_team <- function(every_team, cfg, narrow = FALSE) {
  d <- every_team[order(-every_team$net_swing), ]
  misses <- sum(d$lo > 0 | d$hi < 0)
  rank   <- which(d$team == cfg$team)
  d <- data.frame(group = "", label = if (narrow) nickname(d$team) else d$team,
                  est = d$net_swing, lo = d$lo, hi = d$hi,
                  colour = ifelse(d$team == cfg$team, moon_pal$gold, moon_pal$gray),
                  stringsAsFactors = FALSE)
  interval_plot(d, "Change in point margin, new moon to full moon", narrow = narrow) +
    ggplot2::labs(
      title = fit_title(sprintf("The Wolves' test on all %d teams", nrow(d)), narrow),
      subtitle = fit_text(sprintf(paste0(
        "Each team's full-moon effect on its own margin, with a 95%% interval. The Wolves rank %d of %d.\n",
        "By chance I would expect one or two of %d intervals to miss zero, and %s."),
        rank, nrow(d), nrow(d),
        if (misses == 1) "one does" else sprintf("%s do", number_word(misses))), narrow))
}

# ---- 8. Does the model know basketball? -----------------------------------------------------------------
fig_covariates <- function(covariates, team_games, narrow = FALSE) {
  cv <- covariates
  pick <- function(pattern, group, relabel) {
    rows <- cv[grepl(pattern, cv$term), ]
    data.frame(group = group, label = unname(relabel(rows$term)), est = rows$estimate,
               lo = rows$lo, hi = rows$hi, stringsAsFactors = FALSE)
  }
  d <- rbind(
    pick(":home_court$", "Playing at home, by season",
         function(t) sub("^season(.*):home_court$", "\\1", t)),
    pick("^rest", "Days of rest, against none",
         function(t) c(rest1 = "1 day", rest2 = "2 days", "rest3+" = "3 or more")[t]),
    pick("^opp_rest", "Opponent's days of rest, against none",
         function(t) c(opp_rest1 = "1 day", opp_rest2 = "2 days", "opp_rest3+" = "3 or more")[t]),
    pick("^game_type", "Against a regular-season game",
         function(t) c("game_typePlay-in" = "Play-in game", game_typePlayoffs = "Playoff game")[t]))
  d$colour <- moon_pal$blue
  overtime <- cv$estimate[cv$term == "overtimes"]
  interval_plot(d, "Points per team", narrow = narrow) +
    ggplot2::labs(
      title = fit_title("The model's estimates before I added the moon", narrow),
      subtitle = fit_text(sprintf(paste0(
        "Effects on a team's score with 95%% intervals, from %s team-games. Home court shrank in 2020-21,\n",
        "with crowds absent or small. I left out overtime, which adds %.1f points per period."),
        format(team_games, big.mark = ","), overtime), narrow))
}

# ---- 9. Other reasonable versions of the model ------------------------------------------------------------------
fig_sensitivity <- function(sensitivity, narrow = FALSE) {
  s <- sensitivity[sensitivity$term == "net", ]
  misses <- sum(s$swing_lo > 0 | s$swing_hi < 0)
  # the same checks as results/sensitivity.csv, in the page's wording
  plain <- c("Regular season only" = "Regular season alone",
             "No monthly form or drift terms" = "No form or drift terms by month",
             "Team quality fixed for all ten seasons" = "One level of team quality for all ten seasons")
  check <- ifelse(s$check %in% names(plain), plain[s$check], s$check)
  count <- if (narrow) "%s (%d games)" else "%s (%d Wolves games)"
  d <- data.frame(group = "", label = sprintf(count, check, s$n_wolves_games),
                  est = s$swing, lo = s$swing_lo, hi = s$swing_hi,
                  colour = moon_pal$gold, stringsAsFactors = FALSE)
  interval_plot(d, "Change in the Wolves' margin, new moon to full moon",
                narrow = narrow, above = TRUE) +
    ggplot2::labs(
      title = fit_title(sprintf("The Wolves' full-moon effect under %s versions of the model",
                                number_word(nrow(s))), narrow),
      subtitle = fit_text(sprintf(
        "Effect on point margin with 95%% intervals. %s.%s",
        if (misses == 0) sprintf("All %s intervals include zero", number_word(nrow(s)))
        else sprintf("%s of them miss zero", tools::toTitleCase(number_word(misses))),
        if (narrow) " The Wolves' games in each version are in parentheses." else ""), narrow))
}

# ---- 10. The flagged estimates in a second sample -------------------------------------------------------------
fig_second_sample <- function(second_sample, second_counts, cfg, narrow = FALSE) {
  s <- second_sample
  # the narrow layout breaks each heading after its colon
  what <- sprintf(if (narrow) "%s:\n%s" else "%s: %s",
                  c(league = "League, points per team", offense = "Wolves, points scored",
                    defense = "Wolves, points allowed", net = "Wolves, margin")[s$term],
                  tolower(MOON_LABELS[s$moon]))
  main   <- sprintf("Main sample, %s to %s", season_label(min(cfg$seasons)), season_label(max(cfg$seasons)))
  second <- sprintf("Second sample, %s to %s", second_counts$first_season, second_counts$last_season)
  colour <- ifelse(s$term == "league", moon_pal$blue, moon_pal$gold)
  d <- rbind(
    data.frame(group = what, label = main, est = s$swing_main, lo = s$swing_lo_main,
               hi = s$swing_hi_main, colour = colour, stringsAsFactors = FALSE),
    data.frame(group = what, label = second, est = s$swing_second, lo = s$swing_lo_second,
               hi = s$swing_hi_second, colour = colour, stringsAsFactors = FALSE))
  d <- d[order(match(d$group, what)), ]
  held <- sum(s$swing_lo_second > 0 | s$swing_hi_second < 0)
  interval_plot(d, "Change in points, low end of the moon variable to the high end", step = 1,
                narrow = narrow, above = TRUE) +
    ggplot2::labs(
      title = fit_title(sprintf("The %s flagged estimates in %s older seasons", number_word(nrow(s)),
                                number_word(second_counts$seasons)), narrow),
      subtitle = fit_text(sprintf(
        "I ran the same model on %d seasons that I had kept out of the main analysis.\n%s",
        second_counts$seasons,
        if (held == 0) "Both intervals include zero there."
        else sprintf("%s of the two intervals excludes zero there.",
                     tools::toTitleCase(number_word(held)))), narrow))
}

# ---- 11. The Wolves' full-moon edge in each season ---------------------------------------------------
fig_wolves_seasons <- function(wolves_seasons, samples, this_season, cfg, narrow = FALSE) {
  ws <- wolves_seasons
  heading <- function(sample) {
    rows <- ws[ws$sample == sample, ]
    sprintf("%s sample, %s to %s", if (sample == "main") "Main" else "Second",
            rows$season[1], rows$season[nrow(rows)])
  }
  p_value <- 2 * stats::pnorm(-abs(ws$estimate / ws$se))
  seasons <- data.frame(
    group = vapply(ws$sample, heading, ""),
    label = sprintf("%s (%s)", ws$season, record(ws$near_wins, ws$near_losses)),
    est = ws$estimate, lo = ws$lo, hi = ws$hi, colour = moon_pal$gold,
    note = ifelse(p_value < 0.05, sprintf("p = %.3f", p_value), NA), stringsAsFactors = FALSE)
  together <- data.frame(
    group = vapply(samples$sample, heading, ""),
    # the narrow layout has a shorter label, to leave the rows more room
    label = if (narrow) sprintf("All %d (%s)", samples$seasons,
                                record(samples$near_wins, samples$near_losses))
            else sprintf("All %s seasons (%s)", vapply(samples$seasons, number_word, ""),
                         record(samples$near_wins, samples$near_losses)),
    est = samples$estimate, lo = samples$lo, hi = samples$hi, colour = moon_pal$ink,
    note = signed(samples$estimate), stringsAsFactors = FALSE)
  main <- samples[samples$sample == "main", ]
  d <- rbind(seasons, together)
  d <- d[order(match(d$group, unique(seasons$group))), ]
  off_zero <- sum(p_value < 0.05)
  now <- if (is.null(this_season)) "" else sprintf(
    " In %s the Wolves are %s inside the window so far.", this_season$season,
    record(this_season$near_wins, this_season$near_losses))
  interval_plot(d, "Change in the Wolves' margin inside the full-moon window, in points",
                step = if (narrow) 10 else 5, narrow = narrow) +
    # room at the right for the notes beside the longest intervals
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = if (narrow) ggplot2::margin(14, 50, 10, 10)
                                 else ggplot2::margin(18, 84, 14, 18)) +
    ggplot2::labs(
      title = fit_title("The full-moon edge, one season at a time", narrow),
      subtitle = fit_text(sprintf(paste0(
        "Each gold row is one season's estimate with its 95%% interval, and the record inside the window in parentheses.\n",
        "%s of %d intervals exclude%s zero, and I would expect about %s by chance. The %s main seasons differ\n",
        "from one another by no more than chance (p = %.2f).%s"),
        tools::toTitleCase(number_word(off_zero)), nrow(ws), if (off_zero == 1) "s" else "",
        number_word(round(0.05 * nrow(ws))), number_word(main$seasons), main$season_p, now),
        narrow))
}

# ---- 12. Each team in each season ------------------------------------------------------------------------
fig_team_seasons <- function(by_season, pooled, cells, cfg, narrow = FALSE) {
  p <- moon_pal
  d <- by_season[by_season$measure == "window" & by_season$outcome == "net", ]
  all_ten <- pooled[pooled$measure == "window" & pooled$outcome == "net", ]
  teams   <- all_ten$team[order(all_ten$estimate)]               # best team drawn at the top
  seasons <- sort(unique(d$season))
  last    <- "All ten"
  d$team  <- factor(d$team, levels = teams)
  d$ring  <- abs(d$estimate) > stats::qnorm(0.975) * d$se
  all_ten$team <- factor(all_ten$team, levels = teams)
  limit <- 12
  wolves_row <- match(cfg$team, teams)
  # On the narrow layout a season goes by the year it ends, "2016-17" as "'17",
  # and a team by its nickname.
  short_season <- function(v) ifelse(v == last, "All", paste0("\u2019", substr(v, 6, 7)))
  ggplot2::ggplot(d, ggplot2::aes(season, team)) +
    ggplot2::geom_tile(ggplot2::aes(fill = estimate), width = 0.94, height = 0.84) +
    ggplot2::geom_point(data = d[d$ring, ], shape = 21, size = if (narrow) 1.9 else 2.6,
                        stroke = if (narrow) 0.65 else 0.8, fill = NA, colour = p$ink) +
    ggplot2::geom_text(ggplot2::aes(x = last, label = signed(estimate)), data = all_ten,
                       colour = ifelse(all_ten$team == cfg$team, p$ink, p$ink2),
                       fontface = ifelse(all_ten$team == cfg$team, "bold", "plain"),
                       size = if (narrow) 3.1 else 3.4, family = moon_font()) +
    ggplot2::annotate("rect", xmin = 0.5, xmax = length(seasons) + 0.5,
                      ymin = wolves_row - 0.5, ymax = wolves_row + 0.5,
                      fill = NA, colour = p$ink, linewidth = 0.5) +
    ggplot2::scale_x_discrete(limits = c(seasons, last), position = "top",
                              labels = if (narrow) short_season else ggplot2::waiver(),
                              expand = ggplot2::expansion(add = c(0.6, 0.7))) +
    ggplot2::scale_y_discrete(labels = if (narrow) nickname else ggplot2::waiver(),
                              expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_fill_gradient2(
      low = p$blue, mid = "#1c2746", high = p$gold, midpoint = 0, limits = c(-limit, limit),
      oob = function(x, range) pmin(pmax(x, range[1]), range[2]),
      breaks = c(-10, -5, 0, 5, 10), labels = tick_signed,
      guide = ggplot2::guide_colourbar(barwidth = grid::unit(if (narrow) 2.4 else 2.6, "in"),
                                       barheight = grid::unit(0.12, "in"),
                                       title.position = "top", ticks = FALSE)) +
    ggplot2::labs(
      title = fit_title(sprintf("%d teams, %s seasons", length(teams), number_word(length(seasons))),
                        narrow),
      subtitle = fit_text(sprintf(paste0(
        "Each cell is one team's change in margin inside the full-moon window, in one season.\n",
        "I ringed the %s cells with a 95%% interval that excludes zero, and I would expect about %.0f of %d by chance.\n",
        "I sorted the teams by their ten-season estimate, at the right, and outlined the Wolves' row."),
        number_word(cells$off_zero), cells$expected, cells$cells), narrow),
      x = NULL, y = NULL, fill = "Change in margin inside the window, in points") +
    theme_moon(narrow = narrow) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = p$ink2),
                   axis.text.x.top = ggplot2::element_text(colour = p$ink2),
                   legend.title = ggplot2::element_text(colour = p$ink2, size = ggplot2::rel(0.85)))
}

# All twelve figures in both layouts. Each row is a file name, the function that
# draws it, and the height in inches of the wide and of the narrow layout.
make_figures <- function(out, cfg) {
  r <- out$results
  figures <- list(
    list("games-by-moon", function(n) fig_games(out$w, r$counts, cfg, n), 7.2, 6.5),
    list("moon-variables", function(n) fig_moon_variables(cfg, n), 5.6, 5.4),
    list("phases", function(n) fig_phases(r$phase_wolves, n), 6, 5.9),
    list("estimates", function(n) fig_estimates(r$terms, n), 8.2, 10.1),
    list("fake-records", function(n) fig_fake_records(r$fake_records, r$counts, n), 5.6, 5.2),
    list("fake-estimates", function(n) fig_fake_estimates(r$fake_screen, r$fake_summary,
                                                          r$fake_summary_injected, cfg, n), 6, 5.6),
    list("every-team", function(n) fig_every_team(r$every_team, cfg, n), 9.4, 8.9),
    list("covariates", function(n) fig_covariates(r$covariates, r$counts$team_games, n), 8.6, 7.9),
    list("sensitivity", function(n) fig_sensitivity(r$sensitivity, n), 4.6, 5.4),
    list("second-sample", function(n) fig_second_sample(r$second_sample, r$second_counts, cfg, n),
         4.4, 5.2),
    list("wolves-seasons", function(n) fig_wolves_seasons(r$wolves_seasons, r$wolves_samples,
                                                          r$this_season, cfg, n), 10.4, 9.7),
    list("team-seasons", function(n) fig_team_seasons(r$breakdown$by_season, r$breakdown$pooled,
                                                      r$cells_off_zero, cfg, n), 10.25, 9))
  for (f in figures) {
    save_figure(f[[2]](FALSE), f[[1]], cfg, WIDE_INCHES, f[[3]])
    save_figure(f[[2]](TRUE), paste0(f[[1]], "-narrow"), cfg, NARROW_INCHES, f[[4]])
  }
  invisible(TRUE)
}
