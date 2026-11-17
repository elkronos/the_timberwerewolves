# Every figure on the site. Each function takes data or result tables and
# returns a ggplot; make_figures() saves them all as PNGs.

MOON_LABELS <- c(
  moon_cos  = "New moon to full moon",
  moon_sin  = "First quarter to last quarter",
  near_full = "Outside to inside the full-moon window",
  moon_dist = "Moon at its nearest to its farthest")

TERM_LABELS <- c(
  league  = "League: points per team, all 30 teams",
  offense = "Wolves: points scored, beyond the league effect",
  defense = "Wolves: points allowed, beyond the league effect",
  net     = "Wolves: margin (scored minus allowed)")

NUMBER_WORDS <- c("none", "one", "two", "three", "four", "five", "six", "seven",
                  "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                  "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
number_word <- function(n) if (n <= 20) NUMBER_WORDS[n + 1] else as.character(n)

days_axis <- function() ggplot2::scale_x_continuous(
  breaks = c(-14, -7, 0, 7, 14), limits = c(-15.8, 15.8),
  labels = c("14 days before", "7 before", "Full moon", "7 after", "14 days after"),
  expand = ggplot2::expansion(mult = 0))

# ---- 1. Every Wolves game, lined up by the moon -------------------------------------
fig_games <- function(w, counts, cfg) {
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
                        stroke = 0.5, size = 3) +
    geom_moon(ggplot2::aes(x, y, phase = phase),
              data = moon_axis(cfg$synodic_days, top), size = 8) +
    ggplot2::scale_fill_manual(values = c(Win = p$gold, Loss = p$gray)) +
    days_axis() +
    ggplot2::scale_y_continuous(breaks = seq(-20, 20, 10), labels = abs,
                                limits = c(-max(d$stack) - 1, top + 1.6)) +
    ggplot2::labs(
      title = sprintf("%d Timberwolves games, lined up by the moon", nrow(w)),
      subtitle = sprintf(paste0(
        "One dot per game, %s to %s. Wins stack up and losses stack down.\n",
        "Inside the shaded full-moon window the Wolves are %s. Outside it they are %s."),
        season_label(min(cfg$seasons)), season_label(max(cfg$seasons)),
        record(counts$near_wins, counts$near_losses),
        record(counts$far_wins, counts$far_losses)),
      x = NULL, y = "Games") +
    theme_moon() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# ---- 2. How the moon becomes numbers ---------------------------------------------------
fig_moon_variables <- function(cfg) {
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
              data = moon_axis(cfg$synodic_days, 1.4), size = 8) +
    ggplot2::scale_colour_manual(values = colours) +
    days_axis() +
    ggplot2::scale_y_continuous(breaks = c(-1, 0, 1), labels = tick_signed,
                                limits = c(-1.12, 1.55)) +
    ggplot2::labs(
      title = "One lunar month as three variables",
      subtitle = sprintf(paste0(
        "moon_cos peaks at the full moon. moon_sin is negative while the moon waxes and positive while\n",
        "it wanes. near_full is 1 inside the shaded window, %s nights wide, and 0 outside it."),
        number_word(round(2 * cfg$near_full_days))),
      x = NULL, y = "Value of the variable") +
    theme_moon() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# ---- 3. The Wolves by named phase ----------------------------------------------------------
fig_phases <- function(phase_wolves) {
  p <- moon_pal
  d <- phase_wolves
  d$x     <- as.integer(d$phase)
  d$moon  <- (d$x - 1) / 8
  d$label <- sprintf("%s\n%d games", gsub(" ", "\n", d$phase), d$n)
  low     <- min(d$lo) - 1.5
  misses  <- sum(d$lo > 0 | d$hi < 0)
  verdict <- if (misses == 0) "All eight include zero."
             else sprintf("%s of the eight miss zero.", tools::toTitleCase(number_word(misses)))

  ggplot2::ggplot(d, ggplot2::aes(x, mean)) +
    ggplot2::geom_hline(yintercept = 0, colour = p$axis, linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lo, ymax = hi), colour = p$gold,
                            linewidth = 0.7) +
    ggplot2::geom_point(colour = p$gold, size = 3.4) +
    ggplot2::geom_text(ggplot2::aes(label = signed(mean)),
                       data = d[d$phase %in% c("New", "Full"), ],
                       nudge_x = 0.3, colour = p$ink, size = 4, family = moon_font()) +
    geom_moon(ggplot2::aes(x, y, phase = moon), data = transform(d, y = low), size = 9) +
    ggplot2::scale_x_continuous(breaks = d$x, labels = d$label,
                                expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_y_continuous(breaks = step_breaks(2), labels = tick_signed) +
    ggplot2::labs(
      title = "The Wolves against expectation, phase by phase",
      subtitle = paste0(
        "Average point margin compared with what the model expected from the opponent, venue and rest.\n",
        "Bars are 95% intervals. ", verdict),
      x = NULL, y = "Points better or worse than expected") +
    theme_moon() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# ---- Shared layout for the dot-and-interval figures --------------------------------------
#
# Rows are grouped under headings. `d` needs columns group, label, est, lo, hi
# and colour (the mark's color), plus an optional `note` printed at the right end of the interval.
# Rows are drawn top to bottom in the order given.
interval_plot <- function(d, x_title, step = 2) {
  p <- moon_pal
  groups <- unique(d$group)
  heads  <- data.frame(group = groups, y = NA_real_)
  y <- numeric(nrow(d)); at <- 0
  for (k in seq_along(groups)) {
    if (nzchar(groups[k])) { heads$y[k] <- at; at <- at - 1 }
    rows <- which(d$group == groups[k])
    y[rows] <- at - seq_along(rows) + 1
    at <- at - length(rows) - 0.7
  }
  d$y  <- y
  span <- range(c(d$lo, d$hi, 0))
  left <- span[1] - diff(span) * 0.02
  if (is.null(d$note)) d$note <- NA_character_

  ggplot2::ggplot(d, ggplot2::aes(est, y)) +
    ggplot2::geom_vline(xintercept = 0, colour = p$axis, linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lo, xmax = hi, colour = colour),
                            linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(colour = colour), size = 3.2) +
    ggplot2::geom_text(ggplot2::aes(x = left, y = y, label = group),
                       data = heads[nzchar(heads$group), ],
                       hjust = 0, colour = p$ink, fontface = "bold", size = 3.9,
                       family = moon_font()) +
    ggplot2::geom_text(ggplot2::aes(x = hi, label = note), data = d[!is.na(d$note), ],
                       hjust = -0.12, colour = p$ink, size = 3.6, family = moon_font()) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_y_continuous(breaks = d$y, labels = d$label,
                                expand = ggplot2::expansion(add = c(0.8, 0.6))) +
    ggplot2::scale_x_continuous(breaks = step_breaks(step), labels = tick_signed) +
    ggplot2::labs(x = x_title, y = NULL) +
    theme_moon() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = p$ink2))
}

# ---- 4. The sixteen estimates ------------------------------------------------------------------
fig_estimates <- function(terms) {
  d <- data.frame(
    group  = unname(TERM_LABELS[terms$term]), label = unname(MOON_LABELS[terms$moon]),
    est = terms$swing, lo = terms$swing_lo, hi = terms$swing_hi,
    colour = ifelse(terms$term == "league", moon_pal$blue, moon_pal$gold),
    note = ifelse(terms$p < 0.05, sprintf("p = %.3f", terms$p), NA),
    stringsAsFactors = FALSE)
  d <- d[order(match(d$group, TERM_LABELS)), ]
  flagged <- sum(terms$p < 0.05)
  interval_plot(d, "Change in points from the low end of each moon variable to the high end") +
    ggplot2::labs(
      title = sprintf("%s estimates, %s with p below 0.05",
                      tools::toTitleCase(number_word(nrow(terms))), number_word(flagged)),
      subtitle = sprintf(paste0(
        "Each row is one moon variable's effect with its 95%% interval. Blue is the whole league. Gold is the\n",
        "Wolves' difference from the league. The p-values shown are before any correction for %d tests."),
        nrow(terms)))
}

# ---- 5. The real full-moon record among fake ones --------------------------------------------------
fig_fake_records <- function(fake_records, counts) {
  p <- moon_pal
  real  <- counts$near_wins / (counts$near_wins + counts$near_losses)
  best  <- fake_records[which.max(fake_records$win_rate), ]
  share <- mean(fake_records$win_rate >= real)
  h     <- graphics::hist(fake_records$win_rate, breaks = seq(0.30, 0.70, by = 0.02), plot = FALSE)
  bars  <- data.frame(x = h$mids, n = h$counts)
  top   <- max(bars$n)

  ggplot2::ggplot(bars, ggplot2::aes(x, n)) +
    ggplot2::geom_col(fill = p$gray, width = 0.011) +
    ggplot2::annotate("segment", x = real, xend = real, y = 0, yend = top * 1.03,
                      colour = p$gold, linewidth = 0.9) +
    geom_moon(ggplot2::aes(x, y, phase = phase),
              data = data.frame(x = real, y = top * 1.12, phase = 0.5), size = 9) +
    ggplot2::annotate("text", x = real + 0.014, y = top * 1.12, hjust = 0, colour = p$ink,
                      size = 4, family = moon_font(), lineheight = 0.95,
                      label = sprintf("The real full moon\n%s", record(counts$near_wins, counts$near_losses))) +
    ggplot2::annotate("segment", x = best$win_rate, xend = best$win_rate, y = top * 0.035,
                      yend = top * 0.2, colour = p$muted, linewidth = 0.3) +
    ggplot2::annotate("text", x = best$win_rate, y = top * 0.22, hjust = 0.5, vjust = 0,
                      colour = p$ink2, size = 3.6, family = moon_font(), lineheight = 0.95,
                      label = sprintf("Best fake window: %s\non a cycle of %.0f days",
                                      record(best$wins, best$games - best$wins), best$period)) +
    ggplot2::scale_x_continuous(labels = function(v) paste0(round(v * 100), "%"),
                                breaks = seq(0.3, 0.7, 0.1), limits = c(0.3, 0.72)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.02)),
                                limits = c(0, top * 1.25)) +
    ggplot2::labs(
      title = sprintf("%d fake full-moon windows, and the real one", nrow(fake_records)),
      subtitle = sprintf(paste0(
        "The Wolves' win rate inside each fake full-moon window. %.1f%% of them\n",
        "match or beat the real moon's record."), 100 * share),
      x = "Wolves win rate inside the window", y = "Fake windows") +
    theme_moon() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# ---- 6. The model's estimate among fake moons, by cycle length ------------------------------------
fig_fake_estimates <- function(fake_screen, fake_summary, fake_injected, cfg) {
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

  ggplot2::ggplot(d, ggplot2::aes(period, swing)) +
    ggplot2::annotate("rect", xmin = 14.4, xmax = edge, ymin = band[[1]], ymax = band[[2]],
                      fill = p$band) +
    ggplot2::annotate("segment", x = 14.4, xend = edge, y = 0, yend = 0, colour = p$axis,
                      linewidth = 0.4) +
    ggplot2::geom_point(shape = 21, fill = p$gray, colour = p$surface, stroke = 0.5, size = 2.6) +
    note(real, sprintf("The real moon: %s", signed(real)), p$ink, 4) +
    note(inj, sprintf("The real moon with a %g-point\nswing added to the Wolves'\nscoring: %s",
                      2 * cfg$injected_effect, signed(inj)), p$ink2, 3.6) +
    ggplot2::annotate("point", x = cfg$synodic_days, y = inj, shape = 21, size = 4.2,
                      stroke = 1.1, colour = p$gold, fill = p$surface) +
    geom_moon(ggplot2::aes(x, y, phase = phase),
              data = data.frame(x = cfg$synodic_days, y = real, phase = 0.5), size = 8) +
    ggplot2::scale_x_log10(breaks = c(15, 20, 30, 45, 60), limits = c(14.4, 112),
                           expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_y_continuous(breaks = step_breaks(2), labels = tick_signed,
                                limits = c(min(d$swing) - 0.3, max(d$swing, inj) + 0.8)) +
    ggplot2::labs(
      title = "The same model, fitted to moons that do not exist",
      subtitle = sprintf(paste0(
        "Each gray dot is a fake moon's effect on the Wolves' margin, low end to high end. Each cycle is\n",
        "tried at two starting points. The band holds the middle 95%% of the dots, from %s to %s points."),
        signed(band[[1]]), signed(band[[2]])),
      x = "Length of the cycle, in days", y = "Change in the Wolves' margin, in points") +
    theme_moon() +
    ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = 0.36))
}

# ---- 7. The same question for all 30 teams ---------------------------------------------------------
fig_every_team <- function(every_team, cfg) {
  d <- every_team[order(-every_team$net_swing), ]
  misses <- sum(d$lo > 0 | d$hi < 0)
  rank   <- which(d$team == cfg$team)
  d <- data.frame(group = "", label = d$team, est = d$net_swing, lo = d$lo, hi = d$hi,
                  colour = ifelse(d$team == cfg$team, moon_pal$gold, moon_pal$gray),
                  stringsAsFactors = FALSE)
  interval_plot(d, "Change in point margin, new moon to full moon") +
    ggplot2::labs(
      title = "Asking every team the Wolves' question",
      subtitle = sprintf(paste0(
        "Each team's full-moon effect on its own margin, with a 95%% interval. The Wolves rank %d of %d.\n",
        "Luck alone should push one or two of 30 intervals off zero. Here it is %s."),
        rank, nrow(d), number_word(misses)))
}

# ---- 8. Does the model know basketball? -----------------------------------------------------------------
fig_covariates <- function(covariates, team_games) {
  cv <- covariates
  pick <- function(pattern, group, relabel) {
    rows <- cv[grepl(pattern, cv$term), ]
    data.frame(group = group, label = unname(relabel(rows$term)), est = rows$estimate,
               lo = rows$lo, hi = rows$hi, stringsAsFactors = FALSE)
  }
  d <- rbind(
    pick(":home_court$", "Playing at home, by season",
         function(t) sub("^season(.*):home_court$", "\\1", t)),
    pick("^rest", "Days of rest, compared with none",
         function(t) c(rest1 = "1 day", rest2 = "2 days", "rest3+" = "3 or more")[t]),
    pick("^opp_rest", "Opponent's days of rest, compared with none",
         function(t) c(opp_rest1 = "1 day", opp_rest2 = "2 days", "opp_rest3+" = "3 or more")[t]),
    pick("^game_type", "Compared with a regular-season game",
         function(t) c("game_typePlay-in" = "Play-in game", game_typePlayoffs = "Playoff game")[t]))
  d$colour <- moon_pal$blue
  overtime <- cv$estimate[cv$term == "overtimes"]
  interval_plot(d, "Points per team") +
    ggplot2::labs(
      title = "What the model learned before it met the moon",
      subtitle = sprintf(paste0(
        "Effects on a team's score, with 95%% intervals, from %s team-games. Home court shrank in 2020-21,\n",
        "when crowds were absent or small. Not shown: each overtime period adds %.1f points."),
        format(team_games, big.mark = ","), overtime))
}

# ---- 9. Other reasonable versions of the model ------------------------------------------------------------------
fig_sensitivity <- function(sensitivity) {
  s <- sensitivity[sensitivity$term == "net", ]
  misses <- sum(s$swing_lo > 0 | s$swing_hi < 0)
  d <- data.frame(group = "", label = sprintf("%s (%d Wolves games)", s$check, s$n_wolves_games),
                  est = s$swing, lo = s$swing_lo, hi = s$swing_hi,
                  colour = moon_pal$gold, stringsAsFactors = FALSE)
  interval_plot(d, "Change in the Wolves' margin, new moon to full moon") +
    ggplot2::labs(
      title = sprintf("The Wolves' full-moon effect under %s versions of the model",
                      number_word(nrow(s))),
      subtitle = sprintf("Effect on point margin with 95%% intervals. %s.",
                         if (misses == 0) "Every interval includes zero"
                         else sprintf("%s of them miss zero", tools::toTitleCase(number_word(misses)))))
}

# ---- 10. The flagged estimates in a second sample -------------------------------------------------------------
fig_second_sample <- function(second_sample, second_counts, cfg) {
  s <- second_sample
  what <- sprintf("%s: %s", c(league = "League, points per team", offense = "Wolves, points scored",
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
  interval_plot(d, "Change in points, low end of the moon variable to the high end", step = 1) +
    ggplot2::labs(
      title = sprintf("The %s estimates that stood out, tried on other seasons", number_word(nrow(s))),
      subtitle = sprintf(
        "The same model on %d seasons the main analysis never used.\n%s", second_counts$seasons,
        if (held == 0) "Neither estimate has an interval that excludes zero there."
        else sprintf("%s of the two has an interval that excludes zero there.",
                     tools::toTitleCase(number_word(held)))))
}

# ---- 11. The Wolves' full-moon edge in each season ---------------------------------------------------
fig_wolves_seasons <- function(wolves_seasons, samples, this_season, cfg) {
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
    label = sprintf("All %s seasons (%s)", vapply(samples$seasons, number_word, ""),
                    record(samples$near_wins, samples$near_losses)),
    est = samples$estimate, lo = samples$lo, hi = samples$hi, colour = moon_pal$ink,
    note = signed(samples$estimate), stringsAsFactors = FALSE)
  main <- samples[samples$sample == "main", ]
  d <- rbind(seasons, together)
  d <- d[order(match(d$group, unique(seasons$group))), ]
  off_zero <- sum(p_value < 0.05)
  now <- if (is.null(this_season)) "" else sprintf(
    " So far in %s the record inside the window is %s.", this_season$season,
    record(this_season$near_wins, this_season$near_losses))
  interval_plot(d, "Change in the Wolves' margin inside the full-moon window, in points", step = 5) +
    # room at the right for the notes beside the longest intervals
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(18, 84, 14, 18)) +
    ggplot2::labs(
      title = "The full-moon edge, one season at a time",
      subtitle = sprintf(paste0(
        "Each gold row is one season's estimate with its 95%% interval, and the record inside the window in parentheses.\n",
        "%s of %d intervals exclude%s zero, and luck alone would give about %s. The %s main seasons differ\n",
        "from one another by no more than chance (p = %.2f).%s"),
        tools::toTitleCase(number_word(off_zero)), nrow(ws), if (off_zero == 1) "s" else "",
        number_word(round(0.05 * nrow(ws))), number_word(main$seasons), main$season_p, now))
}

# ---- 12. Every team in every season ---------------------------------------------------------------------
fig_team_seasons <- function(by_season, pooled, cells, cfg) {
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
  ggplot2::ggplot(d, ggplot2::aes(season, team)) +
    ggplot2::geom_tile(ggplot2::aes(fill = estimate), width = 0.94, height = 0.84) +
    ggplot2::geom_point(data = d[d$ring, ], shape = 21, size = 2.6, stroke = 0.8,
                        fill = NA, colour = p$ink) +
    ggplot2::geom_text(ggplot2::aes(x = last, label = signed(estimate)), data = all_ten,
                       colour = ifelse(all_ten$team == cfg$team, p$ink, p$ink2),
                       fontface = ifelse(all_ten$team == cfg$team, "bold", "plain"),
                       size = 3.4, family = moon_font()) +
    ggplot2::annotate("rect", xmin = 0.5, xmax = length(seasons) + 0.5,
                      ymin = wolves_row - 0.5, ymax = wolves_row + 0.5,
                      fill = NA, colour = p$ink, linewidth = 0.5) +
    ggplot2::scale_x_discrete(limits = c(seasons, last), position = "top",
                              expand = ggplot2::expansion(add = c(0.6, 0.7))) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_fill_gradient2(
      low = p$blue, mid = "#1c2746", high = p$gold, midpoint = 0, limits = c(-limit, limit),
      oob = function(x, range) pmin(pmax(x, range[1]), range[2]),
      breaks = c(-10, -5, 0, 5, 10), labels = tick_signed,
      guide = ggplot2::guide_colourbar(barwidth = grid::unit(2.6, "in"),
                                       barheight = grid::unit(0.12, "in"),
                                       title.position = "top", ticks = FALSE)) +
    ggplot2::labs(
      title = "Every team, every season",
      subtitle = sprintf(paste0(
        "Each cell is one team's margin inside the full-moon window in one season, against the rest of that season.\n",
        "A ring marks the %s cells whose 95%% interval excludes zero. Luck alone would ring about %.0f of %d.\n",
        "Teams are ordered by their ten-season estimate, printed at the right. The Wolves' row is outlined."),
        number_word(cells$off_zero), cells$expected, cells$cells),
      x = NULL, y = NULL, fill = "Points of margin inside the window") +
    theme_moon() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = p$ink2),
                   axis.text.x.top = ggplot2::element_text(colour = p$ink2),
                   legend.title = ggplot2::element_text(colour = p$ink2, size = ggplot2::rel(0.85)))
}

make_figures <- function(out, cfg) {
  r <- out$results
  save_figure(fig_games(out$w, r$counts, cfg), "games-by-moon", cfg, 10, 7.2)
  save_figure(fig_moon_variables(cfg), "moon-variables", cfg, 10, 5.6)
  save_figure(fig_phases(r$phase_wolves), "phases", cfg, 10, 6)
  save_figure(fig_estimates(r$terms), "estimates", cfg, 10, 8.2)
  save_figure(fig_fake_records(r$fake_records, r$counts), "fake-records", cfg, 10, 5.6)
  save_figure(fig_fake_estimates(r$fake_screen, r$fake_summary, r$fake_summary_injected, cfg),
              "fake-estimates", cfg, 10, 6)
  save_figure(fig_every_team(r$every_team, cfg), "every-team", cfg, 10, 9.4)
  save_figure(fig_covariates(r$covariates, r$counts$team_games), "covariates", cfg, 10, 8.6)
  save_figure(fig_sensitivity(r$sensitivity), "sensitivity", cfg, 10, 4.6)
  save_figure(fig_second_sample(r$second_sample, r$second_counts, cfg), "second-sample", cfg, 10, 4.4)
  save_figure(fig_wolves_seasons(r$wolves_seasons, r$wolves_samples, r$this_season, cfg),
              "wolves-seasons", cfg, 10, 10.4)
  save_figure(fig_team_seasons(r$breakdown$by_season, r$breakdown$pooled, r$cells_off_zero, cfg),
              "team-seasons", cfg, 10, 10.25)
  invisible(TRUE)
}
