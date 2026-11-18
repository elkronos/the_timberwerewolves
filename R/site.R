# Builds docs/index.html from site/index.template.html.
#
# The template holds the prose with {{placeholders}}. Every result on the page,
# in the text and in the figure descriptions, is filled in here, so the words
# cannot drift from the models. What the prose says in words instead of
# numbers ("both intervals include zero") is checked in site_facts().

# ---- Number formatting -------------------------------------------------------------
minus <- function(s) gsub("-", "−", s, fixed = TRUE)
num   <- function(x, digits = 1) minus(formatC(x, format = "f", digits = digits))
big   <- function(n) format(n, big.mark = ",", trim = TRUE)
pct   <- function(x, digits = 0) formatC(100 * x, format = "f", digits = digits)
ordinal <- function(n) {
  suffix <- if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]
  paste0(n, suffix)
}
pval  <- function(p) if (p < 0.001) "below 0.001" else if (p < 0.1) sprintf("%.3f", p) else sprintf("%.2f", p)

# "2026-11-15" as "November 15". Month names are spelled out here so the page
# reads the same whatever language the session's locale is set to.
long_date <- function(iso) {
  d <- as.Date(iso)
  paste(month.name[as.integer(format(d, "%m"))], as.integer(format(d, "%d")))
}

# The phase each section of the page starts at, by section id: new moon at the
# top, full at "The result", and back toward new by the end. The moon beside
# each heading, the list of contents and the moon in the corner (site/moon.js)
# all use these.
SECTION_PHASES <- c(record = 0, games = 1 / 8, moon = 2 / 8, model = 3 / 8, result = 4 / 8,
                    "fake-moons" = 0.57, seasons = 0.64, teams = 0.71, models = 0.78,
                    verdict = 0.86, run = 0.93)

# Everything the interactive chart needs, as one list that becomes JSON: for
# each measure and outcome, a row per team holding its estimate in each season
# and then for all seasons together, with standard errors in the same layout
# and each team's record inside the window.
explorer_data <- function(r, cfg) {
  b <- r$breakdown
  teams   <- sort(unique(b$pooled$team))
  seasons <- sort(unique(b$by_season$season))
  named   <- function(x) stats::setNames(x, x)
  grid <- function(column) lapply(named(names(MEASURES)), function(m) {
    lapply(named(names(OUTCOME_LABELS)), function(o) {
      by  <- b$by_season[b$by_season$measure == m & b$by_season$outcome == o, ]
      all <- b$pooled[b$pooled$measure == m & b$pooled$outcome == o, ]
      lapply(teams, function(tm) {
        round(c(by[[column]][match(paste(tm, seasons), paste(by$team, by$season))],
                all[[column]][all$team == tm]), 3)
      })
    })
  })
  records <- lapply(teams, function(tm) {
    x <- r$window_records[r$window_records$team == tm, ]
    x <- x[match(seasons, x$season), ]
    unname(rbind(cbind(x$near_wins, x$near_losses), c(sum(x$near_wins), sum(x$near_losses))))
  })
  list(seasons = seasons, teams = teams, short = nickname(teams), focus = cfg$team,
       measures = lapply(MEASURES, `[[`, "label"), outcomes = as.list(OUTCOME_LABELS),
       est = grid("estimate"), se = grid("se"), record = records)
}

# The moon at a given phase as an inline SVG, drawn like geom_moon().
moon_svg <- function(phase, size = 28, class = "moon") {
  r    <- size / 2
  ph   <- phase %% 1
  edge <- cos(2 * pi * ph)
  waxing <- ph <= 0.5
  # limb: top to bottom down the lit side; terminator: back up to the top
  limb <- sprintf("A %g %g 0 0 %d %g %g", r, r, as.integer(waxing), r, size)
  term <- sprintf("A %.3f %g 0 0 %d %g 0", abs(edge) * r, r,
                  as.integer(if (waxing) edge < 0 else edge > 0), r)
  sprintf(paste0(
    '<svg class="%s" width="%g" height="%g" viewBox="0 0 %g %g" aria-hidden="true">',
    '<circle cx="%g" cy="%g" r="%g" fill="%s" stroke="%s" stroke-width="1"/>',
    '<path d="M %g 0 %s %s Z" fill="%s"/></svg>'),
    class, size, size, size, size, r, r, r - 0.5, moon_pal$moon_dark, moon_pal$moon_rim,
    r, limb, term, moon_pal$moon_lit)
}

# The sixteen estimates as an HTML table, the text twin of the figure.
estimates_table <- function(terms) {
  row <- function(i) sprintf(
    "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s to %s</td><td>%s</td><td>%s</td></tr>",
    c(league = "League, per team", offense = "Wolves, points scored",
      defense = "Wolves, points allowed", net = "Wolves, margin")[[terms$term[i]]],
    MOON_LABELS[[terms$moon[i]]], signed(terms$swing[i], 2),
    signed(terms$swing_lo[i], 2), signed(terms$swing_hi[i], 2),
    pval(terms$p[i]), pval(terms$p_holm[i]))
  ord <- order(match(terms$term, names(TERM_LABELS)))
  paste0(
    "<table><thead><tr><th>Who</th><th>Moon variable, low end to high end</th>",
    "<th>Change in points</th><th>95% interval</th><th>p</th><th>p after Holm</th></tr></thead><tbody>",
    paste(vapply(ord, row, ""), collapse = ""), "</tbody></table>")
}

# Every {{placeholder}} the template uses, as text.
site_facts <- function(out, cfg) {
  r <- out$results; n <- r$counts; tt <- r$terms
  term <- function(moon, which) tt[tt$moon == moon & tt$term == which, ]
  win_net <- term("near_full", "net");  win_def <- term("near_full", "defense")
  cos_net <- term("moon_cos", "net");   cos_off <- term("moon_cos", "offense")
  dist_lg <- term("moon_dist", "league")
  cv   <- r$covariates
  coef <- function(name) cv$estimate[cv$term == name]
  home <- cv$estimate[grepl(":home_court$", cv$term)]
  rest <- cv$estimate[grepl("^rest", cv$term)]
  opp_rest <- cv$estimate[grepl("^opp_rest", cv$term)]
  fs   <- r$fake_summary; fw <- r$fake_window_summary; fi <- r$fake_summary_injected
  fr   <- r$fake_records
  best <- fr[which.max(fr$win_rate), ]
  real_rate <- n$near_wins / (n$near_wins + n$near_losses)
  subs <- r$wolves_subsets
  best_subset <- vapply(split(subs, subs$outcome), function(d) d$moon_vars[which.min(d$aic)], "")
  ws   <- r$wolves_single
  wsp  <- function(outcome, col = "p") ws[[col]][ws$outcome == outcome & ws$moon == "near_full"]
  et   <- r$every_team[order(-r$every_team$net_swing), ]
  extreme <- et[which.max(abs(et$net_swing)), ]
  sens <- r$sensitivity[r$sensitivity$term == "net", ]
  # the Wolves' margin under each width of the full-moon window
  wn   <- r$windows[r$windows$term == "net", ]
  wn_off_zero <- abs(wn$estimate) > stats::qnorm(0.975) * wn$se
  ph   <- r$phase_wolves
  flagged <- sum(tt$p < 0.05)
  s2 <- r$second_sample; n2 <- r$second_counts
  s2_def  <- s2[s2$moon == "near_full" & s2$term == "defense", ]
  s2_dist <- s2[s2$moon == "moon_dist" & s2$term == "league", ]
  # spread of the fake estimates over the model's standard error, 1 when they agree
  fake_ratio <- function(d, which) d$fake_sd[d$term == which] / d$model_se[d$term == which]
  rate   <- function(wins, losses) wins / (wins + losses)
  fixed  <- out$games$game_date[out$games$time_fixed]
  # the 2020 restart in Orlando ran from 30 July to 11 October
  in_bubble <- out$games$game_date >= as.Date("2020-07-30") &
    out$games$game_date <= as.Date("2020-10-11")
  # p_fake is (1 + count) / (1 + n), so this recovers the count
  inj_larger <- round(fi$p_fake[fi$term == "net"] * (fi$n_fake[fi$term == "net"] + 1)) - 1
  # chance that two or more of the estimates fall under 0.05 with no effect anywhere
  two_or_more <- 1 - stats::pbinom(1, nrow(tt), 0.05)

  # season by season and team by team
  wsn   <- r$wolves_seasons
  wsn_main <- wsn[wsn$sample == "main", ]
  sm    <- r$wolves_samples
  main_season_p  <- sm$season_p[sm$sample == "main"]
  early_season_p <- sm$season_p[sm$sample == "second"]
  lead  <- wsn_main$near_wins - wsn_main$near_losses
  top_seasons <- wsn_main[lead == max(lead), ]
  others <- wsn_main[lead != max(lead), ]   # the seasons without the best record
  off_zero  <- wsn[wsn$lo > 0 | wsn$hi < 0, ]
  early_net <- sm[sm$sample == "second", ]
  bp <- r$breakdown$pooled
  explorer_window <- bp$estimate[bp$team == cfg$team & bp$measure == "window" & bp$outcome == "net"]
  by_window <- bp[bp$measure == "window" & bp$outcome == "net", ]
  by_window <- by_window[order(-by_window$estimate), ]            # best ten-season estimate first
  now   <- r$this_season
  ts_sd <- max(r$team_season_slopes$sd_offense, r$team_season_slopes$sd_defense)
  cells <- r$cells_off_zero
  season_intervals <- if (nrow(off_zero) == 0) {
    sprintf("All %d intervals include zero.", nrow(wsn))
  } else {
    sprintf("%s of the %d intervals exclude%s zero, %s above and %s below.",
            tools::toTitleCase(number_word(nrow(off_zero))), nrow(wsn),
            if (nrow(off_zero) == 1) "s" else "",
            number_word(sum(off_zero$lo > 0)), number_word(sum(off_zero$hi < 0)))
  }
  now_record <- record(now$near_wins, now$near_losses)
  now_sentence <- if (now$full_moons == 0) {
    "No full moon has passed yet."
  } else if (now$full_moons == 1) {
    sprintf("One full moon has passed, and they went %s in its window.", now_record)
  } else {
    sprintf("%s full moons have passed, and they are %s in those windows.",
            tools::toTitleCase(number_word(now$full_moons)), now_record)
  }

  # The prose names seasons, a team and a window width. A different setup
  # needs the template edited first.
  stopifnot(
    "the page text is written for the seasons 2016-17 to 2025-26" =
      identical(as.integer(cfg$seasons), 2017:2026),
    "the page text is written for a second sample of 2002-03 to 2015-16" =
      identical(as.integer(cfg$second_sample), 2003:2016),
    "the page text is written for the Timberwolves and a 1.5-day window" =
      cfg$team == "Minnesota Timberwolves" && cfg$near_full_days == 1.5,
    "text says 30 teams" = nrow(et) == 30)

  # The prose also makes claims in words. Stop if a rerun on new data ever
  # contradicts them, so nobody publishes a sentence the numbers do not support.
  stopifnot(
    "text says sixteen estimates" = nrow(tt) == 16,
    "text says two estimates have p < 0.05" = flagged == 2,
    "text says fourteen intervals include zero" = sum(tt$swing_lo < 0 & tt$swing_hi > 0) == 14,
    "text says two or more small p-values turn up about one time in five" =
      two_or_more > 0.15 && two_or_more < 0.25,
    "text says the moon shows no effect on the Wolves' scoring" =
      all(tt$p[tt$term == "offense"] > 0.05),
    "text says the corrected tip-off times are all in February 2020" =
      all(format(fixed, "%Y-%m") == "2020-02"),
    "text says the bubble games are neutral sites, and most of the neutral sites" =
      all(out$games$neutral[in_bubble]) && sum(in_bubble) > n$neutral_games / 2,
    "text says home court was smallest in 2020-21" =
      cv$term[grepl(":home_court$", cv$term)][which.min(home)] == "season2020-21:home_court",
    "text says rest helps, a rested opponent hurts, and playoff games score lower" =
      all(rest > 0) && all(opp_rest < 0) && coef("game_typePlayoffs") < 0,
    "text says league scoring rose over the decade" =
      coef(paste0("season", season_label(max(cfg$seasons)))) > 0,
    "text says the Wolves are above expectation at both full and new moon" =
      ph$mean[ph$phase == "Full"] > 0 && ph$mean[ph$phase == "New"] > 0,
    "text says all eight phase intervals include zero" = all(ph$lo < 0 & ph$hi > 0),
    "text says the error bars on the Wolves' margin are the right size" =
      abs(fake_ratio(fs, "net") - 1) < 0.1,
    "text says the real moon sits in the middle of the fake ones" =
      fs$p_fake[fs$term == "net"] > 0.2,
    "text says the fake estimates for points allowed spread wider than the model allows" =
      fake_ratio(fs, "defense") > 1 && fake_ratio(fw, "defense") > 1 &&
      fw$p_fake[fw$term == "defense"] > win_def$p,
    "text says the fake-moon test detects the added effect" = fi$p_fake[fi$term == "net"] < 0.05,
    # "probably" means power under one half, which holds below 1.96 standard errors
    "text says this data would probably miss an effect the size of home court" =
      stats::median(home) < stats::qnorm(0.975) * cos_net$se * r$swing[["moon_cos"]],
    "text says the defense estimate lost most of its size in the second sample" =
      s2_def$swing_second / s2_def$swing_main < 0.5,
    "text says the second sample shows no gap in wins" =
      abs(rate(n2$near_wins, n2$near_losses) - rate(n2$far_wins, n2$far_losses)) < 0.02,
    "text says those two are league distance and Wolves defense in the window" =
      dist_lg$p < 0.05 && win_def$p < 0.05,
    "text says opponents score fewer in the window" = win_def$estimate < 0,
    "text says the Wolves' margin is better in the window" = win_net$estimate > 0,
    "text says league scoring is lower when the moon is far" = dist_lg$estimate < 0,
    "text says the best models for points and margin have no moon variable" =
      best_subset[["points"]] == "none" && best_subset[["margin"]] == "none",
    "text says the best models for points allowed and wins add the window" =
      best_subset[["opp_points"]] == "near_full" && best_subset[["win"]] == "near_full",
    "text says the window widths run from half a day to three days" =
      identical(range(wn$window_days), c(0.5, 3)),
    "text says the two-day window is the only width whose interval excludes zero" =
      identical(wn$window_days[wn_off_zero], 2),
    "text says two seasons share the Wolves' best record inside the window" = nrow(top_seasons) == 2,
    "text says the other seasons are close to even inside the window" =
      abs(sum(others$near_wins) - sum(others$near_losses)) <= 3,
    "text says neither set of seasons differs by more than chance" =
      main_season_p > 0.05 && early_season_p > 0.05,
    "text says luck alone would put about one season's interval off zero" =
      round(0.05 * nrow(wsn)) == 1,
    "text treats the number of team-seasons off zero as within luck" =
      cells$off_zero <= stats::qbinom(0.975, cells$cells, 0.05),
    "text says a team's effect does not carry into the next season" = abs(r$carryover$r) < 0.15,
    "text says another team has a larger ten-season estimate inside the window than the Wolves" =
      by_window$team[1] != cfg$team,
    "the interactive chart must show the same Wolves estimate as the text" =
      abs(explorer_window - win_net$swing) < 0.01,
    "text says the league-wide test of all moon terms is carried by distance" =
      which.min(tt$p[tt$term == "league"]) == which(tt$moon[tt$term == "league"] == "moon_dist"),
    "text says every sensitivity interval includes zero" =
      all(sens$swing_lo < 0 & sens$swing_hi > 0),
    "text says the extreme team is not the Wolves" = extreme$team != cfg$team,
    "text says neither flagged estimate excludes zero in the second sample" =
      all(r$second_sample$swing_lo_second < 0 & r$second_sample$swing_hi_second > 0),
    "text says the Wolves' defense has the same sign in the second sample" = s2_def$same_sign)

  seasons_needed <- n$seasons * ((stats::qnorm(0.975) + stats::qnorm(0.80)) *
                                 win_net$se / win_net$estimate)^2
  last_year <- max(cfg$seasons)

  facts <- list(
    repo_url = cfg$repo_url, site_url = cfg$site_url,
    first_season = season_label(min(cfg$seasons)), last_season = season_label(last_year),
    next_season = season_label(last_year + 1L),
    games = big(n$games), team_games = big(n$team_games), wolves_games = big(n$wolves_games),
    lunar_cycles = n$lunar_cycles, exhibitions = n$exhibitions_dropped,
    time_fixed = number_word(n$time_fixed), neutral_games = n$neutral_games,
    bubble_games = sum(in_bubble),
    near_full_days = cfg$near_full_days,
    near_record = record(n$near_wins, n$near_losses),
    far_record = record(n$far_wins, n$far_losses),
    near_wins = n$near_wins, near_losses = n$near_losses,
    near_pct = pct(real_rate), far_pct = pct(n$far_wins / (n$far_wins + n$far_losses)),
    near_home_pct = pct(n$near_home_share), far_home_pct = pct(n$far_home_share),
    s22_record = record(n$s22_wins, n$s22_losses), s22_home = n$s22_home,
    s22_games = n$s22_wins + n$s22_losses,
    recent_record = record(n$recent_near_wins, n$recent_near_losses),
    # moon check
    frac_cor = formatC(r$moon_check$fraction_cor, format = "f", digits = 4),
    max_hours_off = num(r$moon_check$max_hours_off, 1),
    dist_cor = formatC(r$moon_check$distance_cor, format = "f", digits = 3),
    synodic_days = num(cfg$synodic_days, 1), anomalistic_days = num(cfg$anomalistic_days, 1),
    # the first drafts' two variables, illuminated fraction and days from the full moon
    frac_days_cor = num(stats::cor(out$w$moon_frac, abs(out$w$days_from_full)), 2),
    # what the model learned
    season_rise = num(coef(paste0("season", season_label(last_year))), 0),
    home_typical = num(stats::median(home), 0),
    home_2021 = num(coef("season2020-21:home_court"), 1),
    home_min = num(min(home), 1), home_max = num(max(home), 1),
    rest_min = num(min(rest), 1), rest_max = num(max(rest), 1),
    opp_rest_min = num(min(abs(opp_rest)), 1), opp_rest_max = num(max(abs(opp_rest)), 1),
    rest1 = num(coef("rest1"), 1), playoff_drop = num(abs(coef("game_typePlayoffs")), 1),
    overtime_pts = num(coef("overtimes"), 1),
    # results
    off_cos = signed(cos_off$swing), off_cos_lo = signed(cos_off$swing_lo),
    off_cos_hi = signed(cos_off$swing_hi),
    def_win_abs = num(abs(win_def$swing)), def_win_p = pval(win_def$p),
    def_win_holm = pval(win_def$p_holm),
    window_net = signed(win_net$swing), window_net_lo = signed(win_net$swing_lo),
    window_net_hi = signed(win_net$swing_hi), window_net_plain = num(win_net$swing),
    cos_net = signed(cos_net$swing), cos_net_lo = signed(cos_net$swing_lo),
    cos_net_hi = signed(cos_net$swing_hi),
    dist_league_abs = num(abs(dist_lg$swing)), dist_league_p = pval(dist_lg$p),
    dist_league_holm = pval(dist_lg$p_holm),
    omni_wolves_p = pval(r$omnibus$p[r$omnibus$test == "Wolves-specific moon terms"]),
    omni_league_p = pval(r$omnibus$p[r$omnibus$test == "League-wide moon terms"]),
    full_phase = num(ph$mean[ph$phase == "Full"]), new_phase = num(ph$mean[ph$phase == "New"]),
    wo_allowed_p = pval(wsp("opp_points")), wo_win_p = pval(wsp("win")),
    wo_allowed_holm = pval(wsp("opp_points", "p_holm")), wo_win_holm = pval(wsp("win", "p_holm")),
    expected_flags = num(0.05 * nrow(tt), 1),
    estimates_table = estimates_table(tt),
    # fake moons
    fake_moons = n$fake_moons, fake_windows = nrow(fr),
    fake_min_days = cfg$fake_period_range[1], fake_max_days = cfg$fake_period_range[2],
    window_share = pct(2 * cfg$near_full_days / cfg$synodic_days),
    fake_share = num(100 * mean(fr$win_rate >= real_rate), 1),
    fake_better = sum(fr$win_rate >= real_rate), fake_center = pct(mean(fr$win_rate)),
    best_fake_days = round(best$period), best_fake_record = record(best$wins, best$games - best$wins),
    fake_sd_net = num(fs$fake_sd[fs$term == "net"], 2), model_se_net = num(fs$model_se[fs$term == "net"], 2),
    p_fake_net = pct(fs$p_fake[fs$term == "net"]),
    def_excess_pct = pct(fake_ratio(fw, "defense") - 1),
    def_cos_excess = pct(fake_ratio(fs, "defense") - 1),
    def_win_p_fake = pval(fw$p_fake[fw$term == "defense"]),
    inj_swing = 2 * cfg$injected_effect,
    inj_net = signed(2 * fi$estimate[fi$term == "net"]),
    # how many fake estimates are at least as large as the one with the effect added
    inj_beyond = if (inj_larger == 0) "all" else paste("all but", number_word(inj_larger)),
    fake_estimates = fi$n_fake[fi$term == "net"],
    # limits
    detect_net = num(cos_net$detectable_swing), seasons_needed = round(seasons_needed),
    answer_year = min(cfg$seasons) - 1L + round(seasons_needed),
    wolves_rank = ordinal(which(et$team == cfg$team)),
    team_include_zero = sum(et$lo < 0 & et$hi > 0),
    extreme_team = nickname(extreme$team), extreme_swing = signed(extreme$net_swing),
    extreme_p = pval(extreme$p),
    slope_sd = num(max(r$team_slopes$sd_offense, r$team_slopes$sd_defense), 1),
    slope_p = pval(r$team_slopes$p),
    sens_min = signed(min(sens$swing)), sens_max = signed(max(sens$swing)),
    other_versions = number_word(nrow(sens) - 1), all_versions = number_word(nrow(sens)),
    window_widths = number_word(nrow(wn)),
    window_min = signed(min(wn$estimate)), window_max = signed(max(wn$estimate)),
    # second sample
    second_seasons = number_word(n2$seasons), second_first = n2$first_season,
    second_last = n2$last_season, second_games = big(n2$games),
    second_wolves_games = big(n2$wolves_games),
    second_local_seasons = number_word(n2$local_clock_seasons),
    second_near_record = record(n2$near_wins, n2$near_losses),
    second_far_record = record(n2$far_wins, n2$far_losses),
    second_near_pct = pct(n2$near_wins / (n2$near_wins + n2$near_losses)),
    second_far_pct = pct(n2$far_wins / (n2$far_wins + n2$far_losses)),
    second_dist = signed(s2_dist$swing_second), second_dist_lo = signed(s2_dist$swing_lo_second),
    second_dist_hi = signed(s2_dist$swing_hi_second),
    second_def_abs = num(abs(s2_def$swing_second)), second_def_lo = signed(s2_def$swing_lo_second),
    second_def_hi = signed(s2_def$swing_hi_second),
    second_def_ratio = pct(s2_def$swing_second / s2_def$swing_main),
    # season by season and team by team
    all_seasons = nrow(wsn),
    season_min = signed(min(wsn_main$estimate)),
    season_min_name = wsn_main$season[which.min(wsn_main$estimate)],
    season_max = signed(max(wsn_main$estimate)),
    season_max_name = wsn_main$season[which.max(wsn_main$estimate)],
    season_test_p = pval(main_season_p), early_season_test_p = pval(early_season_p),
    near_per_season = number_word(round((n$near_wins + n$near_losses) / n$seasons)),
    season_intervals = season_intervals,
    best_record = record(top_seasons$near_wins[1], top_seasons$near_losses[1]),
    best_season_1 = top_seasons$season[1], best_season_2 = top_seasons$season[2],
    other_seasons = number_word(nrow(others)),
    rest_record = record(sum(others$near_wins), sum(others$near_losses)),
    early_net = signed(early_net$estimate),
    early_net_lo = signed(early_net$lo), early_net_hi = signed(early_net$hi),
    now_season = now$season, now_through = long_date(now$through),
    now_record = record(now$wins, now$losses), now_sentence = now_sentence,
    now_next = long_date(now$next_full_moon),
    window_rank = ordinal(which(by_window$team == cfg$team)),
    window_top_team = nickname(by_window$team[1]), window_top = signed(by_window$estimate[1]),
    cells = cells$cells, cells_off = cells$off_zero, cells_expected = round(cells$expected),
    carry_r = num(r$carryover$r, 2), carry_pairs = r$carryover$pairs,
    ts_spread = if (ts_sd < 0.05) "zero" else paste(num(ts_sd, 1), "points"),
    ts_p = pval(r$team_season_slopes$p),
    # one file per season in the two samples, and one for the season in progress
    season_files = length(cfg$seasons) + length(cfg$second_sample) + 1L,
    r_version = sub("^R version ([0-9.]+).*$", "\\1", r$session$r),
    lme4_version = r$session$lme4, data_commit = substr(cfg$data_commit, 1, 7)
  )
  facts
}

# Give each <section> of the page its phase, put a moon of that phase beside
# its heading, and build the list of contents from the headings. The template
# marks the place for the list with <!-- contents -->.
add_section_marks <- function(page) {
  ids <- regmatches(page, gregexpr('(?<=<section id=")[^"]+', page, perl = TRUE))[[1]]
  if (!identical(ids, names(SECTION_PHASES))) {
    stop("The template has the sections ", paste(ids, collapse = ", "),
         " and SECTION_PHASES names ", paste(names(SECTION_PHASES), collapse = ", "), call. = FALSE)
  }
  items <- character(length(ids))
  for (i in seq_along(ids)) {
    opening <- sprintf('<section id="%s">[[:space:]]*<h2>([^<]+)</h2>', ids[i])
    found   <- regmatches(page, regexec(opening, page))[[1]]
    if (length(found) != 2) stop("Section ", ids[i], " must open with a plain <h2>", call. = FALSE)
    phase <- SECTION_PHASES[[i]]
    page  <- sub(found[1], sprintf('<section id="%s" data-phase="%s">\n  <h2>%s%s</h2>',
                                   ids[i], format(phase), moon_svg(phase), found[2]),
                 page, fixed = TRUE)
    items[i] <- sprintf('    <li><a href="#%s">%s%s</a></li>', ids[i], moon_svg(phase, size = 14), found[2])
  }
  nav <- paste0('<nav class="contents" id="contents" aria-label="Contents">\n',
                '  <p class="contents-label">Contents</p>\n  <ol>\n',
                paste(items, collapse = "\n"), "\n  </ol>\n</nav>")
  place <- "<!-- contents -->"
  if (lengths(regmatches(page, gregexpr(place, page, fixed = TRUE))) != 1) {
    stop("The template needs one ", place, " line", call. = FALSE)
  }
  sub(place, nav, page, fixed = TRUE)
}

# Width and height of a PNG file in pixels, read from its header.
png_size <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  readBin(con, "raw", 16)                                   # the signature and the start of the first chunk
  readBin(con, "integer", n = 2, size = 4, endian = "big")
}

# The template names the wide drawing of each figure in a plain <img>. Give
# each one its size in pixels, read from the file, and wrap it in a <picture>
# that hands a phone the narrow drawing instead.
add_narrow_figures <- function(page, cfg) {
  imgs <- unique(regmatches(page, gregexpr('<img src="assets/figures/[^>]+>', page))[[1]])
  for (img in imgs) {
    name   <- sub('^<img src="assets/figures/([a-z-]+)\\.png".*$', "\\1", img)
    wide   <- png_size(file.path(cfg$dir_figures, paste0(name, ".png")))
    narrow <- png_size(file.path(cfg$dir_figures, paste0(name, "-narrow.png")))
    sized  <- sub(".png\"", sprintf(".png\" width=\"%d\" height=\"%d\"", wide[1], wide[2]), img, fixed = TRUE)
    source <- sprintf('<source media="(max-width: %s)" srcset="assets/figures/%s-narrow.png" width="%d" height="%d">',
                      NARROW_SCREEN, name, narrow[1], narrow[2])
    page <- gsub(img, paste0("<picture>\n      ", source, "\n      ", sized, "\n    </picture>"),
                 page, fixed = TRUE)
  }
  page
}
# Screens up to this wide get the narrow drawings. site/style.css uses the same width.
NARROW_SCREEN <- "44rem"

# Replace every {{placeholder}} in a template file with its value.
fill_template <- function(path, facts) {
  text <- paste(readLines(path, encoding = "UTF-8"), collapse = "\n")
  keys <- gsub("[{}]", "", unique(regmatches(text, gregexpr("\\{\\{[a-z0-9_]+\\}\\}", text))[[1]]))
  missing <- setdiff(keys, names(facts))
  if (length(missing)) stop(path, " uses placeholders with no value: ", paste(missing, collapse = ", "))
  for (k in keys) text <- gsub(paste0("{{", k, "}}"), as.character(facts[[k]]), text, fixed = TRUE)
  text
}

write_utf8 <- function(text, path) {
  con <- file(path, open = "wb")
  writeLines(enc2utf8(text), con, useBytes = TRUE)
  close(con)
}

# Writes docs/index.html and README.md, both from templates in site/.
build_site <- function(out, cfg) {
  facts <- site_facts(out, cfg)
  dir.create(file.path("docs", "assets"), showWarnings = FALSE, recursive = TRUE)
  page <- add_section_marks(fill_template(file.path("site", "index.template.html"), facts))
  page <- add_narrow_figures(page, cfg)
  write_utf8(page, file.path("docs", "index.html"))
  write_utf8(fill_template(file.path("site", "README.template.md"), facts), "README.md")
  file.copy(file.path("site", c("style.css", "moon.js", "explorer.js", "scroll.js")),
            file.path("docs", "assets"), overwrite = TRUE)
  json <- jsonlite::toJSON(explorer_data(out$results, cfg), auto_unbox = TRUE, digits = NA)
  write_utf8(paste0("// Written by R/site.R from the results. Edit the R code, not this file.\n",
                    "window.MOON_DATA = ", json, ";"),
             file.path("docs", "assets", "explorer-data.js"))
  # tell GitHub Pages to serve the folder as it is, without Jekyll
  file.create(file.path("docs", ".nojekyll"))
  invisible(facts)
}
