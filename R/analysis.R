# Runs every model and collects the results in one list.
# Needs data.R, moon.R, models.R, placebo.R and breakdown.R.

tick <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

run_analysis <- function(data, cfg, second = NULL, this_season = NULL) {
  tg <- data$tg; w <- data$w
  swing  <- moon_swing(tg)
  t_days <- as.numeric(tg$tip_utc) / 86400
  res    <- list()

  # ---- League model without moon terms ----------------------------------------
  tick("League model without moon terms")
  fit0 <- fit_league(tg)
  res$covariates <- covariate_table(fit0)
  res$variance   <- variance_table(fit0)
  g <- gls_setup(fit0)
  stopifnot(length(g$y) == nrow(tg))

  # Points relative to expectation. "Expected" is everything the model knows
  # about a team-game except the shock shared by the two teams in that game.
  game_shock   <- lme4::ranef(fit0)$game_id[as.character(tg$game_id), "(Intercept)"]
  tg$expected  <- stats::fitted(fit0) - game_shock
  tg$resid_pts <- tg$points - tg$expected
  other        <- match(paste(tg$game_id, tg$opponent), paste(tg$game_id, tg$team))
  tg$resid_margin <- tg$resid_pts - tg$resid_pts[other]
  w <- merge(w, tg[tg$is_wolves == 1, c("game_id", "expected", "resid_pts", "resid_margin")],
             by = "game_id", sort = FALSE)
  w <- w[order(w$tip_utc), ]

  # ---- League model with each moon variable --------------------------------------
  tick("League model with each moon variable")
  res$terms <- do.call(rbind, lapply(MOON_VARS, function(v) {
    tick("  ", v)
    wolves_terms(fit_league(tg, v), v, swing[[v]])
  }))
  res$terms$p_holm <- stats::ave(res$terms$p, res$terms$term,
                                 FUN = function(p) stats::p.adjust(p, "holm"))

  tick("Cosine and sine together")
  res$rhythm <- rhythm_tests(fit_league(tg, c("moon_cos", "moon_sin")))

  tick("All four moon variables together")
  res$omnibus <- omnibus_tests(fit_league(tg, MOON_VARS))

  # ---- Fake moons --------------------------------------------------------------------
  tick("Fake moons")
  real_cos <- moon_terms(g, tg$moon_cos, tg)
  screen   <- fake_moon_screen(g, tg, cfg)
  res$fake_summary <- fake_moon_summary(real_cos, screen)
  res$fake_screen  <- screen

  # Amplitude of a strict cycle of any length, for the periodogram.
  cycle_amp <- function(gg, period) {
    a <- moon_terms(gg, fake_moon(t_days, period, 0), tg)[1:4]
    b <- moon_terms(gg, fake_moon(t_days, period, pi / 2), tg)[1:4]
    sqrt(a^2 + b^2)
  }
  periodogram <- function(gg, scr, data_label) {
    real_rows <- rbind(
      data.frame(period = cfg$synodic_days, kind = "Synodic month",
                 t(cycle_amp(gg, cfg$synodic_days))),
      data.frame(period = cfg$anomalistic_days, kind = "Anomalistic month",
                 t(cycle_amp(gg, cfg$anomalistic_days))))
    out <- rbind(cbind(fake_amplitudes(scr)[, "period", drop = FALSE], kind = "Fake moon",
                       fake_amplitudes(scr)[, -1]), real_rows)
    out$data <- data_label
    out
  }

  # Positive control: add a known Wolves effect and run the same test.
  g_inj      <- inject_effect(g, tg, cfg$injected_effect)
  screen_inj <- fake_moon_screen(g_inj, tg, cfg)
  res$fake_summary_injected <- fake_moon_summary(moon_terms(g_inj, tg$moon_cos, tg), screen_inj)
  res$periodogram <- rbind(periodogram(g, screen, "Real results"),
                           periodogram(g_inj, screen_inj,
                                       sprintf("With a %g-point swing added", 2 * cfg$injected_effect)))
  rownames(res$periodogram) <- NULL

  # The window variable, and the raw win-loss record, against fake windows.
  win_screen <- fake_window_screen(g, tg, cfg)
  res$fake_window_summary <- fake_moon_summary(moon_terms(g, tg$near_full, tg), win_screen)
  res$fake_window_screen  <- win_screen
  attr(w, "fake_periods") <- fake_periods(t_days, cfg)
  res$fake_records <- fake_window_records(w, cfg)

  # The distance variable is a cosine at the anomalistic period, so its
  # league-wide estimate can be compared with the fake cosines directly once
  # both are expressed per unit of cosine.
  dist_per_cos <- stats::sd(tg$moon_dist) * sqrt(2)
  dist_league  <- res$terms$estimate[res$terms$moon == "moon_dist" & res$terms$term == "league"]
  res$fake_distance <- data.frame(
    estimate_per_cos = dist_league * dist_per_cos, fake_sd = stats::sd(screen$league),
    p_fake = (1 + sum(abs(screen$league) >= abs(dist_league * dist_per_cos))) /
             (1 + nrow(screen)))

  tick("Checking the fast fake-moon method against full refits")
  res$gls_check <- check_gls_against_refit(tg, g, cfg, real_cos, res$terms)

  # ---- Every team -----------------------------------------------------------------------
  tick("Team-by-team slopes")
  slopes <- fit_team_slopes(tg)
  res$team_slopes <- slopes[c("sd_offense", "sd_defense", "chisq", "p", "singular")]
  if (slopes$singular) tick("  (lme4 put a team-slope variance at zero, a singular fit; that is the result)")
  res$every_team  <- every_team(g, tg, swing[["moon_cos"]])

  # ---- Each team in each season -------------------------------------------------------------
  tick("Each team in each season")
  res$breakdown      <- breakdown(g, tg)
  res$window_records <- window_records(tg)
  res$carryover      <- carryover(res$breakdown$by_season)
  res$cells_off_zero <- cells_off_zero(res$breakdown$by_season)
  season_slopes <- fit_team_slopes(tg, "near_full", c("team_season", "opp_season"))
  res$team_season_slopes <- season_slopes[c("sd_offense", "sd_defense", "chisq", "p", "singular")]
  if (season_slopes$singular) tick("  (lme4 put the team-season slope variances at zero, a singular fit; that is the result)")

  # ---- Wolves-only regressions ------------------------------------------------------------
  tick("Wolves-only regressions")
  res$wolves_subsets <- do.call(rbind, lapply(names(WOLVES_OUTCOMES),
                                              function(o) wolves_subsets(w, o)))
  singles <- lapply(names(WOLVES_OUTCOMES), function(o) wolves_single(w, o, swing))
  res$wolves_single <- do.call(rbind, lapply(singles, `[[`, "single"))
  res$wolves_joint  <- data.frame(outcome = names(WOLVES_OUTCOMES),
                                  p_all_four = vapply(singles, `[[`, numeric(1), "p_all_four"))

  # ---- Does the answer survive other reasonable choices? ------------------------------------
  tick("Sensitivity checks")
  res$sensitivity <- sensitivity_checks(tg, swing)
  res$windows     <- window_checks(g, tg)

  # ---- Averages by named phase, for the figures ------------------------------------------------
  w$phase_name  <- phase_name(w$moon_phase)
  tg$phase_name <- phase_name(tg$moon_phase)
  res$phase_wolves <- mean_by_phase(w$resid_margin, w$phase_name)
  # league: one value per game (the mean of its two rows), so games stay independent
  game_resid <- tapply(tg$resid_pts, tg$game_id, mean)
  game_phase <- tg$phase_name[match(names(game_resid), tg$game_id)]
  res$phase_league <- mean_by_phase(as.numeric(game_resid), game_phase)

  if (!is.null(second)) {
    tick("Second sample: the same two tests on earlier seasons")
    res$second_sample <- second_sample_tests(second, flagged = res$terms[res$terms$p < 0.05, ],
                                             swing = swing)
    res$second_counts <- second_sample_counts(second)

    tick("Second sample: the Wolves season by season")
    g2    <- gls_setup(fit_league(second$tg))
    near2 <- second$tg$near_full
    records2 <- window_records(second$tg)
    seasons2 <- season_effects(g2, second$tg, cfg$team, near2)
    res$second_wolves <- list(
      by_season = seasons2$by_season, tests = seasons2$tests,
      pooled    = team_effect(g2, second$tg, cfg$team, near2),
      records   = records2[records2$team == cfg$team, ])
  }
  res$wolves_seasons <- wolves_season_table(res, cfg)
  res$wolves_samples <- wolves_sample_table(res, cfg)
  if (!is.null(this_season)) res$this_season <- this_season_summary(this_season, cfg)

  res$counts <- describe_sample(data$games, tg, w, cfg)
  res$moon_check <- validate_moon()
  res$swing  <- swing
  res$session <- list(r = R.version.string,
                      lme4 = as.character(utils::packageVersion("lme4")),
                      suncalc = as.character(utils::packageVersion("suncalc")),
                      data_commit = cfg$data_commit)
  list(results = res, tg = tg, w = w, games = data$games)
}

# Every estimate that had p below 0.05 in the main sample, re-estimated with
# the same model on seasons the main analysis never touched. A real effect
# should show up again with the same sign.
second_sample_tests <- function(second, flagged, swing) {
  tg2 <- second$tg
  out <- do.call(rbind, lapply(unique(flagged$moon), function(v) {
    tick("  ", v)
    wolves_terms(fit_league(tg2, v), v, swing[[v]])
  }))
  out <- merge(flagged[, c("moon", "term", "swing", "swing_lo", "swing_hi", "p")], out,
               by = c("moon", "term"), suffixes = c("_main", "_second"))
  out$same_sign <- sign(out$swing_main) == sign(out$swing_second)
  out
}

second_sample_counts <- function(second) {
  w <- second$w; near <- w$near_full == 1
  list(games = nrow(second$games), wolves_games = nrow(w),
       local_clock_seasons = sum(substr(unique(as.character(w$season)), 1, 4) < "2008"),
       first_season = min(as.character(w$season)), last_season = max(as.character(w$season)),
       seasons = length(unique(w$season)),
       near_wins = sum(w$win[near]), near_losses = sum(1 - w$win[near]),
       far_wins = sum(w$win[!near]), far_losses = sum(1 - w$win[!near]))
}

# Fit the mixed model in full for a few fake moons and for the real one, and
# compare with the fast method. Differences should be far below one standard error.
check_gls_against_refit <- function(tg, g, cfg, real_cos, terms, n = 3) {
  t_days  <- as.numeric(tg$tip_utc) / 86400
  periods <- fake_periods(t_days, cfg)
  pick    <- periods[round(seq(1, length(periods), length.out = n + 2))[2:(n + 1)]]
  rows <- lapply(pick, function(p) {
    tg$fake <- fake_moon(t_days, p, 0)
    full <- wolves_terms(fit_league(tg, "fake"), "fake")
    data.frame(moon = sprintf("fake, %.1f days", p), term = full$term,
               refit = full$estimate, fast = unname(moon_terms(g, tg$fake, tg)[1:4]))
  })
  real <- terms[terms$moon == "moon_cos", ]
  rows[[length(rows) + 1]] <- data.frame(moon = "real moon", term = real$term,
                                         refit = real$estimate, fast = unname(real_cos[1:4]))
  out <- do.call(rbind, rows)
  out$difference <- out$fast - out$refit
  out
}

# The moon_cos terms under other defensible versions of the model.
sensitivity_checks <- function(tg, swing) {
  no_game_type <- sub("game_type \\+ ", "", LEAGUE_CONTROLS)
  no_overtime  <- sub("overtimes \\+ ", "", LEAGUE_CONTROLS)
  per48 <- transform(tg, points = points * 48 / (48 + 5 * overtimes))
  specs <- list(
    list("Main model", tg, LEAGUE_CONTROLS, LEAGUE_RANDOM),
    list("Regular season only", tg[tg$game_type == "Regular season", ], no_game_type, LEAGUE_RANDOM),
    list("Without the two pandemic seasons",
         tg[!tg$season %in% c("2019-20", "2020-21"), ], LEAGUE_CONTROLS, LEAGUE_RANDOM),
    list("2021-22 onward", tg[as.character(tg$season) >= "2021-22", ], LEAGUE_CONTROLS, LEAGUE_RANDOM),
    list("Points per 48 minutes", per48, no_overtime, LEAGUE_RANDOM),
    list("No monthly form or drift terms", tg, LEAGUE_CONTROLS,
         "(1 | team_season) + (1 | opp_season) + (1 | game_id)"),
    list("Team quality fixed for all ten seasons", tg, LEAGUE_CONTROLS,
         "(1 | team) + (1 | opponent) + (1 | game_id)"))
  do.call(rbind, lapply(specs, function(s) {
    tick("  ", s[[1]])
    d <- droplevels(s[[2]])
    out <- wolves_terms(fit_league(d, "moon_cos", s[[3]], s[[4]]), "moon_cos", swing[["moon_cos"]])
    cbind(check = s[[1]], n_wolves_games = sum(d$is_wolves), out)
  }))
}

# The full-moon window at other widths.
window_checks <- function(g, tg, widths = c(0.5, 1, 1.5, 2, 3)) {
  do.call(rbind, lapply(widths, function(k) {
    inside <- as.numeric(abs(tg$days_from_full) <= k)
    m <- moon_terms(g, inside, tg)
    data.frame(window_days = k, wolves_games_inside = sum(inside * tg$is_wolves),
               term = c("league", "offense", "defense", "net"),
               estimate = unname(m[1:4]), se = unname(m[5:8]))
  }))
}

mean_by_phase <- function(x, phase) {
  n  <- tapply(x, phase, length)
  mu <- tapply(x, phase, mean)
  se <- tapply(x, phase, stats::sd) / sqrt(n)
  data.frame(phase = factor(names(mu), levels = PHASE_NAMES), n = as.integer(n),
             mean = as.numeric(mu), se = as.numeric(se),
             lo = as.numeric(mu - stats::qnorm(0.975) * se),
             hi = as.numeric(mu + stats::qnorm(0.975) * se), row.names = NULL)
}

describe_sample <- function(games, tg, w, cfg) {
  near <- w$near_full == 1
  near_new <- pmin(w$moon_phase, 1 - w$moon_phase) * cfg$synodic_days <= cfg$near_full_days
  recent <- as.character(w$season) >= "2021-22"
  s22    <- near & w$season == "2021-22"
  list(
    games = nrow(games), team_games = nrow(tg), wolves_games = nrow(w),
    teams = length(unique(tg$team)), seasons = length(cfg$seasons),
    first_game = format(min(games$game_date)), last_game = format(max(games$game_date)),
    lunar_cycles = length(unique(games$lunation)),
    neutral_games = sum(games$neutral), overtime_games = sum(games$overtimes > 0),
    time_fixed = sum(games$time_fixed),
    exhibitions_dropped = attr(games, "exhibitions_dropped"),
    # schedule luck: share of home games inside and outside the window
    near_home_share = mean(w$home_court[near]), far_home_share = mean(w$home_court[!near]),
    # the 2021-22 season, and everything since, inside the window
    s22_wins = sum(w$win[s22]), s22_losses = sum(1 - w$win[s22]), s22_home = sum(w$home_court[s22]),
    recent_near_wins = sum(w$win[near & recent]),
    recent_near_losses = sum(1 - w$win[near & recent]),
    wolves_near_full = sum(near),
    wolves_wins = sum(w$win), wolves_losses = sum(1 - w$win),
    near_wins = sum(w$win[near]), near_losses = sum(1 - w$win[near]),
    far_wins = sum(w$win[!near]), far_losses = sum(1 - w$win[!near]),
    near_points = mean(w$points[near]), far_points = mean(w$points[!near]),
    near_margin = mean(w$margin[near]), far_margin = mean(w$margin[!near]),
    near_resid_margin = mean(w$resid_margin[near]),
    far_resid_margin = mean(w$resid_margin[!near]),
    # the same window around the new moon, for contrast
    new_wins   = sum(w$win[near_new]), new_losses = sum(1 - w$win[near_new]),
    new_resid_margin = mean(w$resid_margin[near_new]),
    fake_moons = length(fake_periods(as.numeric(tg$tip_utc) / 86400, cfg))
  )
}

# Write the tables as CSV and the scalars as JSON.
write_results <- function(res, cfg, w = NULL) {
  dir.create(cfg$dir_results, showWarnings = FALSE, recursive = TRUE)
  if (!is.null(w)) {
    keep <- c("game_date", "season", "game_type", "opponent", "home_court", "points",
              "opp_points", "win", "days_from_full", "near_full", "moon_phase", "moon_frac",
              "moon_cos", "moon_sin", "moon_dist", "resid_margin")
    out <- w[, keep]
    num <- vapply(out, is.double, logical(1)) & names(out) != "game_date"
    out[num] <- lapply(out[num], round, 4)
    utils::write.csv(out, file.path(cfg$dir_results, "wolves_games.csv"), row.names = FALSE)
  }
  tables <- c("terms", "rhythm", "omnibus", "fake_summary", "fake_summary_injected",
              "fake_window_summary", "fake_distance", "fake_records", "periodogram",
              "gls_check", "every_team", "wolves_subsets", "wolves_single", "wolves_joint",
              "sensitivity", "windows", "phase_wolves", "phase_league", "covariates",
              "variance", "second_sample")
  for (t in tables) {
    utils::write.csv(res[[t]], file.path(cfg$dir_results, paste0(t, ".csv")), row.names = FALSE)
  }
  # The breakdown tables are long, so their numbers are rounded to four places.
  rounded <- function(d) { num <- vapply(d, is.double, logical(1)); d[num] <- lapply(d[num], round, 4); d }
  breakdown_tables <- list(team_effects = res$breakdown$pooled, team_seasons = res$breakdown$by_season,
                           season_tests = res$breakdown$tests, window_records = res$window_records,
                           wolves_seasons = res$wolves_seasons, wolves_samples = res$wolves_samples)
  for (t in names(breakdown_tables)) {
    utils::write.csv(rounded(breakdown_tables[[t]]), file.path(cfg$dir_results, paste0(t, ".csv")),
                     row.names = FALSE)
  }
  scalars <- list(counts = res$counts, second_sample = res$second_counts,
                  this_season = res$this_season,
                  team_slopes = res$team_slopes, team_season_slopes = res$team_season_slopes,
                  carryover = res$carryover, cells_off_zero = res$cells_off_zero,
                  moon_check = res$moon_check, swing = as.list(res$swing),
                  session = res$session)
  jsonlite::write_json(scalars, file.path(cfg$dir_results, "summary.json"),
                       auto_unbox = TRUE, pretty = TRUE, digits = 6)
}
