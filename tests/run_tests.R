#!/usr/bin/env Rscript
# Checks on the building blocks. Run from the repo root:  Rscript tests/run_tests.R
# These need no downloads and finish in a few seconds.

library(testthat)
for (f in c("config", "data", "moon", "models", "placebo", "breakdown", "theme_moon", "figures", "site")) {
  source(file.path("R", paste0(f, ".R")))
}

test_that("moon_cos is the illuminated fraction rescaled", {
  t <- seq(as.POSIXct("2024-01-01", tz = "UTC"), by = "7 hours", length.out = 500)
  g <- add_moon_features(data.frame(tip_utc = t), cfg)
  expect_equal(g$moon_cos, 2 * g$moon_frac - 1, tolerance = 1e-12)
  expect_equal(g$moon_cos^2 + g$moon_sin^2, rep(1, 500), tolerance = 1e-12)
  expect_true(all(abs(g$days_from_full) <= cfg$synodic_days / 2 + 0.5))
})

test_that("full moons are found once per cycle, close to the reference times", {
  from <- as.POSIXct("2025-01-01", tz = "UTC"); to <- as.POSIXct("2025-12-31", tz = "UTC")
  ref  <- full_moon_times(from, to)          # the PyEphem table
  full <- full_moon_instants(from, to)       # suncalc's estimate
  expect_equal(format(ref[1], "%Y-%m-%d %H:%M"), "2025-01-13 22:26")
  expect_length(full, length(ref))
  expect_lt(max(abs(as.numeric(difftime(full, ref, units = "hours")))), 6)
})

test_that("moon_sin is negative before the full moon and positive after", {
  full <- full_moon_times(as.POSIXct("2025-01-01", tz = "UTC"),
                          as.POSIXct("2025-02-01", tz = "UTC"))[1]
  g <- add_moon_features(data.frame(tip_utc = full + c(-5, 5) * 86400), cfg)
  expect_lt(g$moon_sin[1], 0)
  expect_gt(g$moon_sin[2], 0)
  expect_equal(g$near_full, c(0L, 0L))
  expect_equal(add_moon_features(data.frame(tip_utc = full + 86400), cfg)$near_full, 1L)
})

test_that("named phases are centered on their namesakes", {
  expect_equal(as.character(phase_name(c(0, 0.25, 0.5, 0.75, 0.99, 0.07))),
               c("New", "First quarter", "Full", "Last quarter", "New", "Waxing crescent"))
})

test_that("suncalc agrees with the PyEphem reference", {
  v <- validate_moon()
  expect_gt(v$fraction_cor, 0.999)
  expect_lt(v$max_hours_off, 6)
  expect_gt(v$distance_cor, 0.95)
})

test_that("rest days count from the previous game in the same season", {
  team   <- rep("A", 5)
  season <- c("s1", "s1", "s1", "s1", "s2")
  dates  <- as.Date(c("2025-01-01", "2025-01-02", "2025-01-05", "2025-01-12", "2025-10-20"))
  expect_equal(as.character(rest_days(team, season, dates)), c("3+", "0", "2", "3+", "3+"))
})

test_that("fake moons leave out the real lunar cycles", {
  periods <- fake_periods(c(0, 3500), cfg)
  expect_true(all(periods >= 15 & periods <= 60))
  expect_true(all(abs(periods - cfg$synodic_days) > 0.2))
  expect_true(all(abs(periods - cfg$anomalistic_days) > 0.2))
  expect_gt(length(periods), 150)
})

test_that("the fast fake-moon estimate matches a full refit", {
  set.seed(1)
  n <- 600
  d <- data.frame(g = factor(sample(40, n, TRUE)), x = rnorm(n), m = rnorm(n))
  d$y <- 1 + 0.5 * d$x + 0.3 * d$m + rnorm(40)[d$g] + rnorm(n)
  null <- lme4::lmer(y ~ x + (1 | g), data = d, REML = FALSE)
  full <- lme4::lmer(y ~ x + m + (1 | g), data = d, REML = FALSE)
  fast <- gls_add(gls_setup(null), d$m)
  expect_equal(fast$est, unname(lme4::fixef(full)["m"]), tolerance = 0.01)
  expect_equal(sqrt(fast$vcov[1, 1]), sqrt(as.matrix(vcov(full))["m", "m"]), tolerance = 0.03)
})

test_that("the SVG moon is dark at new moon and whole at full moon", {
  expect_match(moon_svg(0), "A 14.000 14 0 0 0 14 0")     # terminator on the lit limb: no light
  expect_match(moon_svg(0.5), "A 14.000 14 0 0 1 14 0")   # terminator on the far limb: all lit
  expect_match(moon_svg(0.25), "A 0.000 14")              # straight terminator at first quarter
})

test_that("signed numbers use a real minus sign and no sign on zero", {
  expect_equal(signed(c(-1.26, 0.04, 2)), c("−1.3", "0.0", "+2.0"))
  expect_equal(record(55, 36), "55–36")
  expect_equal(ordinal(c(1, 2, 3, 11, 22)[2]), "2nd")
  expect_equal(vapply(c(1, 3, 11, 21), ordinal, ""), c("1st", "3rd", "11th", "21st"))
})

test_that("exhibition teams are dropped in finished seasons and in one still being played", {
  teams <- paste("Team", LETTERS[1:6])
  pairs <- expand.grid(home = teams, away = teams, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$home != pairs$away, ]
  finished <- pairs[rep(seq_len(nrow(pairs)), 3), ]                     # 30 games a team
  home   <- c(finished$home, "All-Stars East", pairs$home[1:6])
  away   <- c(finished$away, "All-Stars West", pairs$away[1:6])
  season <- c(rep(2024, nrow(finished) + 1), rep(2025, 6))              # 2025 has barely begun
  kept <- franchises(home, away, season)
  expect_false(any(grepl("All-Stars", kept)))
  expect_setequal(kept[grepl("2024$", kept)], paste(teams, 2024))
  expect_true(all(paste(unique(c(pairs$home[1:6], pairs$away[1:6])), 2025) %in% kept))
})

test_that("records inside the window are counted by team and season", {
  tg <- data.frame(team = c("A", "A", "A", "B", "B", "B"), season = c("s1", "s1", "s2", "s1", "s1", "s2"),
                   points = c(100, 90, 95, 90, 100, 99), opp_points = c(90, 100, 99, 100, 90, 95),
                   near_full = c(1, 1, 0, 1, 1, 0))
  rec <- window_records(tg)
  a1 <- rec[rec$team == "A" & rec$season == "s1", ]
  expect_equal(c(a1$near_wins, a1$near_losses, a1$far_wins, a1$far_losses), c(1, 1, 0, 0))
  b2 <- rec[rec$team == "B" & rec$season == "s2", ]
  expect_equal(c(b2$near_wins, b2$near_losses, b2$far_wins, b2$far_losses), c(0, 0, 1, 0))
})

test_that("a team's season-by-season effects find a planted effect and add up", {
  set.seed(2)
  teams <- paste("Team", LETTERS[1:6])
  games <- expand.grid(home = teams, away = teams, season = c("2001-02", "2002-03"), round = 1:8,
                       stringsAsFactors = FALSE)
  games <- games[games$home != games$away, ]
  games$game_id <- seq_len(nrow(games))
  games$moon    <- rbinom(nrow(games), 1, 0.3)
  pace <- rnorm(nrow(games), sd = 4)
  side <- function(team, opponent) data.frame(
    game_id = games$game_id, season = factor(games$season), team = team, opponent = opponent,
    moon = games$moon, stringsAsFactors = FALSE)
  tg <- rbind(side(games$home, games$away), side(games$away, games$home))
  planted <- 12 * tg$moon * (tg$team == "Team A") * (tg$season == "2002-03")
  tg$points <- 100 + planted + pace[tg$game_id] + rnorm(nrow(tg), sd = 8)
  g   <- gls_setup(lme4::lmer(points ~ season + (1 | game_id), data = tg, REML = FALSE))
  out <- season_effects(g, tg, "Team A", tg$moon)
  pick <- function(o) out$by_season$estimate[out$by_season$outcome == o]
  expect_equal(pick("net"), pick("offense") - pick("defense"))
  expect_gt(pick("offense")[2], 6)                   # the planted season
  expect_lt(abs(pick("offense")[1]), 6)              # the season with nothing planted
  expect_lt(out$tests$p[out$tests$outcome == "offense"], 0.05)
  all_seasons <- team_effect(g, tg, "Team A", tg$moon)
  expect_equal(all_seasons$estimate[all_seasons$outcome == "net"],
               all_seasons$estimate[all_seasons$outcome == "offense"] -
                 all_seasons$estimate[all_seasons$outcome == "defense"])
})

test_that("dates are written out the same way in any locale", {
  expect_equal(long_date("2026-11-05"), "November 5")
  expect_equal(long_date("2026-01-24"), "January 24")
})

test_that("a team's nickname is its last word, except Portland's", {
  expect_equal(nickname(c("Minnesota Timberwolves", "LA Clippers", "Philadelphia 76ers")),
               c("Timberwolves", "Clippers", "76ers"))
  expect_equal(nickname("Portland Trail Blazers"), "Trail Blazers")
})

test_that("the narrow layout wraps a figure's text and the wide layout leaves it alone", {
  long <- "Each dot is one game from 2016-17 to 2025-26,\na win above the line or a loss below it."
  expect_identical(fit_text(long, narrow = FALSE), long)
  lines <- strsplit(fit_text(long, narrow = TRUE, width = 30), "\n", fixed = TRUE)[[1]]
  expect_true(all(nchar(lines) < 30))
  expect_identical(paste(lines, collapse = " "), gsub("\n", " ", long, fixed = TRUE))
  # balancing keeps the number of lines and takes words off the longest one
  title <- "Sixteen estimates, two with p below 0.05"
  plain <- strsplit(fit_text(title, narrow = TRUE, width = 39), "\n", fixed = TRUE)[[1]]
  even  <- strsplit(fit_title(title, narrow = TRUE), "\n", fixed = TRUE)[[1]]
  expect_length(even, length(plain))
  expect_lt(max(nchar(even)), max(nchar(plain)))
  expect_identical(fit_title("30 teams, ten seasons", narrow = TRUE), "30 teams, ten seasons")
})

test_that("a figure on the page gets its size from the file and a narrow drawing for phones", {
  dir <- tempfile("figures")
  dir.create(dir)
  blank <- function(name, width, height) {
    ragg::agg_png(file.path(dir, name), width = width, height = height, units = "px")
    grid::grid.newpage()
    grDevices::dev.off()
  }
  blank("demo.png", 200, 120)
  blank("demo-narrow.png", 100, 130)
  expect_equal(png_size(file.path(dir, "demo-narrow.png")), c(100, 130))
  page <- add_narrow_figures('<figure><img src="assets/figures/demo.png" loading="lazy" alt="A demo."></figure>',
                             list(dir_figures = dir))
  expect_match(page, '<source media="(max-width: 44rem)" srcset="assets/figures/demo-narrow.png" width="100" height="130">',
               fixed = TRUE)
  expect_match(page, '<img src="assets/figures/demo.png" width="200" height="120" loading="lazy" alt="A demo.">',
               fixed = TRUE)
  expect_match(page, "^<figure><picture>")
})
