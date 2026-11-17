# The moon effect broken down by team and by season.
#
# Every number here comes from the league model with its variance components
# held at their no-moon values (gls_setup() and gls_add() in R/placebo.R), so
# one team's breakdown takes a fraction of a second and all thirty a few.
#
# Each estimate is that team's difference from the rest of the league, on
# three outcomes:
#   offense  points the team scores
#   defense  points its opponents score against it (negative is better defense)
#   net      offense minus defense, the team's margin

# The two ways the page measures the moon. `swing` turns a coefficient into
# points: 1 for the window (inside against outside) and 2 for moon_cos (new
# moon to full moon).
MEASURES <- list(
  window = list(column = "near_full", swing = 1, label = "Inside the full-moon window"),
  smooth = list(column = "moon_cos",  swing = 2, label = "New moon to full moon"))

OUTCOME_LABELS <- c(net = "Margin", offense = "Points scored", defense = "Points allowed")

# Rows that read offense, defense and net out of k blocks of coefficients laid
# out as [league, offense, defense], each block k columns wide.
outcome_rows <- function(k) {
  zero <- matrix(0, k, k); one <- diag(k)
  list(offense = cbind(zero, one, zero), defense = cbind(zero, zero, one),
       net     = cbind(zero, one, -one))
}

# One team's effect over all seasons together: one row per outcome.
team_effect <- function(g, tg, team, moon, swing = 1) {
  A   <- cbind(moon, moon * (tg$team == team), moon * (tg$opponent == team))
  fit <- gls_add(g, A)
  L   <- outcome_rows(1)
  do.call(rbind, lapply(names(L), function(o) data.frame(
    team = team, outcome = o,
    estimate = drop(L[[o]] %*% fit$est) * swing,
    se = sqrt(drop(L[[o]] %*% fit$vcov %*% t(L[[o]]))) * swing)))
}

# One team's effect in each season. The league's own effect is fitted season
# by season as well, so each number is the team's difference from the league
# in that season. Also tests, for each outcome, whether the seasons differ
# from one another by more than chance (a Wald test on k - 1 contrasts).
season_effects <- function(g, tg, team, moon, swing = 1) {
  S <- stats::model.matrix(~ 0 + season, tg)
  k <- ncol(S)
  A <- cbind(S * moon, S * (moon * (tg$team == team)), S * (moon * (tg$opponent == team)))
  stopifnot(all(colSums(A != 0) > 0))       # every season needs games to estimate from
  fit <- gls_add(g, A)
  L   <- outcome_rows(k)
  differ <- cbind(-1, diag(k - 1))          # each later season against the first
  parts <- lapply(names(L), function(o) {
    est <- drop(L[[o]] %*% fit$est) * swing
    V   <- (L[[o]] %*% fit$vcov %*% t(L[[o]])) * swing^2
    d   <- differ %*% est
    chi <- drop(t(d) %*% solve(differ %*% V %*% t(differ), d))
    list(rows = data.frame(team = team, season = levels(tg$season), outcome = o,
                           estimate = est, se = sqrt(diag(V))),
         test = data.frame(team = team, outcome = o, chisq = chi, df = k - 1,
                           p = stats::pchisq(chi, k - 1, lower.tail = FALSE)))
  })
  list(by_season = do.call(rbind, lapply(parts, `[[`, "rows")),
       tests     = do.call(rbind, lapply(parts, `[[`, "test")))
}

# Every team, both measures: pooled effects, season-by-season effects, and the
# tests of whether a team's seasons differ.
breakdown <- function(g, tg) {
  teams <- sort(unique(as.character(tg$team)))
  per_measure <- lapply(names(MEASURES), function(m) {
    moon  <- tg[[MEASURES[[m]]$column]]; swing <- MEASURES[[m]]$swing
    pooled  <- do.call(rbind, lapply(teams, function(tm) team_effect(g, tg, tm, moon, swing)))
    seasons <- lapply(teams, function(tm) season_effects(g, tg, tm, moon, swing))
    tag <- function(d) cbind(measure = m, d, stringsAsFactors = FALSE)
    list(pooled    = tag(pooled),
         by_season = tag(do.call(rbind, lapply(seasons, `[[`, "by_season"))),
         tests     = tag(do.call(rbind, lapply(seasons, `[[`, "tests"))))
  })
  out <- lapply(c(pooled = "pooled", by_season = "by_season", tests = "tests"),
                function(part) do.call(rbind, lapply(per_measure, `[[`, part)))
  lapply(out, function(d) { rownames(d) <- NULL; d })
}

# Wins and losses inside and outside the full-moon window, for each team in
# each season.
window_records <- function(tg) {
  win <- tg$points > tg$opp_points
  key <- interaction(tg$team, tg$season, drop = TRUE, sep = "|")
  count <- function(rows) tapply(rows, key, sum)
  parts <- strsplit(levels(key), "|", fixed = TRUE)
  out <- data.frame(team = vapply(parts, `[`, "", 1), season = vapply(parts, `[`, "", 2),
                    near_wins   = as.integer(count(win & tg$near_full == 1)),
                    near_losses = as.integer(count(!win & tg$near_full == 1)),
                    far_wins    = as.integer(count(win & tg$near_full == 0)),
                    far_losses  = as.integer(count(!win & tg$near_full == 0)),
                    stringsAsFactors = FALSE)
  out[order(out$team, out$season), ]
}

# Does a team's effect in one season carry into the next? The correlation
# between consecutive seasons of the same team, over every such pair.
carryover <- function(by_season, measure = "window", outcome = "net") {
  d <- by_season[by_season$measure == measure & by_season$outcome == outcome, ]
  d <- d[order(d$team, d$season), ]
  nxt <- c(d$estimate[-1], NA); same_team <- c(d$team[-1] == d$team[-nrow(d)], FALSE)
  list(r = stats::cor(d$estimate[same_team], nxt[same_team]), pairs = sum(same_team))
}

# How many team-seasons have an interval that excludes zero, against the
# number luck alone would produce at the 5% level.
cells_off_zero <- function(by_season, measure = "window", outcome = "net") {
  d <- by_season[by_season$measure == measure & by_season$outcome == outcome, ]
  list(cells = nrow(d), off_zero = sum(abs(d$estimate) > stats::qnorm(0.975) * d$se),
       expected = 0.05 * nrow(d))
}

# The season in progress: the Wolves' record so far, inside and outside the
# window, and how many full moons the season has seen. No model is involved.
this_season_summary <- function(now, cfg) {
  w <- now$w; near <- w$near_full == 1
  played <- range(now$games$tip_utc)
  moons  <- attr(now$games, "full_moons")
  list(season = season_label(cfg$current_season),
       through = format(max(now$games$game_date), "%Y-%m-%d"),
       league_games = nrow(now$games), wolves_games = nrow(w),
       wins = sum(w$win == 1), losses = sum(w$win == 0),
       near_wins = sum(w$win[near] == 1), near_losses = sum(w$win[near] == 0),
       full_moons = sum(moons >= played[1] & moons <= played[2]),
       next_full_moon = format(min(moons[moons > played[2]]), "%Y-%m-%d"))
}

# The Wolves' margin inside the window in every season of both samples, with
# the record beside it. The two samples come from separate fits of the model.
wolves_season_table <- function(res, cfg) {
  one <- function(by_season, records, sample) {
    d <- by_season[by_season$team == cfg$team & by_season$outcome == "net", ]
    r <- records[match(d$season, records$season), ]
    data.frame(sample = sample, season = d$season, estimate = d$estimate, se = d$se,
               lo = d$estimate - stats::qnorm(0.975) * d$se,
               hi = d$estimate + stats::qnorm(0.975) * d$se,
               near_wins = r$near_wins, near_losses = r$near_losses, stringsAsFactors = FALSE)
  }
  main <- res$breakdown$by_season[res$breakdown$by_season$measure == "window", ]
  out  <- one(main, res$window_records[res$window_records$team == cfg$team, ], "main")
  if (!is.null(res$second_wolves)) {
    out <- rbind(one(res$second_wolves$by_season, res$second_wolves$records, "second"), out)
  }
  out
}

# The same margin with all of a sample's seasons together, and the test of
# whether that sample's seasons differ from one another. One row per sample.
wolves_sample_table <- function(res, cfg) {
  one <- function(pooled, tests, sample) {
    seasons <- res$wolves_seasons[res$wolves_seasons$sample == sample, ]
    p <- pooled[pooled$outcome == "net", ]
    t <- tests[tests$outcome == "net", ]
    data.frame(sample = sample, seasons = nrow(seasons),
               near_wins = sum(seasons$near_wins), near_losses = sum(seasons$near_losses),
               estimate = p$estimate, se = p$se,
               lo = p$estimate - stats::qnorm(0.975) * p$se,
               hi = p$estimate + stats::qnorm(0.975) * p$se,
               season_chisq = t$chisq, season_df = t$df, season_p = t$p,
               stringsAsFactors = FALSE)
  }
  mine <- function(d) d[d$team == cfg$team & d$measure == "window", ]
  out  <- one(mine(res$breakdown$pooled), mine(res$breakdown$tests), "main")
  if (!is.null(res$second_wolves)) {
    out <- rbind(one(res$second_wolves$pooled, res$second_wolves$tests, "second"), out)
  }
  out
}
