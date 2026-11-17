#!/usr/bin/env Rscript
# The season-by-season numbers against a full refit.
#
# R/breakdown.R holds the model's variance components at their no-moon values,
# which lets it work through thirty teams in seconds. This script fits the
# league model once more with the Wolves' full-moon effect split by season and
# everything estimated again, then sets the two answers side by side.
#
# Run from the repo root after `Rscript run_all.R data models` (takes under a
# minute):
#   Rscript checks/season_breakdown.R

suppressPackageStartupMessages({library(lme4); library(Matrix)})
for (f in c("config", "moon", "models", "placebo", "breakdown")) source(file.path("R", paste0(f, ".R")))
tg <- readRDS(file.path(cfg$dir_cache, "data.rds"))$tg

seasons <- levels(tg$season)
k    <- length(seasons)
S    <- stats::model.matrix(~ 0 + season, tg)
moon <- tg$near_full
A <- cbind(S * moon, S * (moon * (tg$team == cfg$team)), S * (moon * (tg$opponent == cfg$team)))
colnames(A) <- paste0(rep(c("league_", "offense_", "defense_"), each = k), seq_len(k))

rhs <- paste(c(colnames(A), LEAGUE_CONTROLS, LEAGUE_RANDOM), collapse = " + ")
fit <- lme4::lmer(stats::as.formula(paste("points ~", rhs)), data = cbind(tg, A), REML = FALSE,
                  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
b <- lme4::fixef(fit)[colnames(A)]
V <- as.matrix(stats::vcov(fit))[colnames(A), colnames(A)]

fast  <- utils::read.csv(file.path(cfg$dir_results, "team_seasons.csv"))
fast  <- fast[fast$measure == "window" & fast$team == cfg$team, ]
tests <- utils::read.csv(file.path(cfg$dir_results, "season_tests.csv"))
tests <- tests[tests$measure == "window" & tests$team == cfg$team, ]

L <- outcome_rows(k)
differ <- cbind(-1, diag(k - 1))
for (o in names(L)) {
  est <- drop(L[[o]] %*% b)
  Vo  <- L[[o]] %*% V %*% t(L[[o]])
  d   <- differ %*% est
  chi <- drop(t(d) %*% solve(differ %*% Vo %*% t(differ), d))
  f   <- fast[fast$outcome == o, ]
  f   <- f[match(seasons, f$season), ]
  cat("\n", o, "\n", sep = "")
  print(data.frame(season = seasons, refit = round(est, 3), fast = round(f$estimate, 3),
                   difference = round(est - f$estimate, 3),
                   refit_se = round(sqrt(diag(Vo)), 3), fast_se = round(f$se, 3)), row.names = FALSE)
  cat(sprintf("largest difference %.4f points; do the seasons differ? refit p = %.4f, fast p = %.4f\n",
              max(abs(est - f$estimate)), stats::pchisq(chi, k - 1, lower.tail = FALSE),
              tests$p[tests$outcome == o]))
}
