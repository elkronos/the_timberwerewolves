#!/usr/bin/env Rscript
# Why the Wolves' defense needs its own moon term.
#
# Fits the earlier draft's model twice: with a moon term for the Wolves'
# scoring only, then with one for their points allowed beside it. Both teams
# in a game share its pace, so the scoring term changes when the second one
# arrives. The same pair of fits under the final structure follows.
#
# Run from the repo root after `Rscript run_all.R data` (takes about a minute):
#   Rscript checks/defense_term.R

suppressPackageStartupMessages({library(lme4); library(Matrix)})
for (f in c("config", "moon", "models")) source(file.path("R", paste0(f, ".R")))
tg <- readRDS(file.path(cfg$dir_cache, "data.rds"))$tg
tg$opp_b2b <- as.integer(tg$opp_rest == "0")

draft <- paste(
  "home_court + rest + opp_b2b + game_type + dow + season + splines::ns(day_of_season, 3) +",
  "(1 | team) + (1 | opponent) + (1 | game_id)")
final <- paste(
  "season + season:home_court + rest + opp_rest + game_type + overtimes + dow +", LEAGUE_RANDOM)

scoring_only <- "moon_cos * is_wolves"
with_defense <- "moon_cos * is_wolves + moon_cos * vs_wolves"

fit <- function(moon, rest_of_model) lme4::lmer(
  stats::as.formula(paste("points ~", moon, "+", rest_of_model)), data = tg, REML = FALSE,
  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

scoring_term <- function(moon, rest_of_model) {
  m <- fit(moon, rest_of_model)
  c(estimate = lme4::fixef(m)[["moon_cos:is_wolves"]],
    se = sqrt(as.matrix(stats::vcov(m))["moon_cos:is_wolves", "moon_cos:is_wolves"]))
}

rows <- rbind(
  "Draft structure, scoring term only"        = scoring_term(scoring_only, draft),
  "Draft structure, with a defense term"      = scoring_term(with_defense, draft),
  "Final structure, scoring term only"        = scoring_term(scoring_only, final),
  "Final structure, with a defense term"      = scoring_term(with_defense, final))
cat("The Wolves' scoring term for moon_cos, in points per unit of moon_cos\n")
print(round(rows, 3))
