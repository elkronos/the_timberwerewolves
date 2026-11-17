#!/usr/bin/env Rscript
# How the league model's structure was chosen.
#
# Fits six versions, from the earlier draft's structure to the final one, and
# prints for each: AIC, the Wolves' margin estimate for moon_cos, and the
# ratio of the fake-moon spread to the model's standard error. A ratio near 1
# means the standard error is the right size.
#
# Run from the repo root after `Rscript run_all.R data` (takes a few minutes):
#   Rscript checks/model_structure.R

suppressPackageStartupMessages({library(lme4); library(Matrix)})
options(width = 150)
for (f in c("config", "moon", "models", "placebo", "analysis")) source(file.path("R", paste0(f, ".R")))
tg <- readRDS(file.path(cfg$dir_cache, "data.rds"))$tg

plain   <- "is_wolves + vs_wolves + season + home_court + rest + game_type + dow"
random  <- c(
  "Team quality fixed for ten seasons" = "(1 | team) + (1 | opponent) + (1 | game_id)",
  "Team quality by season"             = "(1 | team_season) + (1 | opp_season) + (1 | game_id)",
  "  + monthly form"                   = paste("(1 | team_season) + (1 | opp_season) +",
                                               "(1 | team_month) + (1 | opp_month) + (1 | game_id)"))
versions <- c(
  lapply(names(random), function(n) list(n, plain, random[[n]])),
  list(list("  + home court by season, opponent rest, overtime", LEAGUE_CONTROLS, random[[3]]),
       list("  + league drift by month (final model)", LEAGUE_CONTROLS, LEAGUE_RANDOM),
       list("  + a random effect for each night", LEAGUE_CONTROLS,
            paste(LEAGUE_RANDOM, "+ (1 | game_date)"))))
tg$game_date <- as.character(tg$game_date)

rows <- lapply(versions, function(v) {
  tick(v[[1]])
  with_moon <- fit_league(tg, "moon_cos", v[[2]], v[[3]])
  net   <- wolves_terms(with_moon, "moon_cos")
  net   <- net[net$term == "net", ]
  g     <- gls_setup(fit_league(tg, character(0), v[[2]], v[[3]]))
  fake  <- fake_moon_summary(moon_terms(g, tg$moon_cos, tg), fake_moon_screen(g, tg, cfg))
  data.frame(model = v[[1]], aic = round(AIC(with_moon), 1),
             wolves_margin = round(net$estimate, 3), se = round(net$se, 3),
             fake_spread_over_se = round(fake$fake_sd[fake$term == "net"] / net$se, 2))
})
print(do.call(rbind, rows), row.names = FALSE, right = FALSE)
