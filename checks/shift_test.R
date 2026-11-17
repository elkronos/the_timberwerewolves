#!/usr/bin/env Rscript
# Why the phase-shift test was removed.
#
# Earlier drafts tested the moon by sliding the lunar calendar forward by a
# random number of days, refitting, and asking how often the shifted estimate
# beat the real one. This script reruns that test on the earlier model and
# shows three things:
#
#   1. Its p-value sits close to 2 * atan(|b / a|) / pi, where a and b are the
#      fitted cosine and sine coefficients. It depends on the angle between
#      them and hardly at all on their size.
#   2. Making the effect 20 times larger barely moves that p-value.
#   3. The joint test of cosine and sine does not move at all under shifting.
#
# Run from the repo root after `Rscript run_all.R data`:
#   Rscript checks/shift_test.R

suppressPackageStartupMessages(library(lme4))
source("R/config.R")
d  <- readRDS(file.path(cfg$dir_cache, "data.rds"))
tg <- d$tg; w <- d$w

# The earlier model: one quality level per team for all ten seasons, and a
# moon term for the Wolves' scoring only.
old_model <- lmer(points ~ is_wolves + home_court + rest + opp_rest + game_type + dow +
                    season + (1 | team) + (1 | opponent) + (1 | game_id),
                  data = tg, REML = FALSE,
                  control = lmerControl(optimizer = "bobyqa"))
r  <- residuals(old_model)
iw <- tg$is_wolves

shifted_cos <- function(phase, days) cos(2 * pi * (((phase - days / cfg$synodic_days) %% 1) - 0.5))
wolves_stat <- function(resid, moon) unname(coef(lm(resid ~ moon * iw))["moon:iw"])

set.seed(43)
shifts <- runif(1000, 0, cfg$synodic_days)
shift_p <- function(resid) {
  real <- wolves_stat(resid, tg$moon_cos)
  fake <- vapply(shifts, function(s) wolves_stat(resid, shifted_cos(tg$moon_phase, s)), numeric(1))
  (sum(abs(fake) >= abs(real)) + 1) / (length(fake) + 1)
}

ab <- coef(lm(r ~ (tg$moon_cos + tg$moon_sin) * iw))
a  <- ab[["tg$moon_cos:iw"]]; b <- ab[["tg$moon_sin:iw"]]

cat("1. The shift test's p-value tracks one angle\n")
cat(sprintf("   p from 1,000 shifts: %.3f\n", shift_p(r)))
cat(sprintf("   2 * atan(|b/a|) / pi: %.3f\n\n", 2 * atan(abs(b / a)) / pi))

cat("2. Scaling the Wolves' effect up barely moves it\n")
for (k in c(1, 5, 20)) {
  bigger <- r + (k - 1) * iw * (a * tg$moon_cos + b * tg$moon_sin)
  t_stat <- summary(lm(bigger ~ tg$moon_cos * iw))$coefficients["tg$moon_cos:iw", "t value"]
  cat(sprintf("   effect x%-2d  ordinary t = %6.2f   shift p = %.3f\n", k, t_stat, shift_p(bigger)))
}

cat("\n3. The joint cosine-and-sine F statistic is the same for every shift\n")
joint_f <- function(days) {
  ph <- (w$moon_phase - days / cfg$synodic_days) %% 1
  cs <- cos(2 * pi * (ph - 0.5)); sn <- sin(2 * pi * (ph - 0.5))
  anova(lm(points ~ home_court + rest + game_type, data = w),
        lm(points ~ home_court + rest + game_type + cs + sn, data = w))[2, "F"]
}
f <- vapply(c(0, shifts[1:200]), joint_f, numeric(1))
cat(sprintf("   real moon %.6f   smallest over 200 shifts %.6f   largest %.6f\n",
            f[1], min(f[-1]), max(f[-1])))
