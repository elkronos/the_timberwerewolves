# The models.
#
# league model   every team-game in the league; the Wolves are compared with
#                the other 29 teams (main analysis)
# team slopes    does any team respond to the moon?
# Wolves-only    regressions on the Wolves' own games, with best-subset
#                selection over the moon variables

MOON_VARS <- c("moon_cos", "moon_sin", "near_full", "moon_dist")

# What one unit of each moon variable means, used to put all four on the same
# footing: points gained going from the low end of the variable to the high end.
moon_swing <- function(tg) c(
  moon_cos  = 2,                               # new moon to full moon
  moon_sin  = 2,                               # first quarter to last quarter
  near_full = 1,                               # outside the window to inside it
  moon_dist = diff(range(tg$moon_dist))        # closest approach to farthest
)

# ---- League model -----------------------------------------------------------
#
# points = moon effects
#        + Wolves indicators, season, home court by season, rest for both
#          teams, game type, overtime periods, day of week
#        + random intercepts:
#            season_month  slow drift in league scoring
#            team_season   how good each offense is that season
#            opp_season    how good each defense is that season
#            team_month    an offense running hot or cold for a month
#            opp_month     the same for a defense
#            game_id       the pace both teams share in one game
#
# Each moon variable enters three times: for the whole league, for the Wolves
# on offense (is_wolves) and for the Wolves on defense (vs_wolves).

LEAGUE_CONTROLS <- paste(
  "is_wolves + vs_wolves + season + season:home_court + rest + opp_rest +",
  "game_type + overtimes + dow")
LEAGUE_RANDOM <- paste(
  "(1 | season_month) + (1 | team_season) + (1 | opp_season) +",
  "(1 | team_month) + (1 | opp_month) + (1 | game_id)")

fit_league <- function(tg, moon = character(0), controls = LEAGUE_CONTROLS,
                       random = LEAGUE_RANDOM) {
  moon_part <- sprintf("%s + %s:is_wolves + %s:vs_wolves", moon, moon, moon)
  rhs <- paste(c(moon_part, controls, random), collapse = " + ")
  lme4::lmer(stats::as.formula(paste("points ~", rhs)), data = tg, REML = FALSE,
             control = lme4::lmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)))
}

# R names an interaction "a:b" or "b:a" depending on which variable it met
# first in the formula. Return whichever name the model used.
interaction_name <- function(coef_names, a, b) {
  hit <- intersect(c(paste0(a, ":", b), paste0(b, ":", a)), coef_names)
  stopifnot(length(hit) == 1)
  hit
}

# League-wide effect, Wolves offense, Wolves defense, and net (offense minus
# defense, the effect on the Wolves' point margin) for one moon variable.
wolves_terms <- function(fit, moon, swing = 1) {
  b <- lme4::fixef(fit); V <- as.matrix(stats::vcov(fit))
  o <- interaction_name(names(b), moon, "is_wolves")
  d <- interaction_name(names(b), moon, "vs_wolves")
  est <- c(league = b[[moon]], offense = b[[o]], defense = b[[d]], net = b[[o]] - b[[d]])
  se  <- c(sqrt(V[moon, moon]), sqrt(V[o, o]), sqrt(V[d, d]),
           sqrt(V[o, o] + V[d, d] - 2 * V[o, d]))
  out <- data.frame(moon = moon, term = names(est), estimate = est, se = se,
                    z = est / se, p = 2 * stats::pnorm(-abs(est / se)),
                    row.names = NULL)
  # the same numbers expressed as a low-to-high swing in points
  out$swing    <- out$estimate * swing
  out$swing_lo <- (out$estimate - stats::qnorm(0.975) * out$se) * swing
  out$swing_hi <- (out$estimate + stats::qnorm(0.975) * out$se) * swing
  # smallest true swing this test would catch 80% of the time at the 5% level
  out$detectable_swing <- (stats::qnorm(0.975) + stats::qnorm(0.80)) * out$se * swing
  out
}

# Wald chi-square test that several linear combinations of coefficients are 0.
wald_test <- function(b, V, L) {
  est  <- L %*% b
  stat <- drop(t(est) %*% solve(L %*% V %*% t(L)) %*% est)
  c(chisq = stat, df = nrow(L), p = stats::pchisq(stat, nrow(L), lower.tail = FALSE))
}

# "Is there any rhythm at the lunar period?" Tests the cosine and sine terms
# together, so the answer does not depend on where in the cycle a peak falls.
rhythm_tests <- function(fit) {
  b <- lme4::fixef(fit); V <- as.matrix(stats::vcov(fit)); nm <- names(b)
  pick <- function(weights) {            # named weights -> one contrast row
    row <- stats::setNames(numeric(length(b)), nm); row[names(weights)] <- weights; row
  }
  off <- vapply(c("moon_cos", "moon_sin"), function(m) interaction_name(nm, m, "is_wolves"), "")
  def <- vapply(c("moon_cos", "moon_sin"), function(m) interaction_name(nm, m, "vs_wolves"), "")
  L <- list(
    league  = rbind(pick(c(moon_cos = 1)), pick(c(moon_sin = 1))),
    offense = rbind(pick(stats::setNames(1, off[[1]])), pick(stats::setNames(1, off[[2]]))),
    defense = rbind(pick(stats::setNames(1, def[[1]])), pick(stats::setNames(1, def[[2]]))),
    net     = rbind(pick(stats::setNames(c(1, -1), c(off[[1]], def[[1]]))),
                    pick(stats::setNames(c(1, -1), c(off[[2]], def[[2]])))))
  out <- do.call(rbind, lapply(names(L), function(k) {
    w <- wald_test(b, V, L[[k]])
    est <- drop(L[[k]] %*% b)
    data.frame(term = k, amplitude = sqrt(sum(est^2)), chisq = w[["chisq"]],
               df = w[["df"]], p = w[["p"]])
  }))
  out
}

# One test for everything at once. With all four moon variables in the model:
# are the four league-wide terms jointly zero, and are the eight Wolves terms
# (four on offense, four on defense) jointly zero?
omnibus_tests <- function(fit) {
  b <- lme4::fixef(fit); V <- as.matrix(stats::vcov(fit)); nm <- names(b)
  rows_for <- function(cols) {
    L <- matrix(0, length(cols), length(b), dimnames = list(NULL, nm))
    L[cbind(seq_along(cols), match(cols, nm))] <- 1
    L
  }
  wolves <- c(vapply(MOON_VARS, function(m) interaction_name(nm, m, "is_wolves"), ""),
              vapply(MOON_VARS, function(m) interaction_name(nm, m, "vs_wolves"), ""))
  out <- rbind(wald_test(b, V, rows_for(MOON_VARS)), wald_test(b, V, rows_for(wolves)))
  data.frame(test = c("League-wide moon terms", "Wolves-specific moon terms"), out)
}

# Covariate effects from the model without moon terms. Their signs are known
# from basketball, so they show whether the model behaves.
covariate_table <- function(fit) {
  co <- summary(fit)$coefficients
  out <- data.frame(term = rownames(co), estimate = co[, "Estimate"],
                    se = co[, "Std. Error"], row.names = NULL)
  out$lo <- out$estimate - stats::qnorm(0.975) * out$se
  out$hi <- out$estimate + stats::qnorm(0.975) * out$se
  out
}

variance_table <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  data.frame(group = vc$grp, sd = vc$sdcor, share = vc$vcov / sum(vc$vcov))
}

# ---- Does any team respond to the moon? ---------------------------------------
#
# Random slopes let every team have its own moon effect on offense and on
# defense. If the spread of those slopes is indistinguishable from zero, no
# team stands out. The likelihood-ratio p-value is conservative because a
# spread cannot be negative.
#
#   moon   the moon variable that gets the slopes
#   by     the grouping for offense and for defense: c("team", "opponent")
#          gives each team one effect for all seasons, and
#          c("team_season", "opp_season") a separate one in every season
fit_team_slopes <- function(tg, moon = "moon_cos", by = c("team", "opponent")) {
  flat   <- fit_league_plain(tg, "", moon)
  # lme4 announces a "singular fit" when it puts a slope variance at zero.
  # Here that is a result, kept in `singular`, so the message is dropped.
  slopes <- withCallingHandlers(
    fit_league_plain(tg, sprintf("+ (0 + %s | %s) + (0 + %s | %s)", moon, by[1], moon, by[2]), moon),
    message = function(m) {
      if (grepl("singular", conditionMessage(m))) invokeRestart("muffleMessage")
    })
  lrt    <- stats::anova(flat, slopes)
  vc     <- as.data.frame(lme4::VarCorr(slopes))
  # a grouping used twice (intercept and slope) is listed as "name" and "name.1"
  slope_sd <- function(group) vc$sdcor[sub("\\.[0-9]+$", "", vc$grp) == group & vc$var1 %in% moon]
  list(fit = slopes,
       sd_offense = slope_sd(by[1]),
       sd_defense = slope_sd(by[2]),
       chisq = lrt[2, "Chisq"], p = lrt[2, "Pr(>Chisq)"],
       singular = lme4::isSingular(slopes))
}

fit_league_plain <- function(tg, extra_random, moon = "moon_cos") {
  rhs <- paste(moon, "+ season + season:home_court + rest + opp_rest +",
               "game_type + overtimes + dow +", LEAGUE_RANDOM, extra_random)
  lme4::lmer(stats::as.formula(paste("points ~", rhs)), data = tg, REML = FALSE,
             control = lme4::lmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)))
}

# The Wolves question asked of every team in turn, with the same model.
# g is the gls_setup() of the league model fitted without moon terms.
every_team <- function(g, tg, swing = 2) {
  teams <- sort(unique(tg$team))
  rows <- lapply(teams, function(tm) {
    cols <- cbind(tg$moon_cos, tg$moon_cos * (tg$team == tm),
                  tg$moon_cos * (tg$opponent == tm))
    f <- gls_add(g, cols); V <- f$vcov
    net <- f$est[2] - f$est[3]
    se  <- sqrt(V[2, 2] + V[3, 3] - 2 * V[2, 3])
    data.frame(team = tm, net_swing = net * swing, se = se * swing)
  })
  out <- do.call(rbind, rows)
  out$lo <- out$net_swing - stats::qnorm(0.975) * out$se
  out$hi <- out$net_swing + stats::qnorm(0.975) * out$se
  out$p  <- 2 * stats::pnorm(-abs(out$net_swing / out$se))
  out[order(out$net_swing), ]
}

# ---- Wolves-only regressions --------------------------------------------------
#
# One row per Wolves game. Controls: season, home court, rest for both teams,
# game type, and how many points the opponent usually scores and allows.
# Overtime periods enter only the two points models: overtime is decided by the
# margin, so it cannot be a control for margin or for winning.

WOLVES_OUTCOMES <- list(
  points     = list(label = "Points scored",  family = "gaussian", overtime = TRUE),
  opp_points = list(label = "Points allowed", family = "gaussian", overtime = TRUE),
  margin     = list(label = "Point margin",   family = "gaussian", overtime = FALSE),
  win        = list(label = "Win",            family = "binomial", overtime = FALSE))

fit_wolves <- function(w, outcome, moon = character(0)) {
  spec <- WOLVES_OUTCOMES[[outcome]]
  rhs  <- c("season", "home_court", "rest", "opp_rest", "game_type", "opp_off",
            "opp_def", if (spec$overtime) "overtimes", moon)
  f <- stats::reformulate(rhs, response = outcome)
  if (spec$family == "gaussian") stats::lm(f, data = w)
  else stats::glm(f, data = w, family = stats::binomial())
}

# All 16 subsets of the four moon variables, ranked by AIC.
wolves_subsets <- function(w, outcome) {
  subsets <- unlist(lapply(0:length(MOON_VARS), function(k)
    utils::combn(MOON_VARS, k, simplify = FALSE)), recursive = FALSE)
  out <- do.call(rbind, lapply(subsets, function(s) {
    m <- fit_wolves(w, outcome, s)
    data.frame(outcome = outcome,
               moon_vars = if (length(s)) paste(s, collapse = " + ") else "none",
               n_moon = length(s), aic = stats::AIC(m), bic = stats::BIC(m))
  }))
  out$delta_aic <- out$aic - min(out$aic)
  out[order(out$aic), ]
}

# Each moon variable on its own, plus the test of all four together.
wolves_single <- function(w, outcome, swing) {
  spec <- WOLVES_OUTCOMES[[outcome]]
  rows <- do.call(rbind, lapply(MOON_VARS, function(v) {
    co <- summary(fit_wolves(w, outcome, v))$coefficients[v, ]
    data.frame(outcome = outcome, moon = v, estimate = co[[1]], se = co[[2]],
               p = co[[4]], swing = co[[1]] * swing[[v]],
               swing_lo = (co[[1]] - stats::qnorm(0.975) * co[[2]]) * swing[[v]],
               swing_hi = (co[[1]] + stats::qnorm(0.975) * co[[2]]) * swing[[v]])
  }))
  rows$p_holm <- stats::p.adjust(rows$p, "holm")
  none <- fit_wolves(w, outcome); all4 <- fit_wolves(w, outcome, MOON_VARS)
  joint <- if (spec$family == "gaussian") stats::anova(none, all4)[2, "Pr(>F)"]
           else stats::anova(none, all4, test = "LRT")[2, "Pr(>Chi)"]
  list(single = rows, p_all_four = joint)
}
