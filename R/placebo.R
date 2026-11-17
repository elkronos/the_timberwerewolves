# Fake-moon test.
#
# Fit the same model with moons that do not exist: cosines whose cycles run
# from 15 to 60 days. Their estimates show how large a "moon effect" the model
# produces from noise alone, including slow swings in team form that an
# ordinary standard error ignores. The real moon is then compared with them.
#
# Refitting the mixed model for every fake moon would take an hour. Instead
# the variance components are held at the values fitted without any moon term
# and each fake moon is added by generalized least squares, which takes
# milliseconds. check_gls_against_refit() confirms the shortcut matches full
# refits.

# Pieces needed to apply the inverse covariance matrix of a fitted lmer model.
gls_setup <- function(fit) {
  X  <- lme4::getME(fit, "X")
  y  <- lme4::getME(fit, "y")
  Ut <- lme4::getME(fit, "Lambdat") %*% lme4::getME(fit, "Zt")
  # With V = sigma^2 (U U' + I), the inverse is (I - U (U'U + I)^-1 U') / sigma^2.
  chol_inner <- Matrix::Cholesky(Matrix::tcrossprod(Ut) + Matrix::Diagonal(nrow(Ut)))
  W <- function(M) {
    M <- as.matrix(M)
    M - as.matrix(Matrix::crossprod(Ut, Matrix::solve(chol_inner, Ut %*% M)))
  }
  WX <- W(X)
  list(X = X, y = y, W = W, Wy = W(y), XtWX_inv = solve(crossprod(X, WX)),
       sigma2 = stats::sigma(fit)^2)
}

# Estimates and covariance for extra columns A added to the fitted model.
gls_add <- function(g, A) {
  A    <- as.matrix(A)
  WA   <- g$W(A)
  XtWA <- crossprod(g$X, WA)
  APA  <- crossprod(A, WA) - crossprod(XtWA, g$XtWX_inv %*% XtWA)
  APy  <- crossprod(A, g$Wy) - crossprod(XtWA, g$XtWX_inv %*% crossprod(g$X, g$Wy))
  list(est = drop(solve(APA, APy)), vcov = solve(APA) * g$sigma2)
}

# The three moon columns for one moon variable m (league-wide, Wolves on
# offense, Wolves on defense), and the four numbers reported for it.
moon_columns <- function(m, tg) cbind(league = m, offense = m * tg$is_wolves,
                                      defense = m * tg$vs_wolves)

moon_terms <- function(g, m, tg) {
  fit <- gls_add(g, moon_columns(m, tg))
  est <- fit$est; V <- fit$vcov
  c(league = est[[1]], offense = est[[2]], defense = est[[3]],
    net    = est[[2]] - est[[3]],
    se_league = sqrt(V[1, 1]), se_offense = sqrt(V[2, 2]), se_defense = sqrt(V[3, 3]),
    se_net    = sqrt(V[2, 2] + V[3, 3] - 2 * V[2, 3]))
}

# Cycle lengths for the fake moons: evenly spaced in frequency, one per
# independent Fourier frequency of the sample, leaving out the two real lunar
# cycles and the once-a-year echoes of the synodic month.
fake_periods <- function(t_days, cfg) {
  step <- 1 / diff(range(t_days))
  freq <- seq(1 / cfg$fake_period_range[2], 1 / cfg$fake_period_range[1], by = step)
  real <- c(1 / cfg$synodic_days, 1 / cfg$anomalistic_days,
            1 / cfg$synodic_days + c(-1, 1) / 365.2422)
  keep <- vapply(freq, function(f) all(abs(f - real) > 1.5 * step), logical(1))
  1 / freq[keep]
}

# One row per fake moon and peak position (cosine or sine).
fake_moon_screen <- function(g, tg, cfg) {
  t_days  <- as.numeric(tg$tip_utc) / 86400
  periods <- fake_periods(t_days, cfg)
  rows <- lapply(periods, function(p) {
    rbind(data.frame(period = p, peak = "cos", t(moon_terms(g, fake_moon(t_days, p, 0), tg))),
          data.frame(period = p, peak = "sin", t(moon_terms(g, fake_moon(t_days, p, pi / 2), tg))))
  })
  do.call(rbind, rows)
}

# Compare the real moon with the fake ones for each of the four terms.
#   fake_sd      spread of the fake-moon estimates
#   model_se     the standard error the model reports for the real moon
#   p_fake       share of fake moons with an estimate at least as large
fake_moon_summary <- function(real, screen) {
  terms <- c("league", "offense", "defense", "net")
  do.call(rbind, lapply(terms, function(k) {
    fake <- screen[[k]]
    data.frame(term = k, estimate = real[[k]], model_se = real[[paste0("se_", k)]],
               fake_sd = stats::sd(fake), n_fake = length(fake),
               p_fake = (1 + sum(abs(fake) >= abs(real[[k]]))) / (1 + length(fake)))
  }))
}

# Amplitude of each fake cycle: half the distance from trough to peak.
fake_amplitudes <- function(screen) {
  cs <- screen[screen$peak == "cos", ]; sn <- screen[screen$peak == "sin", ]
  stopifnot(all(cs$period == sn$period))
  data.frame(period = cs$period,
             league = sqrt(cs$league^2 + sn$league^2),
             offense = sqrt(cs$offense^2 + sn$offense^2),
             defense = sqrt(cs$defense^2 + sn$defense^2),
             net = sqrt(cs$net^2 + sn$net^2))
}

# Same model pieces with a known effect added to the Wolves' points.
inject_effect <- function(g, tg, effect) {
  g$y  <- g$y + effect * tg$moon_cos * tg$is_wolves
  g$Wy <- g$W(g$y)
  g
}

# ---- Fake full-moon windows ---------------------------------------------------
#
# The same idea for the on/off window variable. A fake window is "on" for the
# same share of each fake cycle as the real window is of the lunar month.

fake_window <- function(t_days, period, shift, share) {
  as.numeric(cos(2 * pi * t_days / period - shift) >= cos(pi * share))
}

# Four window positions per fake cycle, a quarter cycle apart.
fake_window_screen <- function(g, tg, cfg) {
  t_days <- as.numeric(tg$tip_utc) / 86400
  share  <- 2 * cfg$near_full_days / cfg$synodic_days
  grid   <- expand.grid(shift = c(0, 0.5, 1, 1.5) * pi, period = fake_periods(t_days, cfg))
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    inside <- fake_window(t_days, grid$period[i], grid$shift[i], share)
    data.frame(period = grid$period[i], shift = grid$shift[i], t(moon_terms(g, inside, tg)))
  })
  do.call(rbind, rows)
}

# The raw version a fan would compute: the Wolves' win-loss record inside each
# fake window, with no model at all.
fake_window_records <- function(w, cfg) {
  t_days <- as.numeric(w$tip_utc) / 86400
  share  <- 2 * cfg$near_full_days / cfg$synodic_days
  # same fake cycles as the league screen, so the two are comparable
  grid   <- expand.grid(shift = c(0, 0.5, 1, 1.5) * pi, period = attr(w, "fake_periods"))
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    inside <- fake_window(t_days, grid$period[i], grid$shift[i], share) == 1
    data.frame(period = grid$period[i], shift = grid$shift[i], games = sum(inside),
               wins = sum(w$win[inside]), win_rate = mean(w$win[inside]),
               win_rate_outside = mean(w$win[!inside]))
  })
  do.call(rbind, rows)
}
