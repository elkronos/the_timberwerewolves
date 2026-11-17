# The moon at each tip-off.
#
# Illuminated fraction, phase and distance come from the suncalc package.
# The instants of the full moons come from data/full_moons.csv, a table made
# with PyEphem (validation/make_reference.py). suncalc's own full-moon times
# can be a few hours off, and the full-moon window has a hard edge, so the
# exact times are worth having. validate_moon() measures the gap.

# Illuminated fraction (0-1), phase (0 = new, 0.5 = full) and distance (km).
moon_state <- function(t) {
  stopifnot(inherits(t, "POSIXct"))
  ill <- suncalc::getMoonIllumination(date = t, keep = c("fraction", "phase"))
  # suncalc asks for an observer location. The distance it returns is to the
  # Earth's center, so the coordinates (Minneapolis) do not change it.
  pos <- suncalc::getMoonPosition(date = t, lat = 44.98, lon = -93.27,
                                  keep = "distance")
  data.frame(fraction = ill$fraction, phase = ill$phase, dist_km = pos$distance)
}

# Exact full-moon instants between two times, read from the PyEphem table.
# If the table does not reach that far, suncalc's estimates are used instead.
full_moon_times <- function(from, to, path = file.path("data", "full_moons.csv")) {
  if (file.exists(path)) {
    all <- as.POSIXct(utils::read.csv(path)$utc, tz = "UTC")
    if (min(all) <= from && max(all) >= to) return(all[all >= from & all <= to])
  }
  message("Full-moon table missing or too short; computing full moons with suncalc.")
  full_moon_instants(from, to)
}

# suncalc's estimate of every full-moon instant between two times: the moment
# the illuminated fraction peaks in each lunar cycle.
full_moon_instants <- function(from, to) {
  grid  <- seq(from, to, by = "1 hour")
  phase <- suncalc::getMoonIllumination(date = grid, keep = "phase")$phase
  n     <- length(phase)
  # phase climbs through 0.5 at full moon (the drop from 1 to 0 is new moon)
  cross <- which(phase[-n] < 0.5 & phase[-1] >= 0.5)
  lit   <- function(s) suncalc::getMoonIllumination(
    date = as.POSIXct(s, origin = "1970-01-01", tz = "UTC"), keep = "fraction")$fraction
  peak  <- vapply(cross, function(i) {
    optimize(lit, as.numeric(grid[i]) + c(-18, 18) * 3600, maximum = TRUE, tol = 30)$maximum
  }, numeric(1))
  as.POSIXct(peak, origin = "1970-01-01", tz = "UTC")
}

# Add the moon variables to a table that has a tip_utc column.
#
#   moon_phase      position in the cycle: 0 new, 0.25 first quarter, 0.5 full
#   moon_frac       share of the disc that is lit, 0 to 1
#   moon_cos        2 * moon_frac - 1: +1 at full moon, -1 at new moon
#   moon_sin        -1 at first quarter (waxing half), +1 at last quarter (waning half)
#   days_from_full  days to the nearest full moon, negative before it
#   near_full       1 when within cfg$near_full_days of a full moon
#   moon_dist       Earth-Moon distance in thousands of km
#   lunation        index of the nearest full moon, one value per lunar cycle
add_moon_features <- function(games, cfg) {
  st    <- moon_state(games$tip_utc)
  angle <- 2 * pi * (st$phase - 0.5)        # 0 at full moon
  games$moon_phase <- st$phase
  games$moon_frac  <- st$fraction
  games$moon_cos  <- cos(angle)
  games$moon_sin  <- sin(angle)
  games$moon_dist <- st$dist_km / 1000

  full <- full_moon_times(min(games$tip_utc) - 40 * 86400,
                          max(games$tip_utc) + 40 * 86400)
  # nearest full moon to each tip-off
  t   <- as.numeric(games$tip_utc)
  f   <- as.numeric(full)
  hi  <- findInterval(t, f) + 1L            # first full moon after tip-off
  lo  <- hi - 1L
  use_hi <- (f[hi] - t) < (t - f[lo])
  idx <- ifelse(use_hi, hi, lo)
  games$days_from_full <- (t - f[idx]) / 86400
  games$near_full      <- as.integer(abs(games$days_from_full) <= cfg$near_full_days)
  games$lunation       <- idx
  attr(games, "full_moons") <- full
  games
}

# A fake moon: a cosine with any cycle length. `t_days` is time in days and
# `shift` moves the peak (0 for the cosine, pi/2 for the sine).
fake_moon <- function(t_days, period, shift = 0) cos(2 * pi * t_days / period - shift)

# The eight named phases, each one eighth of the cycle wide and centered on its
# namesake (so "Full" runs from phase 0.4375 to 0.5625).
PHASE_NAMES <- c("New", "Waxing crescent", "First quarter", "Waxing gibbous",
                 "Full", "Waning gibbous", "Last quarter", "Waning crescent")

phase_name <- function(phase) {
  factor(PHASE_NAMES[floor(((phase + 1 / 16) %% 1) * 8) + 1], levels = PHASE_NAMES)
}

# Compare suncalc with PyEphem over the sample years: full-moon instants
# against data/full_moons.csv, fraction and distance against a daily table.
validate_moon <- function() {
  daily    <- utils::read.csv(file.path("validation", "pyephem_daily.csv"))
  span     <- range(as.POSIXct(daily$utc, tz = "UTC"))
  ref_full <- full_moon_times(span[1], span[2])
  ours     <- full_moon_instants(min(ref_full) - 5 * 86400, max(ref_full) + 5 * 86400)
  stopifnot(length(ours) == length(ref_full))
  hours_off <- as.numeric(difftime(ours, ref_full, units = "hours"))
  st <- moon_state(as.POSIXct(daily$utc, tz = "UTC"))
  list(full_moons_checked = length(ref_full),
       max_hours_off      = max(abs(hours_off)),
       mean_hours_off     = mean(hours_off),
       fraction_cor       = stats::cor(st$fraction, daily$fraction),
       fraction_max_diff  = max(abs(st$fraction - daily$fraction)),
       distance_cor       = stats::cor(st$dist_km, daily$dist_km))
}
