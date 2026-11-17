# Download the NBA schedule files and turn them into analysis tables.
#
# read_games()   one row per completed game
# team_games()   two rows per game, one from each team's point of view
# wolves_games() one row per Timberwolves game, with opponent strength

# Download one file per season into data-raw/. Files already there are reused.
download_schedules <- function(cfg, seasons = cfg$seasons) {
  dir.create(cfg$dir_raw, showWarnings = FALSE, recursive = TRUE)
  paths <- file.path(cfg$dir_raw, sprintf("nba_schedule_%d.rds", seasons))
  for (i in seq_along(paths)) {
    if (file.exists(paths[i])) next
    url <- sprintf(cfg$data_url, cfg$data_commit, seasons[i])
    message("Downloading ", basename(paths[i]))
    utils::download.file(url, paths[i], mode = "wb", quiet = TRUE)
  }
  paths
}

season_label <- function(end_year) sprintf("%d-%02d", end_year - 1L, end_year %% 100L)

# Time zone of each team's home arena, for the seasons whose tip-off times
# need it (2002-03 to 2007-08). Teams not listed play on Eastern time.
home_time_zone <- function(team) {
  zones <- c(
    "Chicago Bulls" = "America/Chicago", "Dallas Mavericks" = "America/Chicago",
    "Houston Rockets" = "America/Chicago", "Memphis Grizzlies" = "America/Chicago",
    "Milwaukee Bucks" = "America/Chicago", "Minnesota Timberwolves" = "America/Chicago",
    "New Orleans Hornets" = "America/Chicago", "NO/Oklahoma City Hornets" = "America/Chicago",
    "San Antonio Spurs" = "America/Chicago",
    "Denver Nuggets" = "America/Denver", "Utah Jazz" = "America/Denver",
    "Phoenix Suns" = "America/Phoenix",
    "Golden State Warriors" = "America/Los_Angeles", "LA Clippers" = "America/Los_Angeles",
    "Los Angeles Lakers" = "America/Los_Angeles", "Portland Trail Blazers" = "America/Los_Angeles",
    "Sacramento Kings" = "America/Los_Angeles", "Seattle SuperSonics" = "America/Los_Angeles",
    # Indiana kept standard time all year until 2006
    "Indiana Pacers" = "America/Indiana/Indianapolis")
  zone <- unname(zones[team])
  zone[is.na(zone)] <- "America/New_York"
  zone
}

# The teams that are franchises, as "name season" strings.
#
# All-Star exhibitions use one-off team names ("Team LeBron", "World") that
# appear at most six times a season, while a franchise plays at least 64 games
# in any finished season here. A team counts as a franchise if it appears at
# least a quarter as often as the typical team that season, which also works
# for a season still being played. Counting by season keeps relocated teams,
# whose old name lasts only a year or two.
franchises <- function(home, away, season) {
  appearances <- table(c(paste(home, season), paste(away, season)))
  season_of   <- sub("^.* ", "", names(appearances))
  typical     <- tapply(as.numeric(appearances), season_of, stats::median)
  names(appearances)[as.numeric(appearances) >= typical[season_of] / 4]
}

read_games <- function(cfg, seasons = cfg$seasons) {
  keep <- c("game_id", "date", "season", "season_type", "neutral_site",
            "home_display_name", "away_display_name", "home_score", "away_score",
            "status_type_completed", "status_period",
            "venue_full_name", "venue_address_city")
  raw <- do.call(rbind, lapply(download_schedules(cfg, seasons),
                               function(p) as.data.frame(readRDS(p))[, keep]))
  raw <- raw[raw$status_type_completed, ]
  # one team name in the 2005-06 file has a line break inside it
  raw$home_display_name <- gsub("\\s+", " ", trimws(raw$home_display_name))
  raw$away_display_name <- gsub("\\s+", " ", trimws(raw$away_display_name))

  completed <- nrow(raw)
  real <- franchises(raw$home_display_name, raw$away_display_name, raw$season)
  raw <- raw[paste(raw$home_display_name, raw$season) %in% real &
             paste(raw$away_display_name, raw$season) %in% real, ]
  exhibitions <- completed - nrow(raw)

  # Tip-off times are UTC instants such as "2025-10-23T02:00Z".
  tip <- as.POSIXct(raw$date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
  stopifnot(!anyNA(tip))

  # Through 2007-08 the source stamps each game with the clock time at the home
  # arena but labels it Eastern, so a 7:30 p.m. tip in Los Angeles reads as
  # 7:30 p.m. Eastern, three hours early. Read those clock times again in the
  # home team's own time zone. From 2008-09 on the times are right.
  local_clock <- as.integer(raw$season) <= 2008
  if (any(local_clock)) {
    zone  <- home_time_zone(raw$home_display_name[local_clock])
    clock <- format(tip[local_clock], "%Y-%m-%d %H:%M", tz = "America/New_York")
    fixed <- tip[local_clock]
    for (z in unique(zone)) {
      fixed[zone == z] <- as.POSIXct(clock[zone == z], format = "%Y-%m-%d %H:%M", tz = z)
    }
    tip[local_clock] <- fixed
  }

  # Four games from late February 2020 carry timestamps about ten hours late,
  # which puts tip-off before 11 a.m. Eastern. No other game in the sample
  # starts before noon Eastern. Moving these four back 12 hours restores the
  # calendar date each game was played on.
  eastern_hour <- as.integer(format(tip, "%H", tz = "America/New_York"))
  time_fixed   <- eastern_hour < 11
  tip[time_fixed] <- tip[time_fixed] - 12 * 3600

  # The NBA lists a game under its US Eastern calendar date.
  game_date <- as.Date(format(tip, "%Y-%m-%d", tz = "America/New_York"))

  # Home court only counts in the home team's own building. The 2020 Orlando
  # bubble, the NBA Cup games in Las Vegas and the international games have a
  # listed home team but no home crowd. The source flags most of these; the
  # bubble and a few Las Vegas and Mexico City games are not flagged.
  # (%in% treats a missing venue as "not one of these")
  neutral <- raw$neutral_site %in% TRUE |
    raw$venue_full_name %in% "ESPN Wide World of Sports Complex" |
    raw$venue_address_city %in% c("Las Vegas", "Mexico City")

  games <- data.frame(
    game_id   = as.character(raw$game_id),
    tip_utc   = tip,
    game_date = game_date,
    season    = season_label(as.integer(raw$season)),
    game_type = factor(c("2" = "Regular season", "5" = "Play-in",
                         "3" = "Playoffs")[as.character(raw$season_type)],
                       levels = c("Regular season", "Play-in", "Playoffs")),
    home      = raw$home_display_name,
    away      = raw$away_display_name,
    home_pts  = as.integer(raw$home_score),
    away_pts  = as.integer(raw$away_score),
    neutral   = neutral,
    # Each overtime period adds five minutes and roughly ten points a team.
    overtimes = pmax(as.integer(raw$status_period) - 4L, 0L),
    time_fixed = time_fixed,
    stringsAsFactors = FALSE
  )
  stopifnot(!anyNA(games), !anyDuplicated(games$game_id),
            all(games$home_pts != games$away_pts))

  games <- games[order(games$tip_utc, games$game_id), ]
  # The calendar month groups games for slow drift in league scoring. Days
  # since the season's first game are used only by checks/defense_term.R,
  # which refits the earlier draft's model.
  games$day_of_season <- ave(as.numeric(games$game_date), games$season,
                             FUN = function(d) d - min(d))
  games$season_month  <- paste(games$season, format(games$game_date, "%m"))
  rownames(games) <- NULL
  attr(games, "exhibitions_dropped") <- exhibitions
  games
}

# Rest days before each game: 0 is the second night of a back-to-back.
# Anything over three days, and a team's first game of a season, counts as 3.
rest_days <- function(team, season, game_date) {
  o <- order(team, season, game_date)
  gap <- rep(NA_real_, length(o))
  prev_same <- c(FALSE, team[o][-1] == team[o][-length(o)] &
                        season[o][-1] == season[o][-length(o)])
  d <- as.numeric(game_date[o])
  gap[o] <- ifelse(prev_same, d - c(NA, d[-length(d)]) - 1, NA)
  stopifnot(all(gap >= 0, na.rm = TRUE))
  gap[is.na(gap) | gap > 3] <- 3
  factor(gap, levels = 0:3, labels = c("0", "1", "2", "3+"))
}

team_games <- function(games, cfg) {
  shared <- games[, setdiff(names(games), c("home", "away", "home_pts", "away_pts",
                                            "neutral", "time_fixed"))]
  side <- function(team, opponent, points, opp_points, is_home) {
    cbind(shared, data.frame(
      team = team, opponent = opponent, points = points, opp_points = opp_points,
      home_court = as.integer(is_home & !games$neutral),
      stringsAsFactors = FALSE))
  }
  tg <- rbind(side(games$home, games$away, games$home_pts, games$away_pts, TRUE),
              side(games$away, games$home, games$away_pts, games$home_pts, FALSE))

  tg$rest <- rest_days(tg$team, tg$season, tg$game_date)
  # The opponent's rest is the rest value on the other row of the same game.
  other <- match(paste(tg$game_id, tg$opponent), paste(tg$game_id, tg$team))
  tg$opp_rest <- tg$rest[other]

  tg$is_wolves <- as.integer(tg$team == cfg$team)       # Wolves on offense
  tg$vs_wolves <- as.integer(tg$opponent == cfg$team)   # Wolves on defense

  # Team quality changes from season to season, so offense and defense levels
  # are grouped by team-season, and "form" by team-season-month.
  tg$team_season <- paste(tg$team, tg$season)
  tg$opp_season  <- paste(tg$opponent, tg$season)
  tg$team_month  <- paste(tg$team, tg$season_month)
  tg$opp_month   <- paste(tg$opponent, tg$season_month)
  tg$season      <- factor(tg$season)
  tg$dow         <- factor(format(tg$game_date, "%u"), levels = as.character(1:7),
                           labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  tg <- tg[order(tg$tip_utc, tg$game_id, -tg$home_court, tg$team), ]
  rownames(tg) <- NULL
  tg
}

wolves_games <- function(tg, cfg) {
  # Opponent strength: each team-season's average points scored and allowed in
  # games that did not involve the Wolves.
  others <- tg[tg$is_wolves == 0 & tg$vs_wolves == 0, ]
  off <- tapply(others$points,     others$team_season, mean)
  def <- tapply(others$opp_points, others$team_season, mean)

  w <- tg[tg$is_wolves == 1, ]
  w$opp_off <- as.numeric(off[w$opp_season])   # what this opponent usually scores
  w$opp_def <- as.numeric(def[w$opp_season])   # what this opponent usually allows
  w$margin  <- w$points - w$opp_points
  w$win     <- as.integer(w$margin > 0)
  stopifnot(!anyNA(w$opp_off), !anyNA(w$opp_def))
  rownames(w) <- NULL
  w
}

# The three tables for a set of seasons.
build_tables <- function(cfg, seasons = cfg$seasons) {
  games <- add_moon_features(read_games(cfg, seasons), cfg)
  tg    <- team_games(games, cfg)
  list(games = games, tg = tg, w = wolves_games(tg, cfg))
}
