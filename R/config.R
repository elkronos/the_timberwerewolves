# Settings for the whole analysis. Change values here, not in the other files.

cfg <- list(
  # A season is named by the year it ends: 2017 is the 2016-17 season.
  seasons = 2017:2026,
  team    = "Minnesota Timberwolves",

  # Earlier seasons, kept apart. Nothing was fitted to them until the main
  # results were in, so they can test what the main sample suggests.
  second_sample = 2003:2016,

  # The season being played now. It is read and shown as a record. No model
  # is fitted to it, and its games stop at the date of the pinned commit below.
  current_season = 2027,

  # "Near a full moon" means tip-off within this many days of the full-moon
  # instant. 1.5 days either side covers the three nights the moon looks full.
  near_full_days = 1.5,

  # NBA schedule files: ESPN data collected by the sportsdataverse project.
  # Pinned to one commit so every run reads the same bytes.
  data_commit = "4890b4ef0f945dbfe0e5788a6b56e271ac24f61f",
  data_url    = paste0("https://raw.githubusercontent.com/sportsdataverse/",
                       "hoopR-nba-raw/%s/nba/schedules/rds/nba_schedule_%d.rds"),

  # Lunar constants, in days.
  synodic_days     = 29.530588853,   # full moon to full moon
  anomalistic_days = 27.554549878,   # perigee to perigee

  # Fake-moon test: cycle lengths (days) of the placebo moons.
  fake_period_range = c(15, 60),

  # Size of the effect injected for the positive control: points added to the
  # Wolves' score per unit of moon_cos (1.5 means a 3-point full-vs-new swing).
  injected_effect = 1.5,

  # Where the code and the site live, for links on the page.
  repo_url = "https://github.com/elkronos/the_timberwerewolves",
  site_url = "https://elkronos.github.io/the_timberwerewolves/",

  dir_raw     = "data-raw",
  dir_cache   = "cache",
  dir_results = "results",
  dir_figures = file.path("docs", "assets", "figures")
)
