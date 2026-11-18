# The Timberwerewolves

Do the Minnesota Timberwolves play better under a full moon? I tested ten NBA seasons in R to find out.

You can read the walkthrough, with its twelve figures and a chart for exploring all 30 teams, at <https://elkronos.github.io/the_timberwerewolves/>.

![854 Timberwolves games by days from the nearest full moon, with wins above the line and losses below.](docs/assets/figures/games-by-moon.png)

## Findings

From 2016-17 through 2025-26 the Wolves are 55–36 in games within a day and a half of a full moon and 373–390 in the rest.

I fit a model to compare them with the other 29 teams, with terms for the opponent and the schedule. I estimate their full-moon edge at 1.7 points of margin, with a 95% interval from −1.2 to +4.6. I see no moon effect on their scoring. Their opponents score 2.9 fewer points inside the full-moon window (p = 0.018). That is one of two results under 0.05 among sixteen estimates. After I correct for the number of tests, its p-value is 0.073.

I checked the error bars with 166 fake moons, cycles of made-up lengths. The error bars on the Wolves' margin are the right size, and the ones on their defense are too narrow. In about 2.9% of fake full-moon windows the Wolves did as well as their real record.

I kept the fourteen seasons before those ten out of the main analysis as a second sample. There the Wolves went 45–70 near a full moon and 405–636 otherwise, and both flagged estimates have an interval that includes zero.

The Wolves built the record in two seasons. They went 10–1 inside the window in 2021-22 and again in 2024-25, and 35–34 in the other eight. By season and by team, the effect varies no more than I would expect from luck.

I cannot rule out an edge of a point or so. To confirm one at the usual standard, I would need about 57 seasons of games.

## Run it

```sh
git clone https://github.com/elkronos/the_timberwerewolves
cd the_timberwerewolves
Rscript run_all.R
```

You need R and six packages. I tested on R 4.3 and R 4.5.

```r
install.packages(c("lme4", "Matrix", "suncalc", "ggplot2", "ragg", "jsonlite"))
```

The run takes six to ten minutes. The script downloads 25 season files into `data-raw/` and fits the models, which fills `results/` with tables. It then draws the figures and rebuilds the site in `docs/`. To rerun one step, name it, as in `Rscript run_all.R figures`. The steps are `data`, `models`, `figures` and `site`.

`Rscript tests/run_tests.R` checks the building blocks. You need the testthat package for it.

## Files

| Path | Contents |
|---|---|
| `run_all.R` | The script that runs all four steps |
| `R/config.R` | Seasons for both samples, the season in progress, team, window width and the pinned data commit |
| `R/data.R` | Download and cleaning, one row per game and per team-game |
| `R/moon.R` | The moon variables, computed with suncalc |
| `R/models.R` | The mixed models, and the regressions on the Wolves' own games |
| `R/placebo.R` | The fake-moon test |
| `R/breakdown.R` | Each team's effect in each season, and records inside the window |
| `R/analysis.R` | Runs the models and writes `results/` |
| `R/theme_moon.R` | Colors, font, the ggplot theme and `geom_moon()` |
| `R/figures.R` | The twelve figures, each in a wide layout and a narrow one for phones |
| `R/site.R` | Fills the page template with numbers from the results and writes the data for the interactive chart |
| `site/` | Page template, stylesheet, the three scripts and the figures' font |
| `checks/` | Four scripts behind claims in the changelog |
| `data/full_moons.csv` | Exact full-moon times for 2001 to 2036, made with PyEphem |
| `validation/` | The script that makes that table, and PyEphem values for each day that I used to check suncalc |
| `results/` | The tables and numbers behind the page, as CSV and JSON |
| `docs/` | The built site, served by GitHub Pages |

## geom_moon

I draw the moons in the figures with a small ggplot2 geom. It takes a position and a `phase` between 0 and 1 (0 is new, 0.5 is full). Its size is in millimetres, so the disc stays round on any axis.

```r
library(ggplot2)
source("R/theme_moon.R")

month <- data.frame(day = seq(0, 28, by = 4))
ggplot(month, aes(day, 0)) +
  geom_moon(aes(x = day, y = 0, phase = day / 29.53), size = 12) +
  theme_moon()
```

## Sources

- I got the scores from ESPN through the [sportsdataverse](https://github.com/sportsdataverse/hoopR-nba-raw) project, and I pinned the download to commit `4890b4e`.
- I computed the moon's position with the [suncalc](https://cran.r-project.org/package=suncalc) R package. The exact full-moon times in `data/full_moons.csv` come from [PyEphem](https://rhodesmill.org/pyephem/), and I checked suncalc against PyEphem too.
- I fit the models with [lme4](https://cran.r-project.org/package=lme4) and drew the figures with [ggplot2](https://ggplot2.tidyverse.org).
- The typefaces are IM Fell English and Archivo. Both are under the SIL Open Font License, and you can read the license texts in `site/fonts/`.

This is a fan project. I have no affiliation with the Minnesota Timberwolves or the NBA.

## License

The code is under the [MIT license](LICENSE).
