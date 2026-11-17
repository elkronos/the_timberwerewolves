# Changelog

## 2026-11-17

The analysis began as one R script that went through four drafts. This version splits it into a repository, fixes what a review of the last draft turned up, and adds the site. Each item gives the reason for the change.

### Fixed

1. The phase-shift test is gone. The last draft tested the moon by sliding the lunar calendar forward a random number of days and refitting. A cosine shifted by any amount is a mix of the same cosine and sine, so the shifted estimate traces a sine wave and the test's p-value sits close to `2 * atan(|b / a|) / pi`, where `a` and `b` are the cosine and sine coefficients (0.157 by that formula, 0.168 from 1,000 shifts). It measures the angle between them and ignores their size. An effect 20 times larger moves the p-value from 0.168 to 0.163, and the joint test of cosine and sine does not move under shifting at all. `checks/shift_test.R` reproduces all three. The draft's notes said this test had exposed an inflated p-value. It had not. The fake-moon test in `R/placebo.R` replaces it.

2. Team quality now changes by season. The last draft gave each team one offensive and one defensive level for all ten seasons. Letting them differ by season lowers AIC by about 2,600, and adding a monthly "form" term for each team lowers it by about 200 more. `checks/model_structure.R` prints the comparison.

3. The Wolves' defense has its own term. The last draft had a moon term only for the Wolves' scoring. Both teams in a game share its pace, so that single term absorbed part of the margin. Its estimate for `moon_cos` was −0.16 alone and −0.39 once a defense term sat beside it, as `checks/defense_term.R` shows. The model now reports scoring, points allowed and their difference.

4. The Wolves-only regressions control for season and opponent. Before, they had neither, so ten years of rising league scoring and every difference in opponent strength sat in the residual.

5. Four tip-off times are corrected. Four games from late February 2020 carried timestamps about ten hours late, which put each of them on the day after it was played.

6. Neutral sites, overtime and opponent rest are in the model. The 2020 bubble, the NBA Cup games in Las Vegas and the games abroad no longer count as home games. Home court is estimated for each season, because it roughly halved in 2020-21. Each overtime period is a covariate in the points models.

7. The sample holds 88 lunar months, not about 119. The earlier figure divided the calendar span by 29.5 days and counted the summers.

8. `moon_sin` is described the right way round. It is −1 at first quarter and +1 at last quarter. The variable did not change; its description had the signs swapped.

9. Full-moon times come from one fixed source. The drafts asked the US Naval Observatory API and fell back to suncalc if the request failed, so two runs could use different times. suncalc's are up to 4.4 hours off, enough to move 9 of the Wolves' 854 games across the edge of the full-moon window. Every run now reads exact times from `data/full_moons.csv`, a table made with PyEphem.

10. The data is pinned. Season files are read from one commit of the source repository.

### Added

- A second sample. The fourteen seasons from 2002-03 to 2015-16 were not used for anything until the main results were final. The same model was then run on them, unchanged, to re-test the estimates that had p below 0.05. The season-by-season chart shows them as well. Using them meant fixing three things in the older files: tip-off times through 2007-08 are stamped in the home arena's local time, one team name contains a line break, and the rule that drops All-Star exhibitions has to count appearances per season so it keeps teams that later moved.
- A fake-moon test: the same model fitted to cycles of 15 to 60 days that are not lunar, to check the standard errors against noise the model does not describe. It includes a known effect added to the real data, to show the test can detect one.
- The Wolves' question asked of all 30 teams, and a random-slope model for whether teams differ.
- One test of all moon terms together, six alternative versions of the model, and five widths of the full-moon window.
- A breakdown by season and by team in `R/breakdown.R`: the Wolves' full-moon effect in each of 24 seasons, every team in every season, a test of whether seasons differ, and a check on whether a team's effect carries from one season to the next. The breakdown reuses the fake-moon test's shortcut. `checks/season_breakdown.R` refits the whole model with the Wolves' effect split by season, and the two sets of estimates agree to within 0.001 points.
- An interactive chart on the page. Pick a team and switch between margin, points scored and points allowed. It is plain JavaScript and SVG, fed by a data file that `R/site.R` writes from the results. The charts are drawn to the width of the screen, and the selected team's chart stays in view while the list of thirty teams scrolls under it.
- The season in progress, read from the same pinned data and shown as a record. No model is fitted to it. The rule that drops All-Star exhibitions now compares each team with the typical team that season, so it works on a season that has barely begun.
- Twelve figures with a shared theme and `geom_moon()`, and the site in `docs/`.
- Tests for the moon variables, the cleaning rules, the fake-moon method and the breakdown.

### Unchanged

The ten seasons, the 1.5-day full-moon window and the four moon variables are the same as in the last draft.
