# Changelog

## 2026-11-17

I began this analysis as one R script and took it through four drafts. For this version I split the script into a repository and fixed what I found in a review of the last draft. I also added the site. Each item gives my reason for the change.

### Fixed

1. I removed the phase-shift test. In the last draft I tested the moon by sliding the lunar calendar forward a random number of days and refitting. A cosine shifted by any amount is a mix of the same cosine and sine, so the shifted estimate traces a sine wave and the test's p-value sits close to `2 * atan(|b / a|) / pi`, where `a` and `b` are the cosine and sine coefficients (0.157 by that formula, 0.168 from 1,000 shifts). The p-value follows the angle between them, whatever their size. With an effect 20 times larger I got 0.163 in place of 0.168, and I get the same joint test of cosine and sine under any shift. You can reproduce all three results with `checks/shift_test.R`. In the draft's notes I wrote that this test had exposed an inflated p-value, and I was wrong. The fake-moon test in `R/placebo.R` takes its place.

2. Team quality now changes by season. In the last draft I gave each team one offensive and one defensive level for all ten seasons. Letting them differ by season lowers AIC by about 2,600, and a form term for each team and month lowers it by about 200 more. You can print the comparison with `checks/model_structure.R`.

3. The Wolves' defense has its own term. In the last draft I had a moon term for the Wolves' scoring alone. Both teams in a game share its pace, so that single term absorbed part of the margin. My estimate for `moon_cos` was −0.16 with that term alone and −0.39 with a defense term beside it, as you can see by running `checks/defense_term.R`. I now report scoring and points allowed, and the difference between them.

4. I added season and opponent controls to the regressions on the Wolves' own games. Without them, ten years of rising league scoring sat in the residual, along with the differences in opponent strength.

5. I corrected four tip-off times. ESPN listed four games from late February 2020 about ten hours late, which put each of them on the day after the teams played it.

6. The model now has a term for the opponent's rest. I stopped counting the 2020 bubble as home games, and did the same for the NBA Cup games in Las Vegas and the games abroad. I estimate home court for each season, because it fell by about half in 2020-21. Each overtime period is a covariate in the points models.

7. The sample has 88 lunar months, and my earlier count of about 119 was wrong. I had divided the calendar span by 29.5 days, which counted the summers.

8. I fixed the description of `moon_sin`. It is −1 at first quarter and +1 at last quarter. The variable is the same, and I had swapped the signs in its description.

9. Full-moon times now come from one fixed source. In the drafts I asked the US Naval Observatory API and fell back to suncalc if the request failed, so I could get different times on two runs. suncalc's times are up to 4.4 hours off, enough to move 9 of the Wolves' 854 games across the edge of the full-moon window. Each run now reads exact times from `data/full_moons.csv`, a table I made with PyEphem.

10. I pinned the data. The script reads the season files from one commit of the source repository.

### Added

- A second sample. I kept the fourteen seasons from 2002-03 to 2015-16 out of the analysis until the main results were final. Then I ran the same model on them without changes, to re-test the estimates that had p below 0.05. You can see them in the season-by-season chart too. To use them I fixed three things in the older files. Tip-off times through 2007-08 are in the home arena's local time, and one team name has a line break in it. The rule that drops All-Star exhibitions now counts appearances per season, so it keeps teams that later moved.
- A fake-moon test. I fit the same model to cycles of 15 to 60 days that are not lunar, to check the standard errors against noise I had not modeled. I also added a known effect to the real data and confirmed that I could detect it.
- The Wolves' question for all 30 teams, and a random-slope model for whether teams differ.
- One joint test of all moon terms. I also fit six other versions of the model and tried five widths of the full-moon window.
- A breakdown by season and by team in `R/breakdown.R`. It has the Wolves' full-moon effect in each of 24 seasons and each team's effect in each of the ten main seasons. I test whether seasons differ, and whether I can predict a team's effect from its season before. For speed I reuse the fake-moon test's shortcut. I checked it against a refit of the whole model with the Wolves' effect split by season, and the two sets of estimates agree to within 0.001 points. You can run that check with `checks/season_breakdown.R`.
- An interactive chart on the page. You can pick a team and choose an outcome: its margin, or the points scored and allowed behind it. I wrote it in plain JavaScript and SVG, and `R/site.R` writes its data file from the results. I draw the charts to the width of the screen, and the team's season chart stays in view while the list of thirty teams scrolls under it.
- The season in progress. I read it from the same pinned data and show it as a record, with no model. The rule that drops All-Star exhibitions now compares each team with the typical team that season, so it works on a season a few weeks old.
- A list of contents on the page. On a wide screen it sits in a rail at the side, and on a narrow one a button takes you back to it.
- Twelve figures with a shared theme and `geom_moon()`, and the site in `docs/`. I draw each figure twice, ten inches wide for a computer screen and five for a phone, and a phone loads the narrow one.
- Tests for the moon variables, the cleaning rules, the fake-moon method and the breakdown.

### Unchanged

I kept the ten seasons and the four moon variables from the last draft. The full-moon window is still a day and a half wide.
