"""Lunar reference values from PyEphem.

Writes two files:
  data/full_moons.csv             every full-moon instant from 2001 to 2036 (UTC).
                                  The analysis reads its full-moon times from here.
  validation/pyephem_daily.csv    illuminated fraction and Earth-Moon distance at
                                  00:00 UTC each day of the sample, used to check
                                  the suncalc R package.

Run from the repo root:  pip install ephem && python validation/make_reference.py
"""
import csv
import datetime as dt
import os

import ephem

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

moons = []
d = ephem.Date("2001/1/1")
end = ephem.Date("2036/1/1")
while True:
    d = ephem.next_full_moon(d)
    if d > end:
        break
    moons.append(ephem.Date(d).datetime())
    d = ephem.Date(d + 1)

with open(os.path.join(root, "data", "full_moons.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["utc"])
    for t in moons:
        w.writerow([t.strftime("%Y-%m-%d %H:%M:%S")])

moon = ephem.Moon()
start = dt.datetime(2016, 10, 1)
with open(os.path.join(root, "validation", "pyephem_daily.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["utc", "fraction", "dist_km"])
    for i in range(3560):
        t = start + dt.timedelta(days=i)
        moon.compute(ephem.Date(t))
        w.writerow([t.strftime("%Y-%m-%d %H:%M:%S"), round(moon.phase / 100.0, 6),
                    round(moon.earth_distance * 149597870.7, 1)])

print(len(moons), "full moons;", 3560, "daily rows; ephem", ephem.__version__)
