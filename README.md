# FactoryDaysHours

Fast, vectorised business-hours and business-days arithmetic for R.

Give it a start time, an end time, a daily working window (e.g. 08:00 to
17:00) and a vector of company holidays, and it returns the working time that
elapsed between them. Weekends are skipped. Everything is computed in closed
form, so there is **no loop over days**: a million date pairs take about one
second, whether they are an hour apart or a decade apart.

## Installation

```r
# install.packages("remotes")
remotes::install_github("jessegreever/FactoryDaysHours")
```

The package has no dependencies beyond base R (>= 4.0).

## Usage

```r
library(FactoryDaysHours)

holidays <- as.Date(c("2026-11-26", "2026-11-27", "2026-12-25", "2027-01-01"))

start <- as.POSIXct("2026-12-24 15:00", tz = "America/New_York")
end   <- as.POSIXct("2026-12-28 10:00", tz = "America/New_York")

business_hours(start, end, "08:00", "17:00", holidays = holidays)
#> [1] 4      # Thu 15:00-17:00 + Mon 08:00-10:00; Fri 25th and the weekend are skipped

business_days(start, end, "08:00", "17:00", holidays = holidays)
#> [1] 0.4444444   # 4 h out of a 9 h day

# Go the other way: when will 4 working hours have elapsed?
add_business_hours(start, 4, "08:00", "17:00", holidays = holidays)
#> [1] "2026-12-28 10:00:00 EST"

is_business_day(as.Date("2026-12-24") + 0:4, holidays = holidays)
#> [1]  TRUE FALSE FALSE FALSE  TRUE
```

All functions are vectorised over `start`/`end` (one may be length 1), so
they drop straight into `dplyr::mutate()` or `data.table` on a whole column:

```r
df |>
  dplyr::mutate(lead_hours = business_hours(ordered_at, shipped_at,
                                            "06:00", "18:00",
                                            holidays = holidays))
```

## Functions

| Function | Returns |
|---|---|
| `business_hours(start, end, ...)` | working hours between two timestamps |
| `business_days(start, end, ...)` | working days (hours / length of the working day) |
| `business_seconds(start, end, ...)` | working seconds |
| `add_business_hours(start, hours, ...)` | the timestamp `hours` of working time after `start` |
| `is_business_day(x, ...)` | `TRUE` for dates that are neither weekend nor holiday |

Shared arguments:

* `work_start`, `work_end`: `"HH:MM"`, `"HH:MM:SS"`, a numeric number of
  hours (`8.5`), an `hms`/`difftime`, or a `POSIXct` whose clock time is used.
  Default `"08:00"` to `"17:00"`.
* `holidays`: `Date`, `"YYYY-MM-DD"` character, or `POSIXct` vector. Weekend
  dates and duplicates are ignored. Default `NULL`.
* `weekend`: days never worked, default Saturday and Sunday. Accepts day
  numbers (`0` = Sunday ... `6` = Saturday) or names; `NULL` for a seven-day
  operation.
* `tz`: time zone for the working window and holidays. Defaults to the
  `tzone` of `start`, then the session time zone.

## Semantics

* Times outside the working window are snapped to the nearest edge of the
  window. A `start` of 03:00 behaves like 08:00; an `end` of 23:00 behaves
  like 17:00.
* `end < start` gives a negative result, so `business_hours(a, b) == -business_hours(b, a)`.
* A working day always contributes exactly `work_end - work_start`, including
  on daylight-saving transition days: an 08:00 to 17:00 shift is nine hours
  on the wall clock regardless of the clock change.
* `NA` inputs give `NA` outputs.
* `add_business_hours()` snaps a `start` outside the window forward to the
  next working period, and reports a result landing exactly on the end of a
  working day as the start of the next one (the same amount of elapsed
  working time).
* Overnight shifts (`work_end` earlier than `work_start`) are not supported.

## How it is fast

Each timestamp is mapped to a cumulative count of working seconds since a
fixed reference. The elapsed working time is simply the difference of two
such counts, so the cost does not depend on how far apart the timestamps are.

* The number of weekdays before a date is integer division by seven plus a
  seven-entry lookup table.
* The holiday correction is one `findInterval()` on the sorted holiday vector
  (binary search, vectorised in C).
* Local dates and seconds-of-day come from a single `as.POSIXlt()` call, with
  the day number derived arithmetically from year and day-of-year rather than
  via the much slower `as.Date.POSIXlt()`.
* `add_business_hours()` inverts the same cumulative function; holidays are
  skipped with a vectorised fixed-point iteration that advances over at least
  one holiday per pass, so it converges in a handful of passes regardless of
  the number of days spanned.

Indicative timings (single core, DST time zone, 120 holidays):

| rows | `business_hours()` | `add_business_hours()` |
|---|---|---|
| 10,000 | 0.01 s | 0.03 s |
| 100,000 | 0.08 s | 0.2 s |
| 1,000,000 | 1.0 s | 1.9 s |

## Testing

```r
devtools::test()
```

The suite cross-checks the closed-form results against a minute-by-minute
brute-force reference over random intervals, holidays, working windows and
custom weekends, in both UTC and a daylight-saving time zone.

## License

MIT
