tz <- "America/New_York"
P <- function(x) as.POSIXct(x, tz = tz)

test_that("basic same-day interval", {
  expect_equal(business_hours(P("2026-03-02 09:00"), P("2026-03-02 12:30")), 3.5)
  expect_equal(business_seconds(P("2026-03-02 09:00"), P("2026-03-02 09:00:30")), 30)
})

test_that("clamps to the working window", {
  # Monday 2026-03-02, window 08:00-17:00 (9h)
  expect_equal(business_hours(P("2026-03-02 03:00"), P("2026-03-02 23:00")), 9)
  expect_equal(business_hours(P("2026-03-02 17:00"), P("2026-03-02 23:00")), 0)
  expect_equal(business_hours(P("2026-03-02 00:00"), P("2026-03-02 08:00")), 0)
})

test_that("spans weekends", {
  # Fri 2026-03-06 16:00 -> Mon 2026-03-09 09:00 = 1h + 1h
  expect_equal(business_hours(P("2026-03-06 16:00"), P("2026-03-09 09:00")), 2)
  # whole weekend contributes nothing
  expect_equal(business_hours(P("2026-03-07 00:00"), P("2026-03-09 00:00")), 0)
})

test_that("holidays are skipped and weekend holidays are harmless", {
  hol <- as.Date(c("2026-12-25", "2026-12-26", "2027-01-01"))  # 26th is a Saturday
  s <- P("2026-12-24 15:00"); e <- P("2026-12-28 10:00")
  expect_equal(business_hours(s, e, "08:00", "17:00", holidays = hol), 4)
  expect_equal(business_hours(s, e, "08:00", "17:00"), 13)
  # character and POSIXct holiday inputs behave the same as Date
  expect_equal(business_hours(s, e, holidays = c("2026-12-25", "2027-01-01")), 4)
  expect_equal(business_hours(s, e, holidays = P(c("2026-12-25 13:00"))), 4)
  expect_equal(business_days(s, e, holidays = hol), 4 / 9)
})

test_that("reversed intervals are negative and antisymmetric", {
  s <- P("2026-03-02 09:00"); e <- P("2026-03-12 11:00")
  expect_equal(business_hours(e, s), -business_hours(s, e))
})

test_that("vectorisation and recycling", {
  s <- P("2026-03-02 09:00") + 3600 * 0:3
  e <- P("2026-03-02 12:00")
  expect_equal(business_hours(s, e), c(3, 2, 1, 0))
  expect_equal(business_hours(e, s), -c(3, 2, 1, 0))
  expect_error(business_hours(s, e + 0:1), "same length")
})

test_that("NA propagates", {
  s <- P(c("2026-03-02 09:00", NA))
  out <- business_hours(s, P("2026-03-02 12:00"))
  expect_equal(out, c(3, NA))
})

test_that("Date and character inputs", {
  expect_equal(business_hours(as.Date("2026-03-02"), as.Date("2026-03-07"), tz = "UTC"), 45)
  expect_equal(business_hours("2026-03-02 09:00", "2026-03-02 10:00", tz = "UTC"), 1)
})

test_that("work window specifications", {
  s <- P("2026-03-02 00:00"); e <- P("2026-03-03 00:00")
  expect_equal(business_hours(s, e, 8.5, 17), 8.5)
  expect_equal(business_hours(s, e, "08:30", "17:00:00"), 8.5)
  expect_equal(business_hours(s, e, as.difftime(8.5, units = "hours"), as.difftime(17, units = "hours")), 8.5)
  expect_equal(business_hours(s, e, P("2000-01-01 08:30"), P("2000-01-01 17:00")), 8.5)
  expect_equal(business_hours(s, e, "00:00", "24:00"), 24)
  expect_error(business_hours(s, e, "17:00", "08:00"), "later than")
  expect_error(business_hours(s, e, "25:00", "26:00"), "between")
  expect_error(business_hours(s, e, "abc", "17:00"), "HH:MM")
})

test_that("custom weekends", {
  s <- P("2026-03-02 00:00"); e <- P("2026-03-09 00:00")  # one full week
  expect_equal(business_hours(s, e, weekend = NULL), 63)
  expect_equal(business_hours(s, e, weekend = c("Friday", "Saturday")), 45)
  expect_equal(business_hours(s, e, weekend = 0), 54)
  expect_error(business_hours(s, e, weekend = 0:6), "every day")
})

test_that("DST days count the wall-clock window", {
  # 2026-03-08 is a Sunday; 2026-03-09 Monday follows spring-forward.
  # 2026-11-01 (Sunday) is fall-back; Monday 11-02 is normal.
  expect_equal(business_hours(P("2026-03-09 00:00"), P("2026-03-10 00:00")), 9)
  expect_equal(business_hours(P("2026-11-02 00:00"), P("2026-11-03 00:00")), 9)
  # Across a DST change, the window on the change day itself is still 9h
  # when the weekend is not excluded.
  expect_equal(business_hours(P("2026-03-08 00:00"), P("2026-03-09 00:00"), weekend = NULL), 9)
  expect_equal(business_hours(P("2026-03-08 01:00"), P("2026-03-08 09:30"), "01:00", "10:00", weekend = NULL), 8.5)
})

test_that("tz argument overrides tzone attribute", {
  s <- as.POSIXct("2026-03-02 12:00", tz = "UTC")
  e <- s + 3600
  # In UTC, 12:00-13:00 is inside 08:00-17:00
  expect_equal(business_hours(s, e), 1)
  # In Tokyo the same instants are 21:00-22:00: outside the window
  expect_equal(business_hours(s, e, tz = "Asia/Tokyo"), 0)
})

test_that("dates before 1970 work", {
  s <- as.POSIXct("1965-06-01 09:00", tz = "UTC"); e <- as.POSIXct("1965-06-08 09:00", tz = "UTC")
  expect_equal(business_hours(s, e), 45)
  expect_equal(business_hours(s, e, holidays = "1965-06-03"), 36)
})

test_that("matches the brute-force reference on random intervals", {
  set.seed(42)
  base <- as.POSIXct("2025-01-01", tz = "UTC")
  hol  <- as.Date("2025-01-01") + sort(sample(0:730, 40))
  n <- 60
  s <- base + round(runif(n, 0, 600 * 86400) / 60) * 60
  e <- s + round(runif(n, -10 * 86400, 20 * 86400) / 60) * 60
  windows <- list(c("08:00", "17:00"), c("06:30", "15:00"), c("00:00", "24:00"), c("22:00", "23:45"))
  for (w in windows) {
    got <- business_seconds(s, e, w[1], w[2], holidays = hol, tz = "UTC")
    want <- mapply(ref_business_seconds, as.list(s), as.list(e),
                   MoreArgs = list(ws = w[1], we = w[2], holidays = hol, tz = "UTC"))
    expect_equal(got, unname(want), info = paste(w, collapse = "-"))
  }
  # custom weekend
  got <- business_seconds(s, e, "08:00", "17:00", holidays = hol, weekend = c(5, 6), tz = "UTC")
  want <- mapply(ref_business_seconds, as.list(s), as.list(e),
                 MoreArgs = list(holidays = hol, weekend = c(5, 6), tz = "UTC"))
  expect_equal(got, unname(want))
})

test_that("matches the reference in a DST time zone", {
  set.seed(7)
  base <- as.POSIXct("2026-03-01", tz = tz)
  hol  <- as.Date("2026-03-01") + sort(sample(0:300, 15))
  n <- 50
  s <- base + round(runif(n, 0, 250 * 86400) / 60) * 60
  e <- s + round(runif(n, 0, 12 * 86400) / 60) * 60
  got <- business_seconds(s, e, "07:00", "19:00", holidays = hol)
  want <- mapply(ref_business_seconds, as.list(s), as.list(e),
                 MoreArgs = list(ws = "07:00", we = "19:00", holidays = hol, tz = tz))
  expect_equal(got, unname(want))
})
