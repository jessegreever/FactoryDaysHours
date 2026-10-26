tz <- "America/New_York"
P <- function(x) as.POSIXct(x, tz = tz)
hol <- as.Date(c("2026-12-25", "2027-01-01"))

test_that("adds within a day and across holidays/weekends", {
  s <- P("2026-12-24 15:00")
  expect_equal(add_business_hours(s, 1, holidays = hol), P("2026-12-24 16:00"))
  # 2h left Thu, skip Fri (holiday) + weekend, 2h into Mon 28th
  expect_equal(add_business_hours(s, 4, holidays = hol), P("2026-12-28 10:00"))
  expect_equal(add_business_hours(s, 0, holidays = hol), s)
})

test_that("snaps starts outside the window forward", {
  expect_equal(add_business_hours(P("2026-03-06 20:00"), 0), P("2026-03-09 08:00"))
  expect_equal(add_business_hours(P("2026-03-07 10:00"), 2), P("2026-03-09 10:00"))
  expect_equal(add_business_hours(P("2026-03-02 05:00"), 2), P("2026-03-02 10:00"))
})

test_that("negative hours go backwards", {
  expect_equal(add_business_hours(P("2026-03-09 09:00"), -2), P("2026-03-06 16:00"))
  expect_equal(add_business_hours(P("2026-12-28 10:00"), -4, holidays = hol), P("2026-12-24 15:00"))
})

test_that("consecutive holidays and long jumps", {
  many <- as.Date("2026-06-01") + 0:13          # two full weeks of holidays
  s <- P("2026-05-29 16:00")                     # Friday
  expect_equal(add_business_hours(s, 2, holidays = many), P("2026-06-15 09:00"))
  expect_equal(add_business_hours(s, 9 * 250 + 2, holidays = many),
               add_business_hours(P("2026-06-15 09:00"), 9 * 250, holidays = many))
})

test_that("round-trips with business_hours", {
  set.seed(1)
  base <- as.POSIXct("2026-01-05 08:00", tz = tz)
  hols <- as.Date("2026-01-01") + sort(sample(0:400, 25))
  n <- 200
  s <- base + round(runif(n, 0, 300 * 86400))
  # pull starts inside the working window so the round trip is exact
  s <- add_business_hours(s, 0, holidays = hols)
  h <- round(runif(n, -500, 1500), 3)
  e <- add_business_hours(s, h, holidays = hols)
  expect_equal(business_hours(s, e, holidays = hols), h, tolerance = 1e-9)
  expect_true(all(is_business_day(e, holidays = hols)))
})

test_that("vectorisation, recycling, NA and tz", {
  s <- P("2026-03-02 09:00")
  out <- add_business_hours(s, c(1, NA, 10))
  expect_equal(out, P(c("2026-03-02 10:00", NA, "2026-03-03 10:00")))
  expect_equal(attr(out, "tzone"), tz)
  out2 <- add_business_hours(P(c("2026-03-02 09:00", NA)), 1)
  expect_true(is.na(out2[2]))
  u <- as.POSIXct("2026-03-02 12:00", tz = "UTC")
  expect_equal(add_business_hours(u, 1, tz = "Asia/Tokyo"),
               as.POSIXct("2026-03-03 09:00", tz = "Asia/Tokyo"))
})

test_that("DST transition day keeps wall-clock times", {
  # Sunday 2026-03-08 springs forward; treat it as a working day.
  expect_equal(add_business_hours(P("2026-03-08 01:00"), 2, "01:00", "10:00", weekend = NULL),
               P("2026-03-08 03:00"))
})
