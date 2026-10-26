test_that("is_business_day", {
  d <- as.Date("2026-12-24") + 0:5  # Thu .. Tue
  expect_equal(is_business_day(d, holidays = "2026-12-25"),
               c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(is_business_day(c(d[1], NA)), c(TRUE, NA))
  expect_equal(is_business_day(as.POSIXct("2026-12-26 23:30", tz = "UTC"), tz = "Asia/Tokyo"), FALSE)
  expect_equal(is_business_day(as.POSIXct("2026-12-27 23:30", tz = "UTC"), tz = "Asia/Tokyo"), TRUE)
  expect_equal(is_business_day(d, weekend = NULL, holidays = NULL), rep(TRUE, 6))
})
