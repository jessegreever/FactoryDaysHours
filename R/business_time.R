#' Working time between two timestamps
#'
#' Computes the amount of working time that elapses between `start` and `end`,
#' counting only the daily window from `work_start` to `work_end` on working
#' days. Weekends (Saturday and Sunday by default) and the dates listed in
#' `holidays` contribute nothing. All functions are fully vectorised and use
#' closed-form arithmetic, so there is no per-day iteration: the cost is
#' independent of how far apart `start` and `end` are.
#'
#' @details
#' Working time is measured on the local wall clock of `tz`. A working day
#' always contributes exactly `work_end - work_start` of time, including on
#' days when daylight-saving time changes, which is what a shift schedule
#' normally means. Instants that fall outside the working window are snapped
#' to the nearest edge of the window, so a `start` of 03:00 behaves like 08:00
#' on a day that starts at 08:00, and an `end` of 23:00 behaves like 17:00.
#'
#' If `end` is earlier than `start` the result is negative, so
#' `business_hours(a, b) == -business_hours(b, a)`.
#'
#' `start` and `end` are recycled against each other: either may be length 1
#' while the other is a long vector. Missing values propagate.
#'
#' @param start,end Timestamps. `POSIXct` is preferred. `Date` values are
#'   treated as local midnight of that day, and character values are parsed
#'   with [as.POSIXct()] in `tz`.
#' @param work_start,work_end Start and end of the working day. Accepts a
#'   `"HH:MM"` or `"HH:MM:SS"` string, a numeric number of hours after
#'   midnight (for example `8.5` for 08:30), a `difftime`/`hms` object, or a
#'   `POSIXct` whose time of day is used. `work_end` must be later than
#'   `work_start`.
#' @param holidays Dates with no working hours: a `Date` vector, a character
#'   vector in `"YYYY-MM-DD"` form, or a `POSIXct` vector (its local date is
#'   used). `NULL` means no holidays. Duplicates and weekend dates are ignored.
#' @param weekend Days of the week that are never worked, as integers
#'   (`0` = Sunday ... `6` = Saturday, matching `POSIXlt$wday`) or day names.
#'   Defaults to Saturday and Sunday. Use `NULL` or `integer(0)` for a
#'   seven-day operation.
#' @param tz Time zone in which the working window and holidays are
#'   interpreted. Defaults to the `tzone` attribute of `start` (then `end`),
#'   and finally the session time zone.
#'
#' @return A numeric vector the length of the longer of `start` and `end`:
#'   hours for `business_hours()`, working days (hours divided by the length
#'   of the working day) for `business_days()`, and seconds for
#'   `business_seconds()`.
#'
#' @examples
#' hol <- as.Date(c("2026-12-25", "2027-01-01"))
#' s <- as.POSIXct("2026-12-24 15:00", tz = "America/New_York")
#' e <- as.POSIXct("2026-12-28 10:00", tz = "America/New_York")
#'
#' # Thu 24th 15:00-17:00 (2h) + Mon 28th 08:00-10:00 (2h); Fri 25th is a
#' # holiday and the weekend is skipped.
#' business_hours(s, e, "08:00", "17:00", holidays = hol)
#'
#' business_days(s, e, "08:00", "17:00", holidays = hol)
#'
#' # Vectorised: one call for a whole column
#' starts <- s + 3600 * 0:5
#' business_hours(starts, e, "08:00", "17:00", holidays = hol)
#' @seealso [add_business_hours()] to go the other way, [is_business_day()].
#' @export
business_hours <- function(start, end, work_start = "08:00", work_end = "17:00",
                           holidays = NULL, weekend = c(6L, 0L), tz = NULL) {
  business_seconds(start, end, work_start, work_end, holidays, weekend, tz) / 3600
}

#' @rdname business_hours
#' @export
business_days <- function(start, end, work_start = "08:00", work_end = "17:00",
                          holidays = NULL, weekend = c(6L, 0L), tz = NULL) {
  tz  <- .resolve_tz(tz, start, end)
  cal <- .make_calendar(work_start, work_end, holidays, weekend, tz)
  .business_seconds_impl(start, end, cal, tz) / cal$D
}

#' @rdname business_hours
#' @export
business_seconds <- function(start, end, work_start = "08:00", work_end = "17:00",
                             holidays = NULL, weekend = c(6L, 0L), tz = NULL) {
  tz  <- .resolve_tz(tz, start, end)
  cal <- .make_calendar(work_start, work_end, holidays, weekend, tz)
  .business_seconds_impl(start, end, cal, tz)
}

.business_seconds_impl <- function(start, end, cal, tz) {
  r <- .recycle2(start, end, "start", "end")
  a <- .to_local(r[[1L]], tz, "start")
  b <- .to_local(r[[2L]], tz, "end")
  .cum_work_secs(b$day, b$sod, cal) - .cum_work_secs(a$day, a$sod, cal)
}

#' Add working hours to a timestamp
#'
#' Returns the instant at which `hours` of working time will have elapsed
#' after `start` (or before it, for negative `hours`), using the same calendar
#' rules as [business_hours()]. This is the inverse of [business_hours()]:
#' `business_hours(start, add_business_hours(start, h))` equals `h` whenever
#' `start` lies inside the working window.
#'
#' @inheritParams business_hours
#' @param start Timestamps to start from (`POSIXct`, `Date`, or character).
#' @param hours Numeric working hours to add; recycled against `start`.
#'
#' @details
#' Results always fall inside the working window. A `start` outside the window
#' is first snapped forward to the next working period, so adding zero hours
#' to 20:00 on a Friday yields 08:00 on the following Monday. A result that
#' lands exactly on the end of a working day is reported as the start of the
#' next working day; both instants represent the same amount of elapsed
#' working time.
#'
#' @return A `POSIXct` vector in time zone `tz`.
#' @examples
#' s <- as.POSIXct("2026-12-24 15:00", tz = "America/New_York")
#' add_business_hours(s, 4, "08:00", "17:00", holidays = as.Date("2026-12-25"))
#' @export
add_business_hours <- function(start, hours, work_start = "08:00", work_end = "17:00",
                               holidays = NULL, weekend = c(6L, 0L), tz = NULL) {
  tz  <- .resolve_tz(tz, start)
  cal <- .make_calendar(work_start, work_end, holidays, weekend, tz)
  r   <- .recycle2(start, hours, "start", "hours")
  a   <- .to_local(r[[1L]], tz, "start")
  secs <- as.numeric(r[[2L]]) * 3600

  target <- .cum_work_secs(a$day, a$sod, cal) + secs
  target[!is.finite(target)] <- NA_real_

  k   <- floor(target / cal$D)
  rem <- target - k * cal$D
  day <- .kth_workday(k, cal)

  .make_posixct(day, cal$ws + rem, tz)
}

# Build POSIXct from integer local date + seconds-of-day on the wall clock.
# Year/month/day are derived arithmetically (no Date round trip) and handed
# to mktime via as.POSIXct.POSIXlt with isdst = -1, so DST is resolved by the
# C library exactly as as.POSIXct("YYYY-mm-dd HH:MM:SS", tz) would.
.make_posixct <- function(day, sod, tz) {
  ok <- !is.na(day) & !is.na(sod)
  day[!ok] <- 0L
  sod[!ok] <- 0
  ymd <- .civil_from_day(day)
  lt <- .POSIXlt(list(
    sec   = sod %% 60,
    min   = as.integer((sod %/% 60) %% 60),
    hour  = as.integer(sod %/% 3600),
    mday  = ymd$mday,
    mon   = ymd$mon,
    year  = ymd$year,
    wday  = rep_len(0L, length(day)),
    yday  = rep_len(0L, length(day)),
    isdst = rep_len(-1L, length(day))
  ), tz = tz)
  out <- as.POSIXct(lt)
  out[!ok] <- NA
  out
}

# Days since 1970-01-01 -> POSIXlt-style year (since 1900), mon (0-11),
# mday (1-31). Howard Hinnant's civil_from_days, vectorised.
.civil_from_day <- function(day) {
  z   <- day + 719468L
  era <- z %/% 146097L
  doe <- z - era * 146097L
  yoe <- (doe - doe %/% 1460L + doe %/% 36524L - doe %/% 146096L) %/% 365L
  doy <- doe - (365L * yoe + yoe %/% 4L - yoe %/% 100L)
  mp  <- (5L * doy + 2L) %/% 153L
  mday <- doy - (153L * mp + 2L) %/% 5L + 1L
  m   <- mp + 3L - 12L * (mp >= 10L)
  y   <- yoe + era * 400L + (m <= 2L)
  list(year = as.integer(y - 1900L), mon = as.integer(m - 1L), mday = as.integer(mday))
}

#' Is a date a working day?
#'
#' @inheritParams business_hours
#' @param x Dates or timestamps (`Date`, `POSIXct`, or character).
#' @return A logical vector: `TRUE` when the local date of `x` is neither a
#'   weekend day nor a holiday.
#' @examples
#' is_business_day(as.Date("2026-12-25") + 0:4, holidays = as.Date("2026-12-25"))
#' @export
is_business_day <- function(x, holidays = NULL, weekend = c(6L, 0L), tz = NULL) {
  tz <- .resolve_tz(tz, x)
  weekend <- .parse_weekend(weekend)
  cal <- list(weekend = weekend, hol = .parse_holidays(holidays, weekend, tz))
  d <- .to_local(x, tz, "x")$day
  out <- .is_workday(d, cal)
  out[is.na(d)] <- NA
  out
}
