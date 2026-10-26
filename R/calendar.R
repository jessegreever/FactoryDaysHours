# Internal helpers: input parsing and the closed-form working-day arithmetic.
#
# Design notes
# ------------
# Every public function reduces to a cumulative function F(t) = number of
# working seconds between a fixed reference (1970-01-01 local midnight) and
# the instant t.  Elapsed working time between two instants is simply
# F(end) - F(start), so no loop over days is ever needed.
#
# F(t) = D * W(day(t)) + [day(t) is a working day] * clamp(sod(t) - ws, 0, D)
#
# where D is the length of the working window in seconds, ws its start
# (seconds after midnight), sod(t) the local seconds-of-day of t, and W(d) the
# number of working days strictly before local date d.  W(d) is itself closed
# form: weekday counting is integer division by 7 plus a 7-entry lookup table,
# and the holiday correction is a single findInterval() on the sorted holiday
# vector (O(log n), vectorised in C).

# Days are integer days since 1970-01-01, which was a Thursday.
# R's convention: 0 = Sunday ... 6 = Saturday.
.EPOCH_WDAY <- 4L

.wday_of <- function(day) (day + .EPOCH_WDAY) %% 7L

# Build the lookup tables for a given set of weekend days (0 = Sunday).
# Returns list(nwork, prefix, inv):
#   prefix[r + 1] = number of working days among day offsets 0..r-1
#                   of a week that starts on a Thursday (r in 0..7)
#   inv[j + 1]    = offset (0..6) of the j-th working day in such a week
.week_tables <- function(weekend) {
  wd <- (.EPOCH_WDAY + 0:6) %% 7L
  is_work <- !(wd %in% weekend)
  list(
    nwork  = sum(is_work),
    prefix = c(0L, cumsum(is_work)),
    inv    = which(is_work) - 1L
  )
}

# Number of non-weekend days in [0, d) for integer d (works for d < 0 too).
.weekdays_before <- function(d, tbl) {
  (d %/% 7L) * tbl$nwork + tbl$prefix[d %% 7L + 1L]
}

# Inverse of .weekdays_before: the (k)-th (0-based) non-weekend day.
.kth_weekday <- function(k, tbl) {
  (k %/% tbl$nwork) * 7L + tbl$inv[k %% tbl$nwork + 1L]
}

# Number of holidays strictly before day d (hol is sorted, holidays on
# weekends already removed).
.holidays_before <- function(d, hol) {
  if (length(hol) == 0L) return(rep_len(0L, length(d)))
  findInterval(d - 1L, hol)
}

# Number of holidays on or before day d.
.holidays_through <- function(d, hol) {
  if (length(hol) == 0L) return(rep_len(0L, length(d)))
  findInterval(d, hol)
}

.is_workday <- function(d, cal) {
  !(.wday_of(d) %in% cal$weekend) &
    (length(cal$hol) == 0L | match(d, cal$hol, nomatch = 0L) == 0L)
}

# Working days strictly before day d.
.workdays_before <- function(d, cal) {
  .weekdays_before(d, cal$tbl) - .holidays_before(d, cal$hol)
}

# Cumulative working seconds from the reference up to (day, sod).
.cum_work_secs <- function(day, sod, cal) {
  within <- pmin(pmax(sod - cal$ws, 0), cal$D)
  cal$D * .workdays_before(day, cal) + within * .is_workday(day, cal)
}

# Resolve the k-th (0-based) working day, skipping holidays.  Vectorised
# fixed-point iteration; each pass advances over at least one holiday for
# any element that has not converged, so the number of passes is bounded by
# the number of holiday clusters crossed, not the number of days.
.kth_workday <- function(k, cal) {
  d <- .kth_weekday(k, cal$tbl)
  if (length(cal$hol) == 0L) return(d)
  repeat {
    d_new <- .kth_weekday(k + .holidays_through(d, cal$hol), cal$tbl)
    same <- is.na(d_new) | d_new == d
    if (all(same)) return(d_new)
    d <- d_new
  }
}

# ---- input parsing ---------------------------------------------------------

.resolve_tz <- function(tz, ...) {
  if (!is.null(tz)) return(tz)
  for (x in list(...)) {
    if (inherits(x, "POSIXt")) {
      z <- attr(x, "tzone")
      if (!is.null(z) && nzchar(z[1L])) return(z[1L])
    }
  }
  ""
}

# Convert a clock-time specification to seconds after midnight.
.parse_clock <- function(x, arg) {
  if (length(x) != 1L) stop(sprintf("`%s` must have length 1.", arg), call. = FALSE)
  if (inherits(x, "difftime")) {
    s <- as.numeric(x, units = "secs")
  } else if (inherits(x, "POSIXt")) {
    lt <- as.POSIXlt(x)
    s <- lt$hour * 3600 + lt$min * 60 + lt$sec
  } else if (is.numeric(x)) {
    s <- x * 3600
  } else if (is.character(x)) {
    parts <- strsplit(trimws(x), ":", fixed = TRUE)[[1L]]
    if (!length(parts) %in% 1:3 || anyNA(suppressWarnings(as.numeric(parts)))) {
      stop(sprintf("`%s` must look like \"HH:MM\" or \"HH:MM:SS\", got \"%s\".", arg, x),
           call. = FALSE)
    }
    parts <- as.numeric(parts)
    s <- sum(parts * c(3600, 60, 1)[seq_along(parts)])
  } else {
    stop(sprintf("`%s` must be a \"HH:MM\" string, a numeric number of hours, a difftime/hms, or a POSIXct.", arg),
         call. = FALSE)
  }
  if (is.na(s) || s < 0 || s > 86400) {
    stop(sprintf("`%s` must be a time of day between 00:00 and 24:00.", arg), call. = FALSE)
  }
  s
}

# Holidays -> sorted integer days, de-duplicated, weekend entries dropped.
.parse_holidays <- function(holidays, weekend, tz) {
  if (is.null(holidays) || length(holidays) == 0L) return(integer(0))
  if (inherits(holidays, "POSIXt")) {
    d <- as.Date(as.POSIXlt(holidays, tz = tz))
  } else if (inherits(holidays, "Date")) {
    d <- holidays
  } else if (is.character(holidays) || is.factor(holidays)) {
    d <- as.Date(as.character(holidays))
  } else if (is.numeric(holidays)) {
    d <- as.Date(holidays, origin = "1970-01-01")
  } else {
    stop("`holidays` must be a Date, POSIXct, or character (\"YYYY-MM-DD\") vector.", call. = FALSE)
  }
  d <- as.integer(floor(unclass(d)))
  d <- d[!is.na(d)]
  d <- d[!(.wday_of(d) %in% weekend)]
  sort(unique(d))
}

.parse_weekend <- function(weekend) {
  if (is.null(weekend)) return(integer(0))
  if (is.character(weekend)) {
    nm <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
    idx <- pmatch(tolower(weekend), nm, duplicates.ok = TRUE)
    if (anyNA(idx)) stop("`weekend` contains an unrecognised day name.", call. = FALSE)
    weekend <- idx - 1L
  }
  weekend <- as.integer(weekend)
  if (anyNA(weekend) || any(weekend < 0L | weekend > 6L)) {
    stop("`weekend` must be day numbers 0 (Sunday) to 6 (Saturday), or day names.", call. = FALSE)
  }
  weekend <- sort(unique(weekend))
  if (length(weekend) >= 7L) stop("`weekend` cannot contain every day of the week.", call. = FALSE)
  weekend
}

# Build the immutable calendar description used by all computations.
.make_calendar <- function(work_start, work_end, holidays, weekend, tz) {
  ws <- .parse_clock(work_start, "work_start")
  we <- .parse_clock(work_end, "work_end")
  if (we <= ws) {
    stop("`work_end` must be later than `work_start` (overnight shifts are not supported).",
         call. = FALSE)
  }
  weekend <- .parse_weekend(weekend)
  list(
    ws = ws, we = we, D = we - ws,
    weekend = weekend,
    tbl = .week_tables(weekend),
    hol = .parse_holidays(holidays, weekend, tz),
    tz = tz
  )
}

# Timestamps -> list(day = integer local date, sod = local seconds-of-day).
.to_local <- function(x, tz, arg) {
  if (inherits(x, "Date")) {
    day <- as.integer(floor(unclass(x)))
    return(list(day = day, sod = rep_len(0, length(day))))
  }
  if (is.character(x) || is.factor(x)) {
    x <- as.POSIXct(as.character(x), tz = tz)
  } else if (!inherits(x, "POSIXt")) {
    stop(sprintf("`%s` must be POSIXct, Date, or a character vector parseable by as.POSIXct().", arg),
         call. = FALSE)
  }
  lt  <- as.POSIXlt(x, tz = tz)
  list(day = .lt_day(lt), sod = lt$hour * 3600 + lt$min * 60 + lt$sec)
}

# Days since 1970-01-01 from POSIXlt year/yday, in pure integer arithmetic.
# (as.Date.POSIXlt is ~10x slower because it re-normalises every field.)
# Leap years <= y: y %/% 4 - y %/% 100 + y %/% 400; there are 477 of them
# up to and including 1969.
.lt_day <- function(lt) {
  y <- lt$year + 1900L
  ym1 <- y - 1L
  as.integer(365L * (y - 1970L) + (ym1 %/% 4L - ym1 %/% 100L + ym1 %/% 400L) - 477L + lt$yday)
}

.recycle2 <- function(a, b, name_a, name_b) {
  na <- length(a); nb <- length(b)
  if (na == nb) return(list(a, b))
  if (na == 1L) return(list(rep_len(a, nb), b))
  if (nb == 1L) return(list(a, rep_len(b, na)))
  stop(sprintf("`%s` (length %d) and `%s` (length %d) must have the same length, or one must be length 1.",
               name_a, na, name_b, nb), call. = FALSE)
}

# Vectorised recycling for POSIXct / Date preserving class.
.rep_like <- function(x, n) {
  if (length(x) == n) return(x)
  x[rep_len(seq_along(x), n)]
}
