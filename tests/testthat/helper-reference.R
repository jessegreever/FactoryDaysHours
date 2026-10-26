# Slow but obviously-correct reference: walk minute by minute.
ref_business_seconds <- function(start, end, ws = "08:00", we = "17:00",
                                 holidays = NULL, weekend = c(6, 0), tz = "UTC") {
  sign <- 1
  if (end < start) { tmp <- start; start <- end; end <- tmp; sign <- -1 }
  ws_s <- FactoryDaysHours:::.parse_clock(ws, "ws")
  we_s <- FactoryDaysHours:::.parse_clock(we, "we")
  hol  <- if (is.null(holidays)) as.Date(character()) else as.Date(holidays)
  # minute grid [start, end)
  s <- as.numeric(start); e <- as.numeric(end)
  if (e <= s) return(0)
  grid <- seq(s, e - 1e-9, by = 60)
  grid <- grid[grid < e]
  lt <- as.POSIXlt(grid, origin = "1970-01-01", tz = tz)
  sod <- lt$hour * 3600 + lt$min * 60 + lt$sec
  d <- as.Date(lt)
  ok <- !(lt$wday %in% weekend) & !(d %in% hol) & sod >= ws_s & sod < we_s
  sign * sum(ok) * 60
}
