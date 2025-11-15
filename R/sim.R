#' Generate synthetic bookings with explicit date types
#' arrival_date is Date; booking_ts is POSIXct (UTC)
#' Generate synthetic bookings with explicit date types
#' arrival_date is Date; booking_ts is POSIXct (UTC)
#' Generate synthetic bookings with explicit date types
#' arrival_date is Date; booking_ts is POSIXct (UTC)
generate_synthetic_bookings <- function(start_date = "2021-01-01",
                                        end_date   = "2022-12-31",
                                        avg_bookings_per_day = 50,
                                        properties = 5,
                                        segments   = c("leisure","business"),
                                        channels   = c("direct","ota"),
                                        max_lead_days = 90,
                                        compression_level = 0.3,
                                        seed = 42) {
  set.seed(seed)
  arrivals <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  out <- vector("list", length(arrivals) * properties)
  idx <- 1L

  for (p in seq_len(properties)) {
    prop_id <- sprintf("P%03d", p)
    for (ad in arrivals) {
      n <- rpois(1, lambda = avg_bookings_per_day)
      if (n <= 0) next

      # mixture that compresses mass toward short lead times
      w_short <- 0.2 + 0.6 * compression_level
      comp <- sample(c("short","long"), size = n, replace = TRUE, prob = c(w_short, 1 - w_short))
      mus  <- ifelse(comp == "short", log(7),  log(30))
      sigs <- ifelse(comp == "short", 0.5,     0.6)
      leads <- pmin(pmax(floor(rlnorm(n, meanlog = mus, sdlog = sigs)), 0L), max_lead_days)

      # explicit types
      arr_dates  <- rep(as.Date(ad), n)                 # Date
      book_dates <- arr_dates - as.integer(leads)       # Date arithmetic, no time-of-day
      book_ts    <- as.POSIXct(book_dates, tz = "UTC")  # convert at the end

      seg <- sample(segments, size = n, replace = TRUE)
      ch  <- sample(channels, size = n, replace = TRUE)
      price <- round(rnorm(n, mean = 150, sd = 30), 2)
      cancelled <- as.logical(rbinom(n, 1, prob = 0.1))
      origin <- sample(c("US","CA","MX","GB","DE"), size = n, replace = TRUE)
      stay_nights <- pmax(1L, rpois(n, lambda = 3L))

      out[[idx]] <- data.frame(
        arrival_date      = arr_dates,         # Date
        booking_ts        = book_ts,           # POSIXct
        stay_nights       = stay_nights,
        channel           = ch,
        segment           = seg,
        origin            = origin,
        price_at_booking  = price,
        cancelled         = cancelled,
        property_id       = prop_id,
        stringsAsFactors  = FALSE
      )
      idx <- idx + 1L
    }
  }
  do.call(rbind, out[seq_len(idx - 1L)])
}
