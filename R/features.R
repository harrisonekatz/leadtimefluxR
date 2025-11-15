#' Validate and coerce the minimal booking schema
#' Requires: arrival_date, booking_ts, stay_nights, channel, segment,
#' origin, price_at_booking, cancelled, property_id
validate_schema <- function(df) {
  req <- c("arrival_date","booking_ts","stay_nights","channel","segment",
           "origin","price_at_booking","cancelled","property_id")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) stop(sprintf("Missing required columns: %s", paste(miss, collapse=", ")))

  # arrival_date -> Date
  if (is.numeric(df$arrival_date)) {
    df$arrival_date <- as.Date(df$arrival_date, origin = "1970-01-01")
  } else {
    df$arrival_date <- as.Date(df$arrival_date)
  }

  # booking_ts -> POSIXct with robust handling
  if (is.character(df$booking_ts)) {
    ts_try <- suppressWarnings(lubridate::ymd_hms(df$booking_ts, tz = "UTC", quiet = TRUE))
    df$booking_ts <- if (all(is.na(ts_try))) as.POSIXct(df$booking_ts, tz = "UTC") else ts_try
  } else if (is.numeric(df$booking_ts)) {
    mx <- suppressWarnings(max(df$booking_ts, na.rm = TRUE))
    if (is.finite(mx) && mx < 1e7) {
      # treat numeric as days since epoch
      df$booking_ts <- as.POSIXct(as.Date(df$booking_ts, origin = "1970-01-01"), tz = "UTC")
    } else {
      # treat numeric as seconds since epoch
      df$booking_ts <- as.POSIXct(df$booking_ts, origin = "1970-01-01", tz = "UTC")
    }
  } else if (inherits(df$booking_ts, "POSIXt")) {
    df$booking_ts <- as.POSIXct(df$booking_ts, tz = "UTC")
  } else {
    stop("Unsupported type for booking_ts")
  }

  # Repair common pitfall: POSIXct stuck in 1970 due to days-as-seconds
  secs <- suppressWarnings(as.numeric(df$booking_ts))
  yr   <- suppressWarnings(lubridate::year(df$booking_ts))
  if (all(yr == 1970, na.rm = TRUE) && is.finite(max(secs, na.rm = TRUE)) && max(secs, na.rm = TRUE) < 1e7) {
    df$booking_ts <- as.POSIXct(as.Date(secs, origin = "1970-01-01"), tz = "UTC")
  }

  # coerce other fields
  df$stay_nights      <- as.integer(df$stay_nights)
  df$price_at_booking <- as.numeric(df$price_at_booking)
  df$cancelled        <- as.logical(df$cancelled)
  df$channel          <- as.character(df$channel)
  df$segment          <- as.character(df$segment)
  df$origin           <- as.character(df$origin)
  df$property_id      <- as.character(df$property_id)

  invisible(df)
}

#' Compute integer lead time in days and drop negatives
compute_lead_time_days <- function(df) {
  df <- validate_schema(df)
  df$lead_time_days <- as.integer(difftime(df$arrival_date, as.Date(df$booking_ts), units = "days"))
  df[df$lead_time_days >= 0, , drop = FALSE]
}

#' Lead-time histograms by cohort
#' Robust to empty k and avoids complete(fill=...) pitfalls
leadtime_histograms <- function(df, group_cols = c("property_id"), max_lead_days = 90) {
  df <- compute_lead_time_days(df)
  df$month <- as.Date(cut(df$arrival_date, "month"))
  df <- df[df$lead_time_days <= max_lead_days, , drop = FALSE]

  keys_all <- c(group_cols, "month", "lead_time_days")
  counts <- dplyr::as_tibble(df) |>
    dplyr::count(dplyr::across(all_of(keys_all)), name = "n")

  keys <- c(group_cols, "month")
  totals <- counts |>
    dplyr::group_by(dplyr::across(all_of(keys))) |>
    dplyr::summarise(N = sum(.data$n), .groups = "drop")

  out <- counts |>
    dplyr::left_join(totals, by = keys) |>
    dplyr::mutate(Lk = .data$n / .data$N) |>
    dplyr::rename(k = .data$lead_time_days) |>
    dplyr::select(dplyr::all_of(keys), k, Lk)

  grid <- tidyr::expand_grid(
    dplyr::distinct(out, dplyr::across(all_of(keys))),
    k = 0:max_lead_days
  )

  res <- grid |>
    dplyr::left_join(out, by = c(keys, "k")) |>
    dplyr::mutate(Lk = tidyr::replace_na(.data$Lk, 0)) |>
    dplyr::arrange(dplyr::across(all_of(keys)), .data$k) |>
    dplyr::select(dplyr::all_of(group_cols), month, k, Lk)

  dplyr::as_tibble(res)
}


#' Pickup curve C_hist(Δ) as cumulative sum of L(k)
#' @param Lk tibble from leadtime_histograms
#' @param group_cols character vector of grouping columns
#' @return tibble with Chist added
pickup_curve <- function(Lk, group_cols = c("property_id")) {
  Lk |>
    dplyr::arrange(dplyr::across(all_of(c(group_cols, "month"))), .data$k) |>
    dplyr::group_by(dplyr::across(all_of(c(group_cols, "month")))) |>
    dplyr::mutate(Chist = cumsum(.data$Lk)) |>
    dplyr::ungroup()
}
