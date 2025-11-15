#' Pickup forecast error bound
#' @param D divergence in [0,1]
#' @param delta horizon in days
#' @param delta_max maximum lead days considered
#' @param Chist_delta cumulative pickup fraction at horizon delta
#' @return upper bound on relative error
relative_error_bound <- function(D, delta, delta_max, Chist_delta) {
  if (length(D) != 1L || !is.finite(D)) stop("D must be a single finite number in [0,1].")
  if (D < 0 || D > 1) stop("D must be in [0,1].")
  if (length(delta) != 1L || length(delta_max) != 1L || length(Chist_delta) != 1L)
    stop("delta, delta_max, and Chist_delta must be scalars.")
  if (!is.finite(delta) || !is.finite(delta_max) || !is.finite(Chist_delta))
    stop("delta, delta_max, and Chist_delta must be finite.")
  if (delta_max <= 0 || delta < 0 || delta > delta_max)
    stop("delta must be in [0, delta_max] with delta_max > 0.")
  if (Chist_delta <= 0)
    stop("Chist_delta must be > 0.")
  2 * D * (1 - delta / delta_max) / Chist_delta
}


#' Map bound to simple actions
#' @param error_bound numeric
#' @return list with cadence, advance_purchase_buffer_days, staffing_buffer_pct
recommend_actions <- function(error_bound) {
  if (error_bound >= 0.20) {
    list(price_cadence = "intraday",
         advance_purchase_buffer_days = 0L,
         staffing_buffer_pct = 0.15)
  } else if (error_bound >= 0.10) {
    list(price_cadence = "daily",
         advance_purchase_buffer_days = 3L,
         staffing_buffer_pct = 0.08)
  } else {
    list(price_cadence = "weekly",
         advance_purchase_buffer_days = 7L,
         staffing_buffer_pct = 0.05)
  }
}
