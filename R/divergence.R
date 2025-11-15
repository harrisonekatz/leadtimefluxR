#' Normalized L1 distance between two distributions on same support
#' @param p numeric vector summing to 1
#' @param q numeric vector summing to 1
#' @return scalar in [0,1]
normalized_l1 <- function(p, q) {
  if (length(p) != length(q)) stop("p and q must have same length")
  0.5 * sum(abs(p - q))
}

#' Build divergence series by month
#' @param Lk tibble from leadtime_histograms
#' @param group_cols character vector of grouping columns
#' @param method "yoy" or "baseline"
#' @param baseline_year integer baseline year for "baseline" method
#' @return tibble with group_cols, month, D
divergence_series <- function(Lk, group_cols = c("property_id"),
                              method = c("yoy","baseline"), baseline_year = 2018) {
  method <- match.arg(method)
  keys <- c(group_cols, "month")
  # pivot month x k
  piv <- Lk |>
    tidyr::pivot_wider(names_from = k, values_from = Lk, values_fill = 0) |>
    dplyr::arrange(dplyr::across(all_of(keys)))

  rows <- list()
  by_groups <- split(piv, interaction(piv[, group_cols], drop = TRUE))
  for (g in by_groups) {
    # sort by month
    g <- g[order(g$month), , drop = FALSE]
    mat <- as.matrix(g[, setdiff(names(g), c(group_cols, "month"))])
    months <- g$month
    if (method == "yoy") {
      if (nrow(g) > 12) {
        for (t in 13:nrow(g)) {
          D <- normalized_l1(mat[t, ], mat[t-12, ])
          rows[[length(rows)+1]] <- cbind(g[t, group_cols, drop=FALSE],
                                          month = months[t], D = D)
        }
      }
    } else {
      # compare to same month in baseline year
      base_idx <- which(lubridate::year(months) == baseline_year)
      if (length(base_idx) > 0) {
        base_map <- split(mat[base_idx, , drop = FALSE], lubridate::month(months[base_idx]))
        for (t in seq_len(nrow(g))) {
          mnum <- lubridate::month(months[t])
          if (as.character(mnum) %in% names(base_map)) {
            base_vec <- as.numeric(base_map[[as.character(mnum)]])
            D <- normalized_l1(mat[t, ], base_vec)
            rows[[length(rows)+1]] <- cbind(g[t, group_cols, drop=FALSE],
                                            month = months[t], D = D)
          }
        }
      }
    }
  }
  if (length(rows) == 0) return(dplyr::tibble(!!!setNames(rep(list(character()), length(group_cols)), group_cols),
                                              month = as.Date(character()), D = numeric()))
  out <- dplyr::bind_rows(rows)
  # fix types
  out$month <- as.Date(out$month)
  dplyr::as_tibble(out)
}

#' Convenience wrappers
yoy_divergence_series <- function(Lk, group_cols = c("property_id")) {
  divergence_series(Lk, group_cols = group_cols, method = "yoy")
}
baseline_divergence_series <- function(Lk, group_cols = c("property_id"), baseline_year = 2018) {
  divergence_series(Lk, group_cols = group_cols, method = "baseline", baseline_year = baseline_year)
}

#' STL decomposition of divergence time series
#' @param Dts tibble with group_cols, month, D
#' @param group_cols character vector
#' @param period seasonal period (12 for months)
#' @return tibble with trend, seasonal, remainder
stl_decompose_divergence <- function(Dts, group_cols = c("property_id"), period = 12) {
  rows <- list()
  by_groups <- split(Dts, interaction(Dts[, group_cols], drop = TRUE))
  for (g in by_groups) {
    g <- g[order(g$month), , drop = FALSE]
    if (nrow(g) < 2*period) next
    # build ts from D
    start_year <- lubridate::year(g$month[1])
    start_month <- lubridate::month(g$month[1])
    ts_obj <- stats::ts(g$D, start = c(start_year, start_month), frequency = period)
    fit <- stats::stl(ts_obj, s.window = "periodic", robust = TRUE)
    comp <- cbind(trend = as.numeric(fit$time.series[, "trend"]),
                  seasonal = as.numeric(fit$time.series[, "seasonal"]),
                  remainder = as.numeric(fit$time.series[, "remainder"]))
    rows[[length(rows)+1]] <- cbind(g[, group_cols, drop=FALSE],
                                    month = g$month,
                                    as.data.frame(comp))
  }
  if (length(rows) == 0) return(dplyr::tibble())
  dplyr::as_tibble(dplyr::bind_rows(rows))
}


#' Adjacent-month divergence series D(L_t, L_{t-1})
#' Works when YoY is unavailable (short samples)
adjacent_divergence_series <- function(Lk, group_cols = c("property_id")) {
  keys <- c(group_cols, "month")
  piv <- Lk |>
    tidyr::pivot_wider(names_from = k, values_from = Lk, values_fill = 0) |>
    dplyr::arrange(dplyr::across(all_of(keys)))

  rows <- list()
  by_groups <- split(piv, interaction(piv[, group_cols], drop = TRUE))
  for (g in by_groups) {
    g <- g[order(g$month), , drop = FALSE]
    if (nrow(g) < 2) next
    mat <- as.matrix(g[, setdiff(names(g), c(group_cols, "month"))])
    months <- g$month
    for (t in 2:nrow(g)) {
      D <- normalized_l1(mat[t, ], mat[t-1, ])
      rows[[length(rows) + 1L]] <- cbind(g[t, group_cols, drop = FALSE], month = months[t], D = D)
    }
  }
  if (length(rows) == 0L) {
    return(dplyr::tibble(!!!setNames(rep(list(character()), length(group_cols)), group_cols),
                         month = as.Date(character()), D = numeric()))
  }
  dplyr::as_tibble(dplyr::bind_rows(rows))
}

#' Safe divergence quantile with fallback default
safe_divergence_quantile <- function(Dts, probs = 0.9, default = 0.2) {
  if (is.null(Dts) || !is.data.frame(Dts) || nrow(Dts) == 0 || all(is.na(Dts$D))) return(default)
  as.numeric(stats::quantile(Dts$D, probs = probs, na.rm = TRUE))
}


#' Adjacent-month divergence series D(L_t, L_{t-1})
#' Works when YoY is unavailable in short samples
adjacent_divergence_series <- function(Lk, group_cols = c("property_id")) {
  keys <- c(group_cols, "month")
  piv <- Lk |>
    tidyr::pivot_wider(names_from = k, values_from = Lk, values_fill = 0) |>
    dplyr::arrange(dplyr::across(all_of(keys)))

  rows <- list()
  by_groups <- split(piv, interaction(piv[, group_cols], drop = TRUE))
  for (g in by_groups) {
    g <- g[order(g$month), , drop = FALSE]
    if (nrow(g) < 2) next
    mat <- as.matrix(g[, setdiff(names(g), c(group_cols, "month"))])
    months <- g$month
    for (t in 2:nrow(g)) {
      D <- normalized_l1(mat[t, ], mat[t-1, ])
      rows[[length(rows) + 1L]] <- cbind(g[t, group_cols, drop = FALSE],
                                         month = months[t], D = D)
    }
  }
  if (length(rows) == 0L) {
    return(dplyr::tibble(!!!setNames(rep(list(character()), length(group_cols)), group_cols),
                         month = as.Date(character()), D = numeric()))
  }
  dplyr::as_tibble(dplyr::bind_rows(rows))
}

#' Safe divergence quantile with fallback default
#' Returns default when series is empty or all NA
safe_divergence_quantile <- function(Dts, probs = 0.9, default = 0.2) {
  if (is.null(Dts) || !is.data.frame(Dts) || nrow(Dts) == 0 || all(is.na(Dts$D))) return(default)
  as.numeric(stats::quantile(Dts$D, probs = probs, na.rm = TRUE))
}




