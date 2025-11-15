#' Plot divergence time series
#' @param Dts tibble with month and D
plot_divergence_ts <- function(Dts, group_cols = c("property_id")) {
  ggplot2::ggplot(Dts, ggplot2::aes(x = .data$month, y = .data$D)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(group_cols, scales = "free_y") +
    ggplot2::labs(x = "Month", y = "Normalized L1 divergence", title = "Divergence series")
}

#' Plot a lead-time histogram for a cohort-month
plot_leadtime_hist <- function(Lk, property_id, month) {
  d <- Lk[Lk$property_id == property_id & Lk$month == as.Date(month), ]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$k, y = .data$Lk)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Lead time (days)", y = "Probability", title = paste("L(k),", property_id, month))
}
