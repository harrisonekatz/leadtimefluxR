#' Build figures and tables for the paper (synthetic data)
#' @param out_dir where to write artifacts (default "paper_artifacts")
#' @return normalized path to out_dir (invisibly)
#' @export
make_paper_artifacts <- function(out_dir = "paper_artifacts") {
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(lubridate)
    library(ggplot2); library(readr)
  })
  
  # ----- paths -----
  fig_dir  <- file.path(out_dir, "figures")
  tab_dir  <- file.path(out_dir, "tables")
  dat_dir  <- file.path(out_dir, "data")
  meta_dir <- file.path(out_dir, "metadata")
  for (d in c(out_dir, fig_dir, tab_dir, dat_dir, meta_dir)) dir.create(d, TRUE, TRUE)
  
  save_png <- function(p, path, width = 1800, height = 1200, res = 220) {
    png(path, width = width, height = height, res = res); print(p); dev.off()
  }
  map_actions <- function(bound) {
    price_cadence <- if (bound >= 0.20) "intraday" else if (bound >= 0.10) "daily" else "weekly"
    ap_days <- if (bound >= 0.20) 0L else if (bound >= 0.10) 3L else 7L
    staff   <- if (bound >= 0.20) 0.15 else if (bound >= 0.10) 0.10 else 0.05
    list(price_cadence = price_cadence,
         advance_purchase_buffer_days = ap_days,
         staffing_buffer_pct = staff)
  }
  
  # ----- 1) synthetic data for 24 months -----
  set.seed(20251115)
  df <- generate_synthetic_bookings(
    start_date = "2021-01-01", end_date = "2022-12-31",
    avg_bookings_per_day = 20, properties = 3,
    max_lead_days = 60, compression_level = 0.4, seed = 123
  )
  
  schema_ok <- data.frame(
    column = c("arrival_date","booking_ts","stay_nights","channel","segment",
               "origin","price_at_booking","cancelled","property_id"),
    class  = sapply(df[c("arrival_date","booking_ts","stay_nights","channel","segment",
                         "origin","price_at_booking","cancelled","property_id")], \(x) class(x)[1])
  )
  readr::write_csv(schema_ok, file.path(tab_dir, "tbl1_schema_check.csv"))
  readr::write_csv(df,        file.path(dat_dir, "synthetic_bookings.csv"))
  
  stopifnot(inherits(df$arrival_date, "Date"))
  stopifnot(inherits(df$booking_ts, "POSIXct"))
  stopifnot(all(as.Date(df$booking_ts) <= df$arrival_date, na.rm = TRUE))
  
  # ----- 2) histograms and pickup -----
  Lk <- leadtime_histograms(df, group_cols = c("property_id"), max_lead_days = 60)
  Lk_pickup <- pickup_curve(Lk, group_cols = c("property_id"))
  
  # ----- 3) divergence series -----
  D_adj <- adjacent_divergence_series(Lk, group_cols = c("property_id"))
  D_yoy <- tryCatch(yoy_divergence_series(Lk, group_cols = c("property_id")),
                    error = function(e) tibble())
  
  p_div_adj <- ggplot(D_adj, aes(month, D)) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ property_id, ncol = 1) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    labs(x = "Month", y = "Normalized L1 divergence",
         title = "Adjacent-month divergence D(L_t, L_{t-1})") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          strip.text  = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  save_png(p_div_adj, file.path(fig_dir, "fig1_divergence_adjacent.png"))
  
  if (nrow(D_yoy) > 0) {
    p_div_yoy <- ggplot(D_yoy, aes(month, D)) +
      geom_line(linewidth = 0.9) +
      facet_wrap(~ property_id, ncol = 1) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
      labs(x = "Month", y = "Normalized L1 divergence", title = "Year-over-year divergence") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            strip.text  = element_text(face = "bold"),
            panel.grid.minor = element_blank())
    save_png(p_div_yoy, file.path(fig_dir, "fig1_divergence_yoy.png"))
  }
  
  div_summary <- D_adj |>
    dplyr::group_by(property_id) |>
    dplyr::summarise(
      n_months = dplyr::n(),
      D_mean   = mean(D, na.rm = TRUE),
      D_median = median(D, na.rm = TRUE),
      D_p90    = as.numeric(stats::quantile(D, 0.90, na.rm = TRUE)),
      .groups = "drop"
    )
  readr::write_csv(div_summary, file.path(tab_dir, "tbl3_divergence_summary.csv"))
  
  D_est <- if (nrow(D_yoy) > 0) {
    as.numeric(stats::quantile(D_yoy$D, 0.90, na.rm = TRUE))
  } else {
    safe_divergence_quantile(D_adj, probs = 0.90, default = 0.20)
  }
  
  # ----- 4) pickup and risk for latest cohort of each property -----
  latest_by_prop <- Lk_pickup |>
    dplyr::group_by(property_id) |>
    dplyr::summarise(latest_month = max(month), .groups = "drop")
  
  risk_rows <- list()
  for (i in seq_len(nrow(latest_by_prop))) {
    pid <- latest_by_prop$property_id[i]
    mm  <- latest_by_prop$latest_month[i]
    cohort <- subset(Lk_pickup, property_id == pid & month == mm)
    for (d in c(7L, 14L, 21L)) {
      Ch <- cohort$Chist[cohort$k == d][1]
      if (is.finite(Ch) && Ch > 0) {
        b <- relative_error_bound(D = D_est, delta = d, delta_max = 60, Chist_delta = Ch)
        act <- map_actions(b)
        risk_rows[[length(risk_rows) + 1L]] <- data.frame(
          property_id = pid, month = mm, delta_days = d,
          Chist = round(Ch, 3), bound = round(b, 3),
          price_cadence = act$price_cadence,
          advance_purchase_buffer_days = act$advance_purchase_buffer_days,
          staffing_buffer_pct = act$staffing_buffer_pct
        )
      }
    }
  }
  risk_tbl <- dplyr::bind_rows(risk_rows)
  readr::write_csv(risk_tbl, file.path(tab_dir, "tbl2_risk_latest_month.csv"))
  
  # ----- 5) pickup + histogram figures for newest property-month -----
  first_pid <- dplyr::first(unique(Lk_pickup$property_id))
  latest_month <- latest_by_prop$latest_month[latest_by_prop$property_id == first_pid]
  cohort_plot <- subset(Lk_pickup, property_id == first_pid & month == latest_month)
  
  p_pickup <- ggplot(cohort_plot, aes(k, Chist)) +
    geom_line() +
    labs(x = "Days before arrival (k)", y = "Cumulative pickup C_hist(k)",
         title = paste0("Pickup curve, property ", first_pid, ", month ", latest_month))
  save_png(p_pickup, file.path(fig_dir, "fig2_pickup_curves_latest.png"))
  
  p_hist <- ggplot(cohort_plot, aes(k, Lk)) +
    geom_col() +
    labs(x = "Days before arrival (k)", y = "Probability mass L(k)",
         title = paste0("Lead-time histogram, property ", first_pid, ", month ", latest_month))
  save_png(p_hist, file.path(fig_dir, "fig3_leadtime_histograms_latest.png"))
  
  # ----- 6) session info -----
  sink(file.path(meta_dir, "session_info.txt"))
  cat("leadtimefluxR version:", as.character(utils::packageVersion("leadtimefluxR")), "\n\n")
  print(sessionInfo())
  sink()
  
  invisible(normalizePath(out_dir))
}
