
# R/make_paper_artifacts_fn.R

#' Build all figures and tables for the commentary from synthetic data
#' @param out_dir Output directory to create and populate. Default "paper_artifacts".
#' @return Invisible normalized output path.
#' @export
make_paper_artifacts <- function(out_dir = "paper_artifacts") {
  # Create dirs
  fig_dir  <- file.path(out_dir, "figures")
  tab_dir  <- file.path(out_dir, "tables")
  dat_dir  <- file.path(out_dir, "data")
  meta_dir <- file.path(out_dir, "metadata")
  dirs <- c(out_dir, fig_dir, tab_dir, dat_dir, meta_dir)
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  # Helper to save ggplot as PNG
  save_png <- function(p, path, width = 1800, height = 1200, res = 220) {
    grDevices::png(path, width = width, height = height, res = res)
    print(p)
    grDevices::dev.off()
  }

  # 1) Synthetic data for 24 months so YoY is available
  set.seed(20251115)
  df <- leadtimefluxR::generate_synthetic_bookings(
    start_date = "2021-01-01",
    end_date   = "2022-12-31",
    avg_bookings_per_day = 20,
    properties = 3,
    max_lead_days = 60,
    compression_level = 0.4,
    seed = 123
  )

  # Schema check table
  schema_ok <- data.frame(
    column = c("arrival_date","booking_ts","stay_nights","channel","segment",
               "origin","price_at_booking","cancelled","property_id"),
    class  = c(class(df$arrival_date)[1],
               class(df$booking_ts)[1],
               class(df$stay_nights)[1],
               class(df$channel)[1],
               class(df$segment)[1],
               class(df$origin)[1],
               class(df$price_at_booking)[1],
               class(df$cancelled)[1],
               class(df$property_id)[1])
  )
  readr::write_csv(schema_ok, file.path(tab_dir, "tbl1_schema_check.csv"))
  # Save simulated data for reproducibility
  readr::write_csv(df, file.path(dat_dir, "synthetic_bookings.csv"))

  # 2) Lead-time histograms and pickup
  Lk <- leadtimefluxR::leadtime_histograms(df, group_cols = c("property_id"), max_lead_days = 60)
  Lk_pickup <- leadtimefluxR::pickup_curve(Lk, group_cols = c("property_id"))

  # 3) Divergence series
  D_adj <- leadtimefluxR::adjacent_divergence_series(Lk, group_cols = c("property_id"))
  D_yoy <- tryCatch(
    leadtimefluxR::yoy_divergence_series(Lk, group_cols = c("property_id")),
    error = function(e) dplyr::tibble()
  )

  # Plot adjacent divergence with readable x axis
  p_div_adj <- ggplot2::ggplot(D_adj, ggplot2::aes(x = month, y = D)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::facet_wrap(~ property_id, ncol = 1, scales = "fixed") +
    ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    ggplot2::labs(x = "Month", y = "Normalized L1 divergence",
                  title = "Adjacent-month divergence D(L_t, L_{t-1})") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                   strip.text = ggplot2::element_text(face = "bold"),
                   panel.grid.minor = ggplot2::element_blank())
  save_png(p_div_adj, file.path(fig_dir, "fig1_divergence_adjacent.png"))

  if (nrow(D_yoy) > 0) {
    p_div_yoy <- ggplot2::ggplot(D_yoy, ggplot2::aes(x = month, y = D)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~ property_id, ncol = 1, scales = "fixed") +
      ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
      ggplot2::labs(x = "Month", y = "Normalized L1 divergence",
                    title = "Year-over-year divergence") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
    save_png(p_div_yoy, file.path(fig_dir, "fig1_divergence_yoy.png"))
  }

  # Divergence summary table
  div_summary <- D_adj |>
    dplyr::group_by(property_id) |>
    dplyr::summarise(
      n_months = dplyr::n(),
      D_mean   = mean(D, na.rm = TRUE),
      D_median = stats::median(D, na.rm = TRUE),
      D_p90    = as.numeric(stats::quantile(D, 0.90, na.rm = TRUE)),
      .groups = "drop"
    )
  readr::write_csv(div_summary, file.path(tab_dir, "tbl3_divergence_summary.csv"))

  # D_est used for risk mapping
  D_est <- if (nrow(D_yoy) > 0) {
    as.numeric(stats::quantile(D_yoy$D, 0.90, na.rm = TRUE))
  } else {
    leadtimefluxR::safe_divergence_quantile(D_adj, probs = 0.90, default = 0.20)
  }

  # 4) Risk for latest cohort of each property
  latest_by_prop <- Lk_pickup |>
    dplyr::group_by(property_id) |>
    dplyr::summarise(latest_month = max(month), .groups = "drop")

  risk_rows <- list()
  for (i in seq_len(nrow(latest_by_prop))) {
    pid <- latest_by_prop$property_id[i]
    mm  <- latest_by_prop$latest_month[i]
    cohort <- subset(Lk_pickup, property_id == pid & month == mm)
    for (d in c(7L,14L,21L)) {
      Ch <- cohort$Chist[cohort$k == d][1]
      if (is.finite(Ch) && Ch > 0) {
        b <- leadtimefluxR::relative_error_bound(D = D_est, delta = d, delta_max = 60, Chist_delta = Ch)
        act <- leadtimefluxR::recommend_actions(b)
        risk_rows[[length(risk_rows) + 1L]] <- data.frame(
          property_id = pid,
          month = mm,
          delta_days = d,
          Chist = round(Ch, 3),
          bound = round(b, 3),
          price_cadence = act$price_cadence,
          advance_purchase_buffer_days = act$advance_purchase_buffer_days,
          staffing_buffer_pct = act$staffing_buffer_pct
        )
      }
    }
  }
  risk_tbl <- dplyr::bind_rows(risk_rows)
  readr::write_csv(risk_tbl, file.path(tab_dir, "tbl2_risk_latest_month.csv"))

  # 5) Pickup and histogram plots for one cohort
  first_pid <- dplyr::first(unique(Lk_pickup$property_id))
  latest_month <- latest_by_prop$latest_month[latest_by_prop$property_id == first_pid]
  cohort_plot <- subset(Lk_pickup, property_id == first_pid & month == latest_month)

  p_pickup <- ggplot2::ggplot(cohort_plot, ggplot2::aes(x = k, y = Chist)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Days before arrival (k)", y = "Cumulative pickup C_hist(k)",
                  title = paste0("Pickup curve, property ", first_pid, ", month ", latest_month))
  save_png(p_pickup, file.path(fig_dir, "fig2_pickup_curves_latest.png"))

  p_hist <- ggplot2::ggplot(cohort_plot, ggplot2::aes(x = k, y = Lk)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Days before arrival (k)", y = "Probability mass L(k)",
                  title = paste0("Lead-time histogram, property ", first_pid, ", month ", latest_month))
  save_png(p_hist, file.path(fig_dir, "fig3_leadtime_histograms_latest.png"))

  # 6) Session info
  con <- file(file.path(meta_dir, "session_info.txt"), open = "wt")
  writeLines(paste("leadtimefluxR version:", as.character(utils::packageVersion("leadtimefluxR"))), con)
  utils::capture.output(sessionInfo(), file = con, append = TRUE)
  close(con)

  invisible(normalizePath(out_dir))
}
