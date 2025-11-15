# scripts/make_paper_artifacts.R
# Reproducible artifact builder for the commentary
# Outputs:
#   paper_artifacts/
#     figures/
#       fig1_divergence_adjacent.png
#       fig1_divergence_yoy.png        # only when >= 13 months
#       fig2_pickup_curves_latest.png
#       fig3_leadtime_histograms_latest.png
#     tables/
#       tbl1_schema_check.csv
#       tbl2_risk_latest_month.csv
#       tbl3_divergence_summary.csv
#     data/
#       synthetic_bookings.csv
#     metadata/
#       session_info.txt

suppressPackageStartupMessages({
  library(leadtimefluxR)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(readr)
})

# --------------------------------------------------------------------
# 0) Paths and helpers
# --------------------------------------------------------------------
out_dir  <- file.path("paper_artifacts")
fig_dir  <- file.path(out_dir, "figures")
tab_dir  <- file.path(out_dir, "tables")
dat_dir  <- file.path(out_dir, "data")
meta_dir <- file.path(out_dir, "metadata")
dirs <- c(out_dir, fig_dir, tab_dir, dat_dir, meta_dir)
invisible(lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE))

save_png <- function(p, path, width = 1800, height = 1200, res = 220) {
  png(path, width = width, height = height, res = res)
  print(p)
  dev.off()
}

# Action mapping, mirrored to what you used interactively
map_actions <- function(bound) {
  price_cadence <- if (bound >= 0.20) "intraday" else if (bound >= 0.10) "daily" else "weekly"
  advance_purchase_buffer_days <- if (bound >= 0.20) 0L else if (bound >= 0.10) 3L else 7L
  staffing_buffer_pct <- if (bound >= 0.20) 0.15 else if (bound >= 0.10) 0.10 else 0.05
  list(price_cadence = price_cadence,
       advance_purchase_buffer_days = advance_purchase_buffer_days,
       staffing_buffer_pct = staffing_buffer_pct)
}

# --------------------------------------------------------------------
# 1) Generate fully synthetic data for 24 months to allow YoY
# --------------------------------------------------------------------
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

# Hard checks that also document the schema in Table 1
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
write_csv(schema_ok, file.path(tab_dir, "tbl1_schema_check.csv"))

stopifnot(inherits(df$arrival_date, "Date"))
stopifnot(inherits(df$booking_ts, "POSIXct"))
stopifnot(all(as.Date(df$booking_ts) <= df$arrival_date, na.rm = TRUE))

# Save the synthetic dataset for full reproducibility
write_csv(df, file.path(dat_dir, "synthetic_bookings.csv"))

# --------------------------------------------------------------------
# 2) Lead-time histograms and pickup
# --------------------------------------------------------------------
Lk <- leadtimefluxR::leadtime_histograms(df, group_cols = c("property_id"), max_lead_days = 60)
Lk_pickup <- leadtimefluxR::pickup_curve(Lk, group_cols = c("property_id"))

# --------------------------------------------------------------------
# 3) Divergence series: Adjacent always, YoY when >= 13 months
# --------------------------------------------------------------------
D_adj <- getFromNamespace("adjacent_divergence_series", "leadtimefluxR")(Lk, group_cols = c("property_id"))
# or

# Plot adjacent divergence
p_div_adj <- ggplot(D_adj, aes(x = month, y = D)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ property_id, ncol = 1, scales = "fixed") +
  scale_x_date(
    date_breaks = "3 months",          # label every 3rd month
    date_labels = "%b\n%Y"             # abbreviated month + newline + year
  ) +
  labs(x = "Month", y = "Normalized L1 divergence",
       title = "Adjacent‑month divergence D(Lₜ, Lₜ₋₁)") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text   = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p_div_adj

save_png(p_div_adj, file.path(fig_dir, "fig1_divergence_adjacent.png"))

# YoY divergence if possible
D_yoy <- tryCatch(
  leadtimefluxR::yoy_divergence_series(Lk, group_cols = c("property_id")),
  error = function(e) tibble()
)
if (nrow(D_yoy) > 0) {
  p_div_yoy <- ggplot(D_yoy, aes(x = month, y = D)) +
    geom_line() +
    facet_wrap(~ property_id, scales = "free_y") +
    labs(x = "Month", y = "Normalized L1 divergence",
         title = "Year-over-year divergence")
  save_png(p_div_yoy, file.path(fig_dir, "fig1_divergence_yoy.png"))
}

# Summarize divergence for Table 3
div_summary <- D_adj %>%
  group_by(property_id) %>%
  summarise(n_months = dplyr::n(),
            D_mean = mean(D, na.rm = TRUE),
            D_median = median(D, na.rm = TRUE),
            D_p90 = as.numeric(quantile(D, 0.90, na.rm = TRUE)),
            .groups = "drop")
write_csv(div_summary, file.path(tab_dir, "tbl3_divergence_summary.csv"))

# Use a conservative D estimate that works for both long and short samples
D_est <- if (nrow(D_yoy) > 0) {
  as.numeric(stats::quantile(D_yoy$D, 0.90, na.rm = TRUE))
} else {
  leadtimefluxR::safe_divergence_quantile(D_adj, probs = 0.90, default = 0.20)
}

# --------------------------------------------------------------------
# 4) Pickup and risk for the latest cohort of each property
# --------------------------------------------------------------------
latest_by_prop <- Lk_pickup %>%
  group_by(property_id) %>%
  summarise(latest_month = max(month), .groups = "drop")

risk_rows <- list()
for (i in seq_len(nrow(latest_by_prop))) {
  pid <- latest_by_prop$property_id[i]
  mm  <- latest_by_prop$latest_month[i]
  cohort <- subset(Lk_pickup, property_id == pid & month == mm)
  
  for (d in c(7L, 14L, 21L)) {
    Ch <- cohort$Chist[cohort$k == d][1]
    if (is.finite(Ch) && Ch > 0) {
      b <- leadtimefluxR::relative_error_bound(D = D_est, delta = d, delta_max = 60, Chist_delta = Ch)
      act <- map_actions(b)
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
risk_tbl <- bind_rows(risk_rows)
write_csv(risk_tbl, file.path(tab_dir, "tbl2_risk_latest_month.csv"))

# --------------------------------------------------------------------
# 5) Figures for pickup and lead-time distribution on the latest cohort
# --------------------------------------------------------------------
first_pid <- dplyr::first(unique(Lk_pickup$property_id))
latest_month <- latest_by_prop$latest_month[latest_by_prop$property_id == first_pid]
cohort_plot <- subset(Lk_pickup, property_id == first_pid & month == latest_month)

p_pickup <- ggplot(cohort_plot, aes(x = k, y = Chist)) +
  geom_line() +
  labs(x = "Days before arrival (k)", y = "Cumulative pickup C_hist(k)",
       title = paste0("Pickup curve, property ", first_pid, ", month ", latest_month))

save_png(p_pickup, file.path(fig_dir, "fig2_pickup_curves_latest.png"))

p_hist <- ggplot(cohort_plot, aes(x = k, y = Lk)) +
  geom_col() +
  labs(x = "Days before arrival (k)", y = "Probability mass L(k)",
       title = paste0("Lead-time histogram, property ", first_pid, ", month ", latest_month))

save_png(p_hist, file.path(fig_dir, "fig3_leadtime_histograms_latest.png"))

# --------------------------------------------------------------------
# 6) Session info for reproducibility
# --------------------------------------------------------------------
sink(file.path(meta_dir, "session_info.txt"))
cat("leadtimefluxR version:", as.character(utils::packageVersion("leadtimefluxR")), "\n\n")
print(sessionInfo())
sink()

message("Artifacts written to: ", normalizePath(out_dir))
