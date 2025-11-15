# Quick start using leadtimefluxR
# install.packages(c("devtools","dplyr","tidyr","lubridate","ggplot2"))
# devtools::load_all("leadtimefluxR")  # from package root

library(leadtimefluxR)

# 1) generate demo data
df <- generate_synthetic_bookings(start_date = "2022-01-01", end_date = "2022-06-30",
                                  avg_bookings_per_day = 20, properties = 3,
                                  max_lead_days = 60, compression_level = 0.4, seed = 123)

# 2) compute L(k) by property and month and pickup curve
Lk <- leadtime_histograms(df, group_cols = c("property_id"), max_lead_days = 60)
Lk_pickup <- pickup_curve(Lk, group_cols = c("property_id"))

# 3) pick a cohort-month and get C_hist at 14 days
cohort <- subset(Lk_pickup, property_id == "P001" & month == as.Date("2022-06-01"))
Chist_14 <- cohort$Chist[cohort$k == 14][1]

# 4) year-over-year divergence series
D_yoy <- yoy_divergence_series(Lk, group_cols = c("property_id"))
D_est <- stats::quantile(D_yoy$D, probs = 0.9, na.rm = TRUE)

# 5) compute error bound and recommend actions
bound <- relative_error_bound(D = as.numeric(D_est), delta = 14, delta_max = 60, Chist_delta = Chist_14)
actions <- recommend_actions(bound)
print(list(bound = bound, actions = actions))
