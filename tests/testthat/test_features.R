test_that("leadtime histograms sum to 1 within cohort", {
  set.seed(1)
  df <- generate_synthetic_bookings(start_date = "2022-01-01", end_date = "2022-01-31",
                                    avg_bookings_per_day = 10, properties = 1,
                                    max_lead_days = 30, compression_level = 0.4)
  Lk <- leadtime_histograms(df, group_cols = c("property_id"), max_lead_days = 30)
  sums <- dplyr::summarise(dplyr::group_by(Lk, property_id, month), s = sum(Lk))
  expect_true(all(abs(sums$s - 1) < 1e-8))
})
