test_that("normalized L1 behaves as TV distance", {
  p <- c(0.5,0.3,0.2)
  q <- c(0.2,0.3,0.5)
  expect_equal(normalized_l1(p,q), 0.5 * sum(abs(p-q)))
  expect_equal(normalized_l1(p,p), 0)
})
