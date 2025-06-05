library(distionary)

test_that("log_distribution works with continuous distributions", {
  d <- dst_unif(1, 10)
  log_d <- log_distribution(d, base = 10)
  expect_s3_class(log_d, "logarithmic")
  expect_equal(eval_cdf(log_d, at = log10(5)), eval_cdf(d, at = 5))
})

test_that("log_distribution handles edge cases", {
  d <- dst_unif(1, 10)
  expect_error(log_distribution(d, base = -1)) # Invalid base
  expect_error(log_distribution(d, base = 0)) # Invalid base
  expect_error(log_distribution(d, base = NA_real_)) # Invalid base
  expect_error(log_distribution(dst_unif(-1, 10))) # Negative range
  
  # Test with a discrete distribution having mass at 0
  discrete_at_zero <- dst_binom(size = 10, prob = 0.5)
  expect_error(expect_warning(log_distribution(discrete_at_zero)))
})
