library(testthat)
library(distionary)
library(distplyr)

test_that("log_distribution works with continuous distributions", {
  d <- distionary::dst_unif(1, 10)
  log_d <- log_distribution(d, base = 10)
  expect_s3_class(log_d, "logarithmic")
  expect_equal(eval_cdf(log_d, at = log10(5)), distionary::eval_cdf(d, at = 5))
})

test_that("log_distribution handles edge cases", {
  d <- distionary::dst_unif(1, 10)
  expect_error(log_distribution(d, base = -1)) # Invalid base
  expect_error(log_distribution(distionary::dst_unif(-1, 10))) # Negative range
})
