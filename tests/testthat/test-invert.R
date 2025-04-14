library(testthat)
library(distionary)
library(distplyr)

test_that("invert works with continuous distributions", {
  d <- distionary::dst_unif(1, 10)
  inv_d <- invert(d)
  expect_s3_class(inv_d, "inverse")
  expect_equal(eval_cdf(inv_d, at = 0.5), distionary::eval_cdf(d, at = 1 / 0.5))
})

test_that("invert handles edge cases", {
  d <- distionary::dst_unif(1, 10)
  expect_error(invert(distionary::dst_unif(0, 10))) # Contains zero
})
