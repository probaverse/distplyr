library(testthat)
library(distionary)
library(distplyr)

test_that("multiply works with continuous distributions", {
  d <- distionary::dst_norm(0, 1)
  scaled <- multiply(d, 2)
  expect_s3_class(scaled, "scaled")
  expect_equal(mean(scaled), mean(d) * 2)
})

test_that("multiply works with discrete distributions", {
  d <- distionary::dst_pois(3)
  scaled <- multiply(d, 2)
  expect_s3_class(scaled, "scaled")
  expect_equal(eval_pmf(scaled, at = 6), distionary::eval_pmf(d, at = 3))
})

test_that("multiply handles edge cases", {
  d <- distionary::dst_unif(0, 10)
  expect_s3_class(multiply(d, 0), "dst_degenerate") # Multiply by zero
  expect_error(multiply(d, Inf)) # Multiply by infinity
})
