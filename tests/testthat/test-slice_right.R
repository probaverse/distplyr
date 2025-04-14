library(testthat)
library(distionary)
library(distplyr)

test_that("slice_right works with continuous distributions", {
  d <- distionary::dst_unif(0, 10)
  sliced <- slice_right(d, 5)
  expect_s3_class(sliced, "slice_right")
  expect_equal(range(sliced)[2], 5)
  expect_equal(eval_cdf(sliced, at = 5), 1)
})

test_that("slice_right works with discrete distributions", {
  d <- distionary::dst_pois(3)
  sliced <- slice_right(d, 2)
  expect_s3_class(sliced, "slice_right")
  expect_equal(range(sliced)[2], 2)
  expect_equal(eval_pmf(sliced, at = 2), distionary::eval_pmf(d, at = 2) / distionary::prob_left(d, of = 2))
})

test_that("slice_right handles edge cases", {
  d <- distionary::dst_unif(0, 10)
  expect_equal(slice_right(d, 15), d) # Breakpoint beyond range
  expect_s3_class(slice_right(d, -5), "dst_null") # Breakpoint below range
  expect_s3_class(slice_right(d, 0, include = FALSE), "dst_null") # Exclude lower bound
})
