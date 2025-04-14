library(testthat)
library(distionary)
library(distplyr)

test_that("slice_left works with continuous distributions", {
  d <- distionary::dst_unif(0, 10)
  sliced <- slice_left(d, 5)
  expect_s3_class(sliced, "slice_left")
  expect_equal(range(sliced)[1], 5)
  expect_equal(eval_cdf(sliced, at = 5), 0)
})

test_that("slice_left works with discrete distributions", {
  d <- distionary::dst_pois(3)
  sliced <- slice_left(d, 4)
  expect_s3_class(sliced, "slice_left")
  expect_equal(range(sliced)[1], 4)
  expect_equal(eval_pmf(sliced, at = 4), distionary::eval_pmf(d, at = 4) / distionary::prob_right(d, of = 4))
})

test_that("slice_left handles edge cases", {
  d <- distionary::dst_unif(0, 10)
  expect_equal(slice_left(d, -5), d) # Breakpoint below range
  expect_s3_class(slice_left(d, 15), "dst_null") # Breakpoint beyond range
  expect_s3_class(slice_left(d, 10, include = FALSE), "dst_null") # Exclude upper bound
})
