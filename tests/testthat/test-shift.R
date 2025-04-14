library(testthat)
library(distionary)
library(distplyr)

test_that("shift works with continuous distributions", {
  d <- distionary::dst_norm(0, 1)
  shifted <- shift(d, 5)
  expect_s3_class(shifted, "shifted")
  expect_equal(mean(shifted), mean(d) + 5)
})

test_that("shift works with discrete distributions", {
  d <- distionary::dst_pois(3)
  shifted <- shift(d, 2)
  expect_s3_class(shifted, "shifted")
  expect_equal(eval_pmf(shifted, at = 5), distionary::eval_pmf(d, at = 3))
})

test_that("shift handles edge cases", {
  d <- distionary::dst_unif(0, 10)
  expect_equal(shift(d, 0), d) # No shift
})
