library(testthat)
library(distionary)
library(distplyr)

test_that("exp_distribution works correctly", {
  d <- dst_norm(0, 1)
  exp_d <- exp_distribution(d)
  expect_s3_class(exp_d, "exponential")
  expect_true(all(eval_cdf(exp_d, at = c(1, 2, 3)) >= 0))
})

test_that("slice_right works correctly", {
  d <- dst_unif(0, 10)
  sliced <- slice_right(d, 5)
  expect_s3_class(sliced, "slice_right")
  expect_equal(range(sliced)[2], 5)
})

test_that("slice_left works correctly", {
  d <- dst_unif(0, 10)
  sliced <- slice_left(d, 5)
  expect_s3_class(sliced, "slice_left")
  expect_equal(range(sliced)[1], 5)
})

test_that("shift works correctly", {
  d <- dst_norm(0, 1)
  shifted <- shift(d, 5)
  expect_s3_class(shifted, "shifted")
  expect_equal(mean(shifted), mean(d) + 5)
})

test_that("multiply works correctly", {
  d <- dst_norm(0, 1)
  scaled <- multiply(d, 2)
  expect_s3_class(scaled, "scaled")
  expect_equal(mean(scaled), mean(d) * 2)
})

test_that("mix works correctly", {
  d1 <- dst_norm(0, 1)
  d2 <- dst_norm(5, 1)
  mixed <- mix(d1, d2, weights = c(1, 2))
  expect_s3_class(mixed, "mixture")
  expect_equal(length(parameters(mixed)$distributions), 2)
})

test_that("minimize works correctly", {
  d1 <- dst_unif(0, 10)
  d2 <- dst_unif(5, 15)
  min_d <- minimize(d1, d2)
  expect_s3_class(min_d, "minimum")
  expect_true(range(min_d)[2] <= 10)
})

test_that("maximize works correctly", {
  d1 <- dst_unif(0, 10)
  d2 <- dst_unif(5, 15)
  max_d <- maximize(d1, d2)
  expect_s3_class(max_d, "maximum")
  expect_true(range(max_d)[1] >= 5)
})

test_that("log_distribution works correctly", {
  d <- dst_unif(1, 10)
  log_d <- log_distribution(d, base = 10)
  expect_s3_class(log_d, "logarithmic")
  expect_true(all(eval_cdf(log_d, at = c(0, 1, 2)) >= 0))
})

test_that("invert works correctly", {
  d <- dst_unif(1, 10)
  inv_d <- invert(d)
  expect_s3_class(inv_d, "inverse")
  expect_true(all(eval_cdf(inv_d, at = c(0.1, 0.5, 1)) >= 0))
})

test_that("graft_right works correctly", {
  base <- dst_unif(0, 10)
  graft <- dst_norm(10, 2)
  grafted <- graft_right(base, graft, breakpoint = 8)
  expect_s3_class(grafted, "graft")
  expect_true(range(grafted)[2] > 10)
})

test_that("graft_left works correctly", {
  base <- dst_unif(0, 10)
  graft <- dst_norm(-2, 1)
  grafted <- graft_left(base, graft, breakpoint = 2)
  expect_s3_class(grafted, "graft")
  expect_true(range(grafted)[1] < 0)
})

test_that("flip works correctly", {
  d <- dst_norm(0, 1)
  flipped <- flip(d)
  expect_s3_class(flipped, "negated")
  expect_equal(mean(flipped), -mean(d))
})
