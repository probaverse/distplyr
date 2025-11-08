library(distionary)

test_that("Log - special cases - light testing", {
  # Special implementation for Normal distribution
  lnorm_dist <- dst_lnorm(2, 3)
  lnorm_trns <- log(lnorm_dist)
  expect_equal(pretty_name(lnorm_trns), "Normal")
  expect_equal(parameters(lnorm_trns)[["mean"]], 2)
  expect_equal(parameters(lnorm_trns)[["sd"]], 3)

  # Special implementation for Finite
  finite_dist <- dst_empirical(c(1, 2, 3), weights = c(0.2, 0.5, 0.3))
  finite_trns <- log(finite_dist)
  expect_equal(pretty_name(finite_trns), "Finite")
  expect_equal(parameters(finite_trns)[["outcomes"]], log(1:3))
  expect_equal(parameters(finite_trns)[["probs"]], c(0.2, 0.5, 0.3))

  # Special implementation for Degenerate
  deg_dist <- dst_degenerate(5)
  deg_trns <- log10(deg_dist)
  expect_equal(pretty_name(deg_trns), "Degenerate")
  expect_equal(parameters(deg_trns)[["location"]], log10(5))
})


test_that("Log - Edge cases", {
  d <- dst_exp(1.1)
  expect_equal(log(d, 0), dst_degenerate(0))
})

test_that("Log - bad parameters", {
  expect_error(log(dst_exp(1), Inf))
  expect_error(log(dst_exp(1), -Inf))
  expect_error(log(dst_exp(1), -1))
  expect_error(log(dst_exp(1), "a"))
  expect_error(log_distribution(5, 5))
  # Length
  expect_error(log(dst_exp(3), numeric(0)))
  expect_error(log(dst_exp(3), 1:4))
  # Distributions that don't work with this verb.
  expect_error(log(dst_norm(0, 1)))
  expect_error(log(dst_degenerate(0)))
  expect_error(log(dst_pois(1)))
})
