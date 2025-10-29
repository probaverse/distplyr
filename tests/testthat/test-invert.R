library(distionary)

test_that("Invert - special cases - light testing", {
  # Special implementation for Finite
  finite_dist <- dst_empirical(c(-3, 1, 2, 3), weights = 1:4 / 10)
  finite_trns <- invert(finite_dist)
  expect_equal(pretty_name(finite_trns), "Finite")
  expect_equal(parameters(finite_trns)[["outcomes"]], 1 / c(-3, 3, 2, 1))
  expect_equal(parameters(finite_trns)[["probs"]], c(1, 4, 3, 2) / 10)

  # Special implementation for Degenerate
  deg_dist <- dst_degenerate(5)
  deg_trns <- invert(deg_dist)
  expect_equal(pretty_name(deg_trns), "Degenerate")
  expect_equal(parameters(deg_trns)[["location"]], 1 / 5)

  # Double inverting
  expect_equal(invert(invert(dst_norm(1, 1))), dst_norm(1, 1))
})

test_that("Invert - bad parameters", {
  expect_error(invert(5))
  # Distributions that don't work with this verb.
  expect_error(invert(dst_degenerate(0)))
  expect_error(invert(dst_pois(1)))
})

test_that("Invert - ranges work.", {
  # There are complications depending on whether:
  # - The CDF crosses 0 and is flat in a neighbourhood there;
  # - The CDF crosses 0 and is strictly increasing there;
  # - The distribution is of positive values.
  d <- mix(dst_unif(-2, -1), dst_unif(2, 4))
  # Flat at 0.
  r <- range(1 / d)
  expect_equal(r, c(-1, 0.5))
  # Flat at 0, but then increases immediately after 0.
  r <- range(1 / (d - 2))
  expect_equal(r[1], -1 / 3)
  expect_gt(r[2], 10^9)
  # Flat at 0, but increasing immediately before 0.
  r <- range(1 / (d + 1))
  expect_lt(r[1], -10^9)
  expect_equal(r[2], 1 / 3)
  # Crosses 0 strictly increasing.
  r <- range(1 / dst_norm(0, 1))
  expect_lt(r[1], -10^9)
  expect_gt(r[2], 10^9)
  # Starts at 0 and increases from there.
  r <- range(1 / dst_exp(1))
  expect_equal(r, c(0, Inf))
  r <- range(1 / dst_beta(4, 2))
  expect_equal(r, c(1, Inf))
  # Increases until 0 and ends there.
  r <- range(1 / -dst_exp(1))
  expect_equal(r, c(-Inf, 0))
  r <- range(1 / -dst_beta(2, 4))
  expect_equal(r, c(-Inf, -1))
})
