library(distionary)

test_that("Exp - special cases - light testing", {
  # Special implementation for Normal distribution
  norm_dist <- dst_norm(2, 3)
  norm_trns <- exp(norm_dist)
  expect_equal(pretty_name(norm_trns), "Log Normal")
  expect_equal(parameters(norm_trns)[["meanlog"]], 2)
  expect_equal(parameters(norm_trns)[["sdlog"]], 3)

  # Special implementation for Finite
  finite_dist <- dst_empirical(1:3, weights = c(0.2, 0.5, 0.3))
  finite_trns <- exp(finite_dist)
  expect_equal(pretty_name(finite_trns), "Finite")
  expect_equal(parameters(finite_trns)[["outcomes"]], exp(1:3))
  expect_equal(parameters(finite_trns)[["probs"]], c(0.2, 0.5, 0.3))

  # Special implementation for Degenerate
  finite_dist <- dst_degenerate(3)
  finite_trns <- exp(finite_dist)
  expect_equal(pretty_name(finite_trns), "Degenerate")
  expect_equal(parameters(finite_trns)[["location"]], exp(3))
})
