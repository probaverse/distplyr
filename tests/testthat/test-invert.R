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
