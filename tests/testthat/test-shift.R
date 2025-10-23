library(distionary)

test_that("Shift - special cases - light testing", {
  # Special implementation for Normal distribution
  norm_dist <- dst_norm(2, 3)
  norm_trns <- shift(norm_dist, 5)
  expect_equal(pretty_name(norm_trns), "Normal")
  expect_equal(parameters(norm_trns)[["mean"]], 7)
  expect_equal(parameters(norm_trns)[["sd"]], 3)

  # Special implementation for Uniform distribution
  unif_dist <- dst_unif(1, 5)
  unif_trns <- shift(unif_dist, 10)
  expect_equal(pretty_name(unif_trns), "Uniform")
  expect_equal(parameters(unif_trns)[["min"]], 11)
  expect_equal(parameters(unif_trns)[["max"]], 15)

  # Special implementation for Cauchy distribution
  cauchy_dist <- dst_cauchy(location = 0, scale = 2)
  cauchy_trns <- shift(cauchy_dist, 3)
  expect_equal(pretty_name(cauchy_trns), "Cauchy")
  expect_equal(parameters(cauchy_trns)[["location"]], 3)
  expect_equal(parameters(cauchy_trns)[["scale"]], 2)

  # Special implementation for Generalised Extreme Value distribution
  gev_dist <- dst_gev(location = 1, scale = 2, shape = 0.5)
  gev_trns <- shift(gev_dist, 4)
  expect_equal(pretty_name(gev_trns), "Generalised Extreme Value")
  expect_equal(parameters(gev_trns)[["location"]], 5)
  expect_equal(parameters(gev_trns)[["scale"]], 2)
  expect_equal(parameters(gev_trns)[["shape"]], 0.5)

  # Special implementation for Finite
  finite_dist <- dst_empirical(c(1, 2, 3), weights = c(0.2, 0.5, 0.3))
  finite_trns <- shift(finite_dist, 4)
  expect_equal(pretty_name(finite_trns), "Finite")
  expect_equal(parameters(finite_trns)[["outcomes"]], c(5, 6, 7))
  expect_equal(parameters(finite_trns)[["probs"]], c(0.2, 0.5, 0.3))

  # Special implementation for Degenerate
  finite_dist <- dst_degenerate(3)
  finite_trns <- shift(finite_dist, 2)
  expect_equal(pretty_name(finite_trns), "Degenerate")
  expect_equal(parameters(finite_trns)[["location"]], 5)

  # Double shifting
  expect_equal(shift(shift(dst_pois(1), 5), -3), shift(dst_pois(1), 2))
  expect_equal(shift(shift(dst_pois(1), 3), -3), dst_pois(1))
})


test_that("Shift - Edge cases", {
  d <- dst_exp(1.1)
  expect_equal(shift(d, 0), d)
})

test_that("Shift - bad parameters", {
  expect_error(shift(dst_exp(1), Inf))
  expect_error(shift(dst_exp(1), -Inf))
  expect_error(shift(dst_exp(1), "a"))
  expect_error(shift(5, 5))
  # Length
  expect_error(shift(dst_exp(3), numeric(0)))
  expect_error(shift(dst_exp(3), 1:4))
})
