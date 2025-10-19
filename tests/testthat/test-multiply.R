library(distionary)

test_that("Multiply - special cases - light testing", {
  # Special implementation for Normal distribution
  norm_dist <- dst_norm(2, 3)
  norm_mult <- multiply(norm_dist, 5)
  expect_equal(pretty_name(norm_mult), "Normal")
  expect_equal(parameters(norm_mult)[["mean"]], 2 * 5)
  expect_equal(parameters(norm_mult)[["sd"]], 3 * 5)

  # Special implementation for Uniform distribution
  unif_dist <- dst_unif(1, 5)
  unif_mult <- multiply(unif_dist, 10)
  expect_equal(pretty_name(unif_mult), "Uniform")
  expect_equal(parameters(unif_mult)[["min"]], 10)
  expect_equal(parameters(unif_mult)[["max"]], 50)

  # Special implementation for Cauchy distribution
  cauchy_dist <- dst_cauchy(location = -1, scale = 2)
  cauchy_mult <- multiply(cauchy_dist, 3)
  expect_equal(pretty_name(cauchy_mult), "Cauchy")
  expect_equal(parameters(cauchy_mult)[["location"]], -3)
  expect_equal(parameters(cauchy_mult)[["scale"]], 6)

  # Special implementation for Generalised Pareto distribution
  gp_dist <- dst_gp(scale = 2, shape = 0.5)
  gp_mult <- multiply(gp_dist, 4)
  expect_equal(pretty_name(gp_mult), "Generalised Pareto")
  expect_equal(parameters(gp_mult)[["scale"]], 2 * 4)
  expect_equal(parameters(gp_mult)[["shape"]], 0.5)

  # Special implementation for Generalised Extreme Value distribution
  gev_dist <- dst_gev(location = 1, scale = 2, shape = 0.5)
  gev_mult <- multiply(gev_dist, 4)
  expect_equal(pretty_name(gev_mult), "Generalised Extreme Value")
  expect_equal(parameters(gev_mult)[["location"]], 1 * 4)
  expect_equal(parameters(gev_mult)[["scale"]], 2 * 4)
  expect_equal(parameters(gev_mult)[["shape"]], 0.5)

  # Special implementation for Finite
  finite_dist <- dst_empirical(c(1, 2, 3), weights = c(0.2, 0.5, 0.3))
  finite_mult <- multiply(finite_dist, 4)
  expect_equal(pretty_name(finite_mult), "Finite")
  expect_equal(parameters(finite_mult)[["outcomes"]], c(4, 8, 12))
  expect_equal(parameters(finite_mult)[["probs"]], c(0.2, 0.5, 0.3))
})


test_that("Multiply - Edge cases", {
  d <- dst_exp(1.1)
  expect_equal(multiply(d, 0), dst_degenerate(0))
})

test_that("Multiply - bad parameters", {
  expect_error(multiply(dst_exp(1), Inf))
  expect_error(multiply(dst_exp(1), -Inf))
  expect_error(multiply(dst_exp(1), "a"))
  expect_error(multiply(5, 5))
  # Length
  expect_error(multiply(dst_exp(3), numeric(0)))
  expect_error(multiply(dst_exp(3), 1:4))
})
