library(distionary)

test_that("Flip - special cases - light testing", {
  # Special implementation for Normal distribution
  norm_dist <- dst_norm(2, 3)
  norm_trns <- flip(norm_dist)
  expect_equal(pretty_name(norm_trns), "Normal")
  expect_equal(parameters(norm_trns)[["mean"]], -2)
  expect_equal(parameters(norm_trns)[["sd"]], 3)

  # Special implementation for Uniform distribution
  unif_dist <- dst_unif(1, 5)
  unif_trns <- flip(unif_dist)
  expect_equal(pretty_name(unif_trns), "Uniform")
  expect_equal(parameters(unif_trns)[["min"]], -5)
  expect_equal(parameters(unif_trns)[["max"]], -1)

  # Special implementation for Cauchy distribution
  cauchy_dist <- dst_cauchy(location = -1, scale = 2)
  cauchy_trns <- flip(cauchy_dist)
  expect_equal(pretty_name(cauchy_trns), "Cauchy")
  expect_equal(parameters(cauchy_trns)[["location"]], 1)
  expect_equal(parameters(cauchy_trns)[["scale"]], 2)

  # Special implementation for Student t distribution
  t_dist <- dst_t(5)
  t_trns <- flip(t_dist)
  expect_equal(pretty_name(t_trns), "Student t")
  expect_equal(parameters(t_trns)[["df"]], 5)

  # Special implementation for Finite
  finite_dist <- dst_empirical(c(-9, 1, 2, 3), weights = 1:4 / 10)
  finite_trns <- flip(finite_dist)
  expect_equal(pretty_name(finite_trns), "Finite")
  expect_equal(parameters(finite_trns)[["outcomes"]], c(-3, -2, -1, 9))
  expect_equal(parameters(finite_trns)[["probs"]], 4:1 / 10)

  # Special implementation for Degenerate
  finite_dist <- dst_degenerate(3)
  finite_trns <- flip(finite_dist)
  expect_equal(pretty_name(finite_trns), "Degenerate")
  expect_equal(parameters(finite_trns)[["location"]], -3)

  # Double flipping
  expect_equal(flip(flip(dst_nbinom(3, 0.5))), dst_nbinom(3, 0.5))
})


test_that("Flip - bad parameters", {
  expect_error(flip(5))
})

test_that("Flip correctly executes right-inverse when flat CDF.", {
  d <- mix(dst_unif(-2, -1), dst_unif(2, 4))
  flipped <- flip(d)
  expect_equal(eval_quantile(flipped, at = 0.5), -2)  # Not 1
})
