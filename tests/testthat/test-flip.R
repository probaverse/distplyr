library(distionary)

test_that("Flip - special cases - light testing", {
  # Special implementation for Normal distribution
  norm_dist <- dst_norm(2, 3)
  norm_trns <- flip(norm_dist, 5)
  expect_equal(pretty_name(norm_trns), "Normal")
  expect_equal(parameters(norm_trns)[["mean"]], 7)
  expect_equal(parameters(norm_trns)[["sd"]], 3)

  # Special implementation for Uniform distribution
  unif_dist <- dst_unif(1, 5)
  unif_trns <- flip(unif_dist, 10)
  expect_equal(pretty_name(unif_trns), "Uniform")
  expect_equal(parameters(unif_trns)[["min"]], 11)
  expect_equal(parameters(unif_trns)[["max"]], 15)

  # Special implementation for Cauchy distribution
  cauchy_dist <- dst_cauchy(location = 0, scale = 2)
  cauchy_trns <- flip(cauchy_dist, 3)
  expect_equal(pretty_name(cauchy_trns), "Cauchy")
  expect_equal(parameters(cauchy_trns)[["location"]], 3)
  expect_equal(parameters(cauchy_trns)[["scale"]], 2)

  # Special implementation for Student t distribution
  t_dist <- dst_t(5)
  t_trns <- flip(t_dist)
  expect_equal(pretty_name(t_trns), "Student t")
  expect_equal(parameters(t_trns)[["df"]], 5)

  # Special implementation for Finite
  finite_dist <- dst_empirical(c(1, 2, 3), weights = c(0.2, 0.5, 0.3))
  finite_trns <- flip(finite_dist, 4)
  expect_equal(pretty_name(finite_trns), "Finite")
  expect_equal(parameters(finite_trns)[["outcomes"]], c(5, 6, 7))
  expect_equal(parameters(finite_trns)[["probs"]], c(0.2, 0.5, 0.3))

  # Special implementation for Degenerate
  finite_dist <- dst_degenerate(3)
  finite_trns <- flip(finite_dist)
  expect_equal(pretty_name(finite_trns), "Degenerate")
  expect_equal(parameters(finite_trns)[["location"]], -3)
})


test_that("Flip - Edge cases", {
  d <- dst_exp(1.1)
  expect_equal(flip(d, 0), d)
})

test_that("Flip - bad parameters", {
  expect_error(flip(dst_exp(1), Inf))
  expect_error(flip(dst_exp(1), -Inf))
  expect_error(flip(dst_exp(1), "a"))
  expect_error(flip(5, 5))
  # Length
  expect_error(flip(dst_exp(3), numeric(0)))
  expect_error(flip(dst_exp(3), 1:4))
})
