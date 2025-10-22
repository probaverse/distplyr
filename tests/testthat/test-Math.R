library(distionary)

test_that("log function works correctly", {
  # Create test distributions
  unif_dist <- dst_unif(1, 10)
  exp_dist <- dst_exp(0.5)

  # Test log with default base (natural log)
  expect_equal(
    log(unif_dist),
    log_distribution(unif_dist)
  )

  # Test log with specified base
  expect_equal(
    log(exp_dist, base = 2),
    log_distribution(exp_dist, base = 2)
  )

  # Test log10
  expect_equal(
    log10(unif_dist),
    log_distribution(unif_dist, base = 10)
  )
})

test_that("exp function works correctly", {
  # Create test distributions
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)

  # Test exp
  expect_equal(
    exp(gamma_dist),
    exp_distribution(gamma_dist)
  )
})

test_that("unsupported Math functions raise errors", {
  # Create test distribution
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)

  # Test an unsupported Math function
  expect_error(sin(gamma_dist))
  expect_error(sqrt(gamma_dist))
  expect_error(log(gamma_dist - 5))
})
