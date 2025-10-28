library(distionary)

test_that("log function dispatches correctly", {
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

test_that("exp function dispatches correctly", {
  # Create test distributions
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)

  # Test exp
  expect_equal(
    exp(gamma_dist),
    exp_distribution(gamma_dist)
  )
})

test_that("sqrt function dispatches correctly", {
  # Create test distributions
  chi_sq_dist <- dst_chisq(df = 4)

  # Test sqrt
  expect_equal(
    sqrt(chi_sq_dist),
    chi_sq_dist^0.5
  )
})

test_that("Simplifications", {
  # Test that log(exp(X)) simplifies to X
  dist <- dst_beta(4, 3)
  expect_equal(log(exp(dist)), dist)
  expect_equal(exp(log(dist)), dist)

  # Different bases
  expect_equal(5^(log(dist, base = 5)), dist)
  expect_equal(log(5^dist, base = 5), dist)

  # Numeric powers simplification
  expect_equal(sqrt(dist^2), dist)
  expect_equal(sqrt(dist)^2, dist)
})
