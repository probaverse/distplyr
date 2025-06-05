library(distionary)

test_that("log function works correctly", {
  # Create test distributions
  unif_dist <- dst_unif(1, 10)
  exp_dist <- dst_exp(0.5)
  
  # Test log with default base (natural log)
  result1 <- log(unif_dist)
  expect_equal(result1, log_distribution(unif_dist))
  
  # Test log with specified base
  result2 <- log(unif_dist, base = 2)
  expect_equal(result2, log_distribution(unif_dist, base = 2))
})

test_that("log10 function works correctly", {
  # Create test distributions
  unif_dist <- dst_unif(1, 10)
  
  # Test log10
  result <- log10(unif_dist)
  expect_equal(result, log_distribution(unif_dist, base = 10))
})

test_that("exp function works correctly", {
  # Create test distributions
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)
  
  # Test exp
  result <- exp(gamma_dist)
  expect_equal(result, exp_distribution(gamma_dist))
})

test_that("unsupported Math functions raise errors", {
  # Create test distribution
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)
  
  # Test an unsupported Math function
  expect_error(sin(gamma_dist))
  expect_error(sqrt(gamma_dist))
})
