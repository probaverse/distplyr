library(distionary)

test_that("addition operator works correctly", {
  # Create test distributions
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)

  # Test distribution + constant
  result1 <- gamma_dist + 5
  expect_equal(result1, shift(gamma_dist, 5))

  # Test constant + distribution
  result2 <- 5 + gamma_dist
  expect_equal(result2, shift(gamma_dist, 5))

  # Test that both approaches give same result
  expect_equal(result1, result2)
})

test_that("subtraction operator works correctly", {
  # Create test distributions
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)

  # Test distribution - constant
  result1 <- gamma_dist - 5
  expect_equal(result1, shift(gamma_dist, -5))

  # Test constant - distribution
  result2 <- 5 - gamma_dist
  expect_equal(result2, shift(flip(gamma_dist), 5))

  # Test unary minus (negation)
  result3 <- -gamma_dist
  expect_equal(result3, flip(gamma_dist))
})

test_that("multiplication operator works correctly", {
  # Create test distributions
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)

  # Test distribution * constant
  result1 <- gamma_dist * 5
  expect_equal(result1, multiply(gamma_dist, 5))

  # Test constant * distribution
  result2 <- 5 * gamma_dist
  expect_equal(result2, multiply(gamma_dist, 5))
})

test_that("division operator works correctly", {
  # Create test distributions
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)

  # Test distribution / constant
  result1 <- gamma_dist / 5
  expect_equal(result1, multiply(gamma_dist, 1 / 5))

  # Test constant / distribution with positive constant
  result2 <- 5 / gamma_dist
  expect_equal(result2, invert(multiply(gamma_dist, 1 / 5)))

  # Test constant / distribution with negative constant
  result3 <- -5 / gamma_dist
  expect_equal(result3, invert(flip(multiply(gamma_dist, 5))))

  # Test constant / distribution with zero constant
  result4 <- 0 / gamma_dist
  expect_equal(result4, dst_degenerate(0))
})

test_that("exponentiation operator works correctly", {
  unif_dist <- dst_unif(0, 5)

  # Test distribution ^ constant
  # d^0 = 1
  result1 <- unif_dist^0
  expect_equal(result1, dst_degenerate(1))

  # d^1 = d
  result2 <- unif_dist^1
  expect_equal(result2, unif_dist)

  # d^n = exp(n*log(d))
  result3 <- unif_dist^2.1
  expected3 <- exp_distribution(multiply(log_distribution(unif_dist), 2.1))
  expect_equal(result3, expected3)
  expect_equal(
    eval_cdf(result3, at = 1:25),
    eval_cdf(unif_dist, at = (1:25)^(1/2.1))
  )

  # Test error cases
  expect_error(unif_dist^"a")
  expect_error((-2)^unif_dist)
  expect_error(0^unif_dist)
  expect_error(dst_norm(0, 1)^2)
})
