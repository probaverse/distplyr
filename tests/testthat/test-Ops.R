library(distionary)

test_that("addition operator works correctly", {
  # Create test distributions
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)

  # Test distribution + constant
  result1 <- gamma_dist + 5
  expected <- shift(gamma_dist, 5)
  expect_equal(result1, expected)

  # Test constant + distribution
  result2 <- 5 + gamma_dist
  expect_equal(result2, expected)

  # Test `+` as unary operator.
  result3 <- +gamma_dist
  expect_equal(result3, gamma_dist)
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

  # Test distribution * -constant
  result3 <- gamma_dist * -5
  expect_equal(result3, multiply(gamma_dist, -5))

  # Test -constant * distribution
  result4 <- -5 * gamma_dist
  expect_equal(result4, multiply(gamma_dist, -5))
})

test_that("division operator works correctly", {
  # Create test distributions
  gamma_dist <- dst_gamma(shape = 2, rate = 0.5)

  # Test distribution / constant
  result1 <- gamma_dist / 5
  expect_equal(result1, multiply(gamma_dist, 1 / 5))

  # Test 1 / distribution
  result2 <- 5 / gamma_dist
  expect_equal(result2, multiply(invert(gamma_dist), 5))

  # Test constant / distribution with positive constant
  result3 <- 5 / gamma_dist
  expect_equal(result3, multiply(invert(gamma_dist), 5))

  # Test constant / distribution with negative constant
  result4 <- -5 / gamma_dist
  expect_equal(result4, multiply(invert(gamma_dist), -5))
  plot(result4, "density", from = -5, to = 0, n = 1000)
  plot(multiply(invert(gamma_dist), -5), "density", from = -5, to = 0, n = 1000)

  # Test 0 / distribution
  result5 <- 0 / gamma_dist
  expect_equal(result5, dst_degenerate(0))
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

  # n^d = exp(log(n)*d)
  result4 <- 2.5^unif_dist
  expected4 <- exp(multiply(unif_dist, log(2.5)))
  expect_equal(result4, expected4)

  # Test error cases
  expect_error(unif_dist^"a")
  expect_error((-2)^unif_dist)
  expect_error(0^unif_dist)
  expect_error(dst_norm(0, 1)^2)
  expect_error(0 / dst_pois(1))
})

test_that("Two distributions input into a binary operator throws an error.", {
  d1 <- dst_exp(5)
  d2 <- dst_weibull(5, 3)
  expect_error(d1 + d2)
  expect_error(d1 - d2)
  expect_error(d1 * d2)
  expect_error(d1 / d2)
  expect_error(d1 ^ d2)
})
