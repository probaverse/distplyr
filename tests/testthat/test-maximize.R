library(distionary)

test_that("cdf of max distribution makes sense.", {
  d_norm <- dst_norm(0, 1)
  d_pois <- dst_pois(1)
  x <- 1:10 / 5
  #' Expect all greater than
  #'
  #' For matching vector inputs, checks component-wise
  #' whether the object is greater than its expected
  #' counterpart.
  #' @param object,expected The two vectors to compare.
  #' @return Invisible; one call to `expect_gt()` per
  #' vector entry.
  expect_all_gt <- function(object, expected) {
    for (i in seq_along(object)) {
      expect_gt(object[i], expected[i])
    }
  }
  d_max <- maximise(d_norm, d_pois)
  expect_equal(eval_cdf(d_max, at = -0.01), 0)
  expect_all_gt(eval_cdf(d_norm, at = x), eval_cdf(d_max, at = x))
  expect_all_gt(eval_cdf(d_pois, at = x), eval_cdf(d_max, at = x))
  d_max2 <- maximise(d_pois, d_norm)
  expect_equal(eval_cdf(d_max, at = x), eval_cdf(d_max2, at = x))
  d_max3 <- maximise(d_norm, d_norm, d_pois)
  d_max3_nested <- maximise(d_max, d_norm)
  expect_equal(eval_cdf(d_max3, at = x), eval_cdf(d_max3_nested, at = x))
  expect_all_gt(eval_cdf(d_norm, at = x), eval_cdf(d_max3, at = x))
  expect_all_gt(eval_cdf(d_pois, at = x), eval_cdf(d_max3, at = x))
  d_max4 <- maximise(d_norm, d_pois, -1 - d_pois)
  expect_equal(eval_cdf(d_max, at = x), eval_cdf(d_max4, at = x))
})

test_that("draws works as expected", {
  d_norm <- dst_norm(0, 1)
  d_pois <- dst_pois(1)
  d_exp <- dst_exp(1)
  x <- 1:10 / 2
  d1a <- maximise(d_norm, d_pois, draws = 2)
  d1b <- maximise(d_norm, d_norm, d_pois, d_pois)
  expect_equal(eval_cdf(d1a, at = x), eval_cdf(d1b, at = x))
  expect_equal(
    eval_pmf(d1a, at = x, strict = FALSE),
    eval_pmf(d1b, at = x, strict = FALSE)
  )
  d2a <- maximise(d_norm, d_pois, draws = 1:2)
  d2b <- maximise(d_norm, d_pois, d_pois)
  d2c <- maximise(d_norm, d_pois, d_exp, draws = c(1, 2, 0))
  expect_equal(eval_cdf(d2a, at = x), eval_cdf(d2b, at = x))
  expect_equal(eval_cdf(d2a, at = x), eval_cdf(d2c, at = x))
  expect_equal(
    eval_pmf(d2a, at = x, strict = FALSE),
    eval_pmf(d2b, at = x, strict = FALSE)
  )
  expect_equal(
    eval_pmf(d2a, at = x, strict = FALSE),
    eval_pmf(d2c, at = x, strict = FALSE)
  )
  expect_null(expect_warning((maximise(draws = 4))))
  expect_error(maximise(d_norm, draws = 1:2))
  expect_error(maximise(d_norm, draws = numeric(0L)))
  d3a <- maximise(d_exp, d_norm, draws = 1:2)
  d3b <- maximise(d_exp, d_norm, d_norm)
  expect_equal(eval_cdf(d3a, at = x), eval_cdf(d3b, at = x))
  expect_equal(eval_density(d3a, at = x), eval_density(d3b, at = x))
})

test_that("pmf of max distribution lines up with cdf.", {
  d_norm <- dst_norm(0, 1)
  d_pois <- dst_pois(1)
  d_max <- maximise(d_pois, draws = 10)
  x <- 0:10
  expect_equal(
    eval_pmf(d_max, at = x),
    eval_cdf(d_max, at = x) - eval_cdf(d_max, at = x - 1)
  )
  d_max2 <- maximise(d_norm, d_pois)
  expect_equal(
    eval_pmf(d_max2, at = x, strict = FALSE),
    eval_cdf(d_max2, at = x) - eval_cdf(d_max2, at = x - 1e-6),
    tolerance = 1e-6
  )
})

test_that("density of max distribution makes sense.", {
  d_norm <- dst_norm(0, 1)
  d_exp <- dst_exp(1)
  d_max <- maximise(d_norm, d_exp)
  x <- 1:10 / 2
  eps <- 1e-6
  expect_equal(
    eval_density(d_max, at = x),
    (eval_cdf(d_max, at = x) - eval_cdf(d_max, at = x - eps)) / eps,
    tolerance = eps
  )
  expect_equal(eval_density(d_max, at = -(3:1)), rep(0, 3))
})

test_that("range of max works.", {
  d_exp <- dst_exp(1)
  d_norm <- dst_norm(0, 1)
  r1 <- range(maximise(d_norm, d_exp))
  expect_equal(r1, c(0, Inf))
  r2 <- range(maximise(d_norm, d_exp, -1 - d_exp))
  expect_equal(r2, c(0, Inf))
  r3 <- range(maximise(d_norm, d_norm))
  expect_equal(r3, c(-Inf, Inf))
})


test_that("simplification of max of finite distributions works.", {
  d1 <- dst_empirical(1:4)
  d2 <- dst_finite(c(3, 5, 6), probs = c(0.5, 0.2, 0.3))
  d_max1 <- maximise(d1, d2, draws = c(2, 1))
  d_max2 <- maximise(d1, d2, dst_unif(-2, -1), draws = c(2, 1, 3))
  x <- 1:13 / 2
  expect_equal(eval_cdf(d_max1, at = x), eval_cdf(d_max2, at = x))
  expect_equal(eval_pmf(d_max1, at = x), eval_pmf(d_max2, at = x))
})

test_that("Can maximise list of distributions.", {
  d1 <- dst_norm(0, 1)
  d2 <- dst_norm(0, 2)
  d3 <- dst_norm(0, 3)
  a1 <- list(d1, d2)
  a2 <- list(d1, list(d2))
  expect_equal(maximise(a1), maximise(d1, d2))
  expect_error(maximise(a2))
  expect_equal(maximise(list(d1), list(d2)), maximise(d1, d2))
  expect_equal(maximise(a1, d3), maximise(d1, d2, d3))
})


test_that("maximize handles edge cases", {
  # When no distributions are provided, should return NULL with a warning
  expect_warning(result <- maximize())
  expect_null(result)
  
  # When a single distribution is provided with draws=1, should return that
  # distribution
  norm_dist <- dst_norm(0, 1)
  expect_equal(maximize(norm_dist, draws = 1), norm_dist)
})

test_that("maximize and maximise are identical functions", {
  # Create test distributions
  norm_dist <- dst_norm(0, 1)
  exp_dist <- dst_exp(1)
  
  # Compare with no arguments
  expect_warning(max1 <- maximize())
  expect_warning(max2 <- maximise())
  expect_identical(max1, max2)
  
  # Compare with single distribution
  expect_identical(maximize(norm_dist), maximise(norm_dist))
  
  # Compare with multiple distributions
  expect_identical(
    maximize(norm_dist, exp_dist),
    maximise(norm_dist, exp_dist)
  )
  
  # Compare with draws parameter
  expect_identical(
    maximize(norm_dist, exp_dist, draws = 2),
    maximise(norm_dist, exp_dist, draws = 2)
  )
  
  # Compare with vector of draws
  expect_identical(
    maximize(norm_dist, exp_dist, draws = c(1, 2)),
    maximise(norm_dist, exp_dist, draws = c(1, 2))
  )
})