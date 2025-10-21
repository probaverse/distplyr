library(distionary)

test_that("minimize handles edge cases", {
  # When no distributions are provided, should return NULL with a warning
  expect_warning(result <- minimize())
  expect_null(result)
  
  # When a single distribution is provided with draws=1, should return that
  # distribution
  norm_dist <- dst_norm(0, 1)
  expect_equal(minimize(norm_dist, draws = 1), norm_dist)
})

test_that("minimize and minimize are identical functions", {
  # Create test distributions
  norm_dist <- dst_norm(0, 1)
  exp_dist <- dst_exp(1)
  
  # Compare with no arguments
  expect_warning(min1 <- minimize())
  expect_warning(min2 <- minimize())
  expect_identical(min1, min2)
  
  # Compare with single distribution
  expect_identical(minimize(norm_dist), minimize(norm_dist))
  
  # Compare with multiple distributions
  expect_identical(
    minimize(norm_dist, exp_dist),
    minimize(norm_dist, exp_dist)
  )
  
  # Compare with draws parameter
  expect_identical(
    minimize(norm_dist, exp_dist, draws = 2),
    minimize(norm_dist, exp_dist, draws = 2)
  )
  
  # Compare with vector of draws
  expect_identical(
    minimize(norm_dist, exp_dist, draws = c(1, 2)),
    minimize(norm_dist, exp_dist, draws = c(1, 2))
  )
})