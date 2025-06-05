library(distionary)

test_that("slice_right works with continuous distributions", {
  d <- dst_unif(0, 10)
  sliced <- slice_right(d, 5)
  expect_s3_class(sliced, "slice_right")
  expect_equal(range(sliced)[2], 5)
  expect_equal(eval_cdf(sliced, at = 5), 1)
})

test_that("slice_right works with discrete distributions", {
  d <- dst_pois(3)
  sliced <- slice_right(d, 2)
  expect_s3_class(sliced, "slice_right")
  expect_equal(range(sliced)[2], 2)
  expect_equal(
    eval_pmf(sliced, at = 2), 
    eval_pmf(d, at = 2) / prob_left(d, of = 2)
  )
})

test_that("slice_right handles edge cases", {
  d <- dst_unif(0, 10)
  # Breakpoint beyond range - returns original distribution
  expect_equal(slice_right(d, 15), d)
  
  # Breakpoint below range - returns null distribution with warning
  expect_warning(result <- slice_right(d, -5))
  expect_s3_class(result, "dst_null")
  
  # Test degenerate case when breakpoint = min and include = FALSE
  # Create a distribution with mass at a specific point
  p <- dst_binom(size = 10, prob = 0.5)
  # When include=FALSE and breakpoint has mass, return degenerate distribution
  expect_warning(degenerate_result <- slice_right(p, 0, include = FALSE))
  expect_equal(degenerate_result, dst_degenerate(0))
})

test_that("include parameter doesn't matter for continuous distributions", {
  # For continuous distributions, include=TRUE or include=FALSE 
  # should give identical results
  d <- dst_unif(0, 10)
  
  # For slice_right
  sliced_include_true <- slice_right(d, 5, include = TRUE)
  sliced_include_false <- slice_right(d, 5, include = FALSE)
  
  # Check probability at point
  expect_equal(
    eval_cdf(sliced_include_true, at = 5),
    eval_cdf(sliced_include_false, at = 5)
  )
  
  # Check quantiles
  test_probs <- c(0.1, 0.5, 0.9)
  expect_equal(
    eval_quantile(sliced_include_true, at = test_probs),
    eval_quantile(sliced_include_false, at = test_probs)
  )
})
