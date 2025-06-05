library(distionary)

dat <- local({
	base <- dst_empirical(-2:2)
	norm <- dst_norm(0, 1)
	list(
		base = base,
		dri = graft_right(base, norm, breakpoint = 0, include = TRUE),
		dre = graft_right(base, norm, breakpoint = 0, include = FALSE),
		dli = graft_left(base, norm, breakpoint = 0, include = TRUE),
		dle = graft_left(base, norm, breakpoint = 0, include = FALSE)
	)
})

test_that("check cdf at breakpoint", {
	expect_equal(prob_left(dat$dle, of = 0, inclusive = TRUE), 0.6)
	expect_equal(prob_left(dat$dle, of = 0, inclusive = FALSE), 0.6)
	expect_equal(prob_left(dat$dli, of = 0, inclusive = TRUE), 0.6)
	expect_equal(prob_left(dat$dli, of = 0, inclusive = FALSE), 0.4)
	expect_equal(prob_left(dat$dre, of = 0, inclusive = TRUE), 0.4)
	expect_equal(prob_left(dat$dre, of = 0, inclusive = FALSE), 0.4)
	expect_equal(prob_left(dat$dri, of = 0, inclusive = TRUE), 0.6)
	expect_equal(prob_left(dat$dri, of = 0, inclusive = FALSE), 0.4)
})

test_that("check cdf in base", {
	x <- c(-2.5, -1.5, -0.5)
	expect_equal(
		eval_cdf(dat$dre, at = x),
		eval_cdf(dat$base, at = x)
	)
	expect_equal(
		eval_cdf(dat$dri, at = x),
		eval_cdf(dat$base, at = x)
	)
	expect_equal(
		eval_cdf(dat$dle, at = -x),
		eval_cdf(dat$base, at = -x)
	)
	expect_equal(
		eval_cdf(dat$dli, at = -x),
		eval_cdf(dat$base, at = -x)
	)
})

test_that("quantiles at breakpoint", {
	expect_equal(eval_quantile(dat$dri, at = 0.5), 0)
	expect_gt(eval_quantile(dat$dre, at = 0.5), 0)
	expect_equal(eval_quantile(dat$dli, at = 0.5), 0)
	expect_lt(eval_quantile(dat$dle, at = 0.5), 0)
})

test_that("a graft distribution is only ever a mixture of two distributions.", {
	d <- dst_norm(0, 1)
	g <- d |>
		graft_right(d, breakpoint = 2) |>
		graft_left(d, breakpoint = -2)
	expect_length(g$components$distributions, 2L)
})

test_that("graft_right handles special cases", {
  # Create distributions for testing
  base <- dst_unif(0, 10)
  graft_dist <- dst_exp(1)
  
  # When p_left = 1 (all mass is in the left part)
  # Create a scenario where breakpoint is beyond the range
  result1 <- graft_right(base, graft_dist, breakpoint = 15)
  expect_equal(result1, base)
  
  # When p_left = 0 (no mass in the left part)
  # Create a scenario where breakpoint is below the range
  result2 <- graft_right(base, graft_dist, breakpoint = -5)
  expect_equal(result2, slice_left(graft_dist, breakpoint = -5, include = FALSE))
})

test_that("graft_left handles special cases", {
  # Create distributions for testing
  base <- dst_unif(0, 10)
  graft_dist <- dst_exp(1)
  
  # When p_right = 1 (all mass is in the right part)
  # Create a scenario where breakpoint is below the range
  result1 <- graft_left(base, graft_dist, breakpoint = -5)
  expect_equal(result1, base)
  
  # When p_right = 0 (no mass in the right part)
  # Create a scenario where breakpoint is beyond the range
  result2 <- graft_left(base, graft_dist, breakpoint = 15)
  expect_equal(result2, slice_right(graft_dist, breakpoint = 15, include = FALSE))
})

test_that("include parameter doesn't matter for continuous distributions in graft", {
  # For continuous distributions, include=TRUE or include=FALSE should give identical results
  base <- dst_unif(0, 10)
  graft_dist <- dst_norm(5, 2)
  
  # For graft_right with continuous distributions
  graft_right_true <- graft_right(base, graft_dist, breakpoint = 5, include = TRUE)
  graft_right_false <- graft_right(base, graft_dist, breakpoint = 5, include = FALSE)
  
  # For graft_left with continuous distributions
  graft_left_true <- graft_left(base, graft_dist, breakpoint = 5, include = TRUE)
  graft_left_false <- graft_left(base, graft_dist, breakpoint = 5, include = FALSE)
  
  # Check evaluations at various points
  test_points <- c(2, 5, 8)
  
  # CDF evaluations should be identical for continuous distributions
  expect_equal(
    eval_cdf(graft_right_true, at = test_points),
    eval_cdf(graft_right_false, at = test_points)
  )
  
  expect_equal(
    eval_cdf(graft_left_true, at = test_points),
    eval_cdf(graft_left_false, at = test_points)
  )
  
  # Quantile evaluations should be identical for continuous distributions
  test_probs <- c(0.1, 0.5, 0.9)
  
  expect_equal(
    eval_quantile(graft_right_true, at = test_probs),
    eval_quantile(graft_right_false, at = test_probs)
  )
  
  expect_equal(
    eval_quantile(graft_left_true, at = test_probs),
    eval_quantile(graft_left_false, at = test_probs)
  )
})

rm('dat')
