dat <- local({
	base <- distionary::dst_empirical(-2:2)
	norm <- distionary::dst_norm(0, 1)
	list(
		base = base,
		dri = graft_right(base, norm, breakpoint = 0, include = TRUE),
		dre = graft_right(base, norm, breakpoint = 0, include = FALSE),
		dli = graft_left(base, norm, breakpoint = 0, include = TRUE),
		dle = graft_left(base, norm, breakpoint = 0, include = FALSE)
	)
})

test_that("check cdf at breakpoint", {
	expect_equal(distionary::prob_left(dat$dle, of = 0, inclusive = TRUE), 0.6)
	expect_equal(distionary::prob_left(dat$dle, of = 0, inclusive = FALSE), 0.6)
	expect_equal(distionary::prob_left(dat$dli, of = 0, inclusive = TRUE), 0.6)
	expect_equal(distionary::prob_left(dat$dli, of = 0, inclusive = FALSE), 0.4)
	expect_equal(distionary::prob_left(dat$dre, of = 0, inclusive = TRUE), 0.4)
	expect_equal(distionary::prob_left(dat$dre, of = 0, inclusive = FALSE), 0.4)
	expect_equal(distionary::prob_left(dat$dri, of = 0, inclusive = TRUE), 0.6)
	expect_equal(distionary::prob_left(dat$dri, of = 0, inclusive = FALSE), 0.4)
})

test_that("check cdf in base", {
	x <- c(-2.5, -1.5, -0.5)
	expect_equal(
		distionary::eval_cdf(dat$dre, at = x),
		distionary::eval_cdf(dat$base, at = x)
	)
	expect_equal(
		distionary::eval_cdf(dat$dri, at = x),
		distionary::eval_cdf(dat$base, at = x)
	)
	expect_equal(
		distionary::eval_cdf(dat$dle, at = -x),
		distionary::eval_cdf(dat$base, at = -x)
	)
	expect_equal(
		distionary::eval_cdf(dat$dli, at = -x),
		distionary::eval_cdf(dat$base, at = -x)
	)
})

test_that("quantiles at breakpoint", {
	expect_equal(distionary::eval_quantile(dat$dri, at = 0.5), 0)
	expect_gt(distionary::eval_quantile(dat$dre, at = 0.5), 0)
	expect_equal(distionary::eval_quantile(dat$dli, at = 0.5), 0)
	expect_lt(distionary::eval_quantile(dat$dle, at = 0.5), 0)
})

test_that("a graft distribution is only ever a mixture of two distributions.", {
	d <- dst_norm(0, 1)
	g <- d |>
		graft_right(d, breakpoint = 2) |>
		graft_left(d, breakpoint = -2)
	expect_length(g$components$distributions, 2L)
})

rm('dat')
