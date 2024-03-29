test_that("slicing simplifies with finite distributions", {
  d <- dst_empirical(1:5)
  expect_error(slice_left(d, 5))
  expect_error(slice_right(d, 1))
  expect_true(is_finite_dst(slice_left(d, 2.5)))
  expect_true(is_finite_dst(slice_right(d, 2.5)))
  expect_true(is_finite_dst(slice_left(slice_right(d, 4), 2)))
  expect_equal(eval_pmf(slice_left(d, 1), at = 1:5), c(0, rep(0.25, 4)))
  expect_equal(
  	eval_pmf(slice_left(d, 2, include = FALSE), at = 1:5),
  	c(0, rep(0.25, 4))
  )
  expect_equal(eval_pmf(slice_right(d, 5), at = 1:5), c(rep(0.25, 4), 0))
  expect_equal(
  	eval_pmf(slice_right(d, 4, include = FALSE), at = 1:5),
  	c(rep(0.25, 4), 0)
  )
})

test_that("Range also gets sliced", {
	d1 <- dst_norm(0, 1)
	d2 <- dst_unif(0, 1)
	d3 <- dst_empirical(1:5)
	d4 <- dst_unif(3, 10)
	expect_equal(range(slice_left(d1, -2)), c(-2, Inf))
	expect_equal(range(slice_right(d2, 0.6, include = TRUE)), c(0, 0.6))
	expect_equal(range(slice_right(d3, 4.5)), c(1, 4))
	m <- mix(d2, d3, d4)
	expect_equal(range(slice_left(m, 1.5)), c(2, 10)) # Another ex. where splitting disc & cont is useful.
})

test_that("Slicing left on a flat part of the cdf works", {
  d <- mix(dst_unif(0, 0.5), dst_unif(1, 1.5))
  d1l <- slice_left(d, breakpoint = 0.5)
  d2l <- slice_left(d, breakpoint = 0.75)
  d3l <- slice_left(d, breakpoint = 1)
  d1r <- slice_right(d, breakpoint = 0.5)
  d2r <- slice_right(d, breakpoint = 0.75)
  d3r <- slice_right(d, breakpoint = 1)
  expect_equal(range(d1l), c(1.0, 1.5))
  expect_equal(range(d2l), c(1.0, 1.5))
  expect_equal(range(d3l), c(1.0, 1.5))
  expect_equal(range(d1r), c(0, 0.5))
  expect_equal(range(d2r), c(0, 0.5))
  expect_equal(range(d3r), c(0, 0.5))
})

test_that("Variable assignment works with slicing", {
	d_disc <- dst_pois(1)
	d_cont <- dst_unif(-1, 1)
	d_mix <- mix(d_disc, d_cont)
	expect_equal(variable(slice_right(d_mix, -0.5)), "continuous")
	expect_equal(variable(slice_left(d_mix, 1.5)), "discrete")
	expect_equal(variable(slice_left(d_mix, 0.5)), "mixed")
	expect_equal(variable(slice_right(d_mix, 0.5)), "mixed")
	expect_equal(variable(slice_left(d_disc, 0.5)), "discrete")
	expect_equal(variable(slice_right(d_disc, 5)), "discrete")
	expect_equal(variable(slice_right(d_cont, 0.5)), "continuous")
	expect_equal(variable(slice_left(d_cont, 0.5)), "continuous")
})

test_that("breakpoint inclusion works", {
	d1 <- dst_norm(0, 1)
	d2 <- dst_empirical(0:4)
	d <- mix(d1, d2)
	dl_inc <- slice_left(d, 0, include = TRUE)
	dl_not <- slice_left(d, 0, include = FALSE)
	dr_inc <- slice_right(d, 0, include = TRUE)
	dr_not <- slice_right(d, 0, include = FALSE)
	expect_equal(eval_pmf(dl_inc, at = 0, strict = FALSE), 0)
	expect_equal(eval_pmf(dl_not, at = 0, strict = FALSE), 0.2)
	expect_equal(eval_pmf(dr_inc, at = 0, strict = FALSE), 0)
	expect_equal(eval_pmf(dr_not, at = 0, strict = FALSE), 1 / 6)
	expect_equal(slice_left(d2, 4, include = FALSE), dst_degenerate(4))
	expect_equal(slice_right(d2, 0, include = FALSE), dst_degenerate(0))
	expect_equal(
		slice_right(dst_pois(1), 0, include = FALSE),
		dst_degenerate(0)
	)
})

test_that("slicing with infinity works as expected", {
	d <- dst_norm(0, 1)
	e <- dst_empirical(1:4)
	expect_error(slice_left(d, Inf))
	expect_error(slice_left(d, Inf, include = FALSE))
	expect_error(slice_right(d, -Inf))
	expect_error(slice_right(d, -Inf, include = FALSE))
	expect_error(slice_left(e, Inf))
	expect_error(slice_left(e, Inf, include = FALSE))
	expect_error(slice_right(e, -Inf))
	expect_error(slice_right(e, -Inf, include = FALSE))
})

test_that("slicing off nothing gives us the same distribution", {
	d1 <- dst_unif(0, 1)
	r1 <- range(d1)
	d2 <- dst_empirical(1:5)
	r2 <- range(d2)
	d3 <- mix(d1, d2)
	r3 <- range(d3)
	expect_equal(slice_left(d1, r1[1L] - 1), d1)
	expect_equal(slice_right(d1, r1[2L] + 1), d1)
	expect_equal(slice_left(d2, r2[1L] - 1), d2)
	expect_equal(slice_right(d2, r2[2L] + 1), d2)
	expect_equal(slice_left(d3, r3[1L] - 1), d3)
	expect_equal(slice_right(d3, r3[2L] + 1), d3)
})
