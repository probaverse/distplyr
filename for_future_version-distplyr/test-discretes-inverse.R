context("Test discretes in an inverse distribution with finite discretes")

neg <- dst_empirical(-(1:2))
pos <- dst_empirical(1:2)
n <- dst_norm(0, 1)
d <- 1 / mix(neg, pos, n)

test_that("next_discrete works with finite number of points, n = Inf", {
	expect_equal(next_discrete(d, from = -0.5, n = Inf), c(0.5, 1))
	expect_equal(
		next_discrete(d, from = -0.5, n = Inf, include_from = TRUE),
		c(-0.5, 0.5, 1)
	)
	expect_equal(next_discrete(d, from = 0.5, n = Inf), 1)
	expect_equal(
		next_discrete(d, from = 0.5, n = Inf, include_from = TRUE),
		c(0.5, 1)
	)
	expect_equal(
		next_discrete(d, from = 0, n = Inf, include_from = TRUE),
		c(0.5, 1)
	)
	expect_length(next_discrete(d, from = 5, n = Inf, include_from = TRUE), 0L)
})


test_that("next_discrete works with finite number of points, n = 1", {
	expect_equal(next_discrete(d, from = -0.5), 0.5)
	expect_equal(next_discrete(d, from = -0.5, include_from = TRUE), -0.5)
	expect_equal(next_discrete(d, from = 0.5), 1)
	expect_equal(next_discrete(d, from = 0.5, include_from = TRUE), 0.5)
})

test_that("prev_discrete works with finite number of points, n = Inf", {
	expect_equal(prev_discrete(d, from = 0.5, n = Inf), c(-0.5, -1))
	expect_equal(
		prev_discrete(d, from = 0.5, n = Inf, include_from = TRUE),
		c(0.5, -0.5, -1)
	)
	expect_equal(prev_discrete(d, from = -0.5, n = Inf), -1)
	expect_equal(
		prev_discrete(d, from = -0.5, n = Inf, include_from = TRUE),
		c(-0.5, -1)
	)
	expect_equal(
		prev_discrete(d, from = 0, n = Inf, include_from = TRUE),
		c(-0.5, -1)
	)
	expect_length(prev_discrete(d, from = -5, n = Inf, include_from = TRUE), 0L)
})


test_that("prev_discrete works with finite number of points, n = 1", {
	expect_equal(prev_discrete(d, from = 0.5), -0.5)
	expect_equal(prev_discrete(d, from = 0.5, include_from = TRUE), 0.5)
	expect_equal(prev_discrete(d, from = -0.5), -1)
	expect_equal(prev_discrete(d, from = -0.5, include_from = TRUE), -0.5)
})

rm('neg', 'pos', 'n', 'd')
