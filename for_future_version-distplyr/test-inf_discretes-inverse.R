context("Test discretes in an inverse distribution with infinite discretes")

sink_0_right <- 1 / (1 + dst_pois(1))
sink_4_left <- sink_0_right / 12 + 0.25 # inverse has seq: 3, 3.4286, ... lim=4.
sink_m4_right <- -sink_4_left # inverse has sequence: -3, -3.4286, ... lim=-4.
fin <- dst_empirical(1 / c(-10:-1, 1:10)) # inverse has -10 to 10 without 0.
d <- 1 / mix(sink_4_left, sink_m4_right, fin)

test_that("Do not get caught in infinite sink.", {
	expect_equal(
		next_discrete(d, from = -2, n = 6, include_from = TRUE),
		c(-2, -1, 1, 2, 3, 24 / 7)
	)
	expect_equal(
		next_discrete(d, from = 1, n = 4, include_from = TRUE),
		c(1, 2, 3, 24 / 7)
	)
	expect_equal(next_discrete(d, from = -Inf, n = 7), -10:-4)
	expect_length(next_discrete(d, from = Inf, n = 7), 0L)
	expect_equal(
		prev_discrete(d, from = 2, n = 6, include_from = TRUE),
		c(2, 1, -1, -2, -3, -24 / 7)
	)
	expect_equal(
		prev_discrete(d, from = -1, n = 4, include_from = TRUE),
		c(-1, -2, -3, -24 / 7)
	)
	expect_equal(prev_discrete(d, from = Inf, n = 7), 10:4)
	expect_length(prev_discrete(d, from = -Inf, n = 7), 0L)
})

test_that("Infinite number of points in query.", {
	expect_error(next_discrete(d, from = 0, n = Inf))
	expect_error(prev_discrete(d, from = 0, n = Inf))
	expect_error(next_discrete(d, from = -4))
	expect_error(prev_discrete(d, from = 4))
	eps <- 1e-10
	expect_equal(num_discretes(d, from = -4, to = -4 + eps), Inf)
	expect_equal(num_discretes(d, from = 4, to = 4 - eps), Inf)
	expect_equal(num_discretes(d, from = -Inf, to = Inf), Inf)
})

rm('sink_0_right', 'sink_4_left', 'sink_m4_right', 'fin', 'd')
