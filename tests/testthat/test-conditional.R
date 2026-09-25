rivers <- function() {
  a <- rbind(r1 = c(1, 0), r2 = c(0, 1), s = c(1, 1))
  distionary::dst_mv_norm(
    mean = stats::setNames(as.numeric(a %*% c(50, 80)), rownames(a)),
    cov = a %*% matrix(c(100, 60, 60, 225), 2) %*% t(a)
  )
}

test_that("conditioning on one variable gives the textbook Normal", {
  d <- distionary::dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)
  y <- conditional(d, given = c(x = 1))
  # Y | X = 1 is Normal with mean 1 + 0.6 * 2, sd 2 * sqrt(1 - 0.36).
  expect_equal(mean(y), 2.2)
  expect_equal(distionary::stdev(y), 1.6)
  expect_equal(conditional(d, given = list(x = 1)), y)
})

test_that("conditioning on a total slices the distribution", {
  sl <- conditional(rivers(), given = c(s = 200))
  expect_identical(distionary::variables(sl), c("r1", "r2"))
  expect_identical(distionary::vtype(sl), "singular")
  r1 <- distionary::marginal(sl, "r1")
  expect_equal(mean(r1), 50 + 160 / 445 * 70)
  expect_equal(distionary::stdev(r1), sqrt(100 - 160^2 / 445))
})

test_that("finite distributions keep the matching points", {
  e <- distionary::dst_mv_empirical(
    list(a = c(1, 2, 2, 3), b = c(1, 1, 2, 2))
  )
  b <- conditional(e, list(a = 2))
  expect_equal(distionary::eval_pmf(b, 1:2), c(0.5, 0.5))
  expect_identical(distionary::variables(b), "b")
  expect_true(is.na(conditional(e, c(a = 9))))
})

test_that("`given` is checked", {
  trio <- rivers()
  expect_error(conditional(trio, c(40, 10)), "Name each value")
  expect_error(conditional(trio, c(z = 1)), "does not have")
  expect_error(conditional(trio, c(s = 1, s = 2)), "twice")
  expect_error(
    conditional(trio, c(r1 = 1, r2 = 1, s = 2)),
    "nothing to"
  )
  expect_error(conditional(trio, "s"), "named vector")
  expect_error(conditional(trio, list(s = 1:2)), "single number")
  expect_error(
    conditional(distionary::dst_norm(0, 1), c(x = 1)),
    "one variable"
  )
  expect_true(is.na(conditional(trio, c(s = NA))))
})
