library(distionary)

d <- list(
  d1 = dst_norm(0, 1),
  d2 = dst_gp(scale = 1, shape = 1)
)
d$m0 <- mix(d$d1, d$d2, weights = c(1, 0))
d$m1 <- mix(d$d1, d$d2, weights = c(0.4, 0.6))

test_that("Distributions with 0 weights excluded in mixture.", {
  expect_identical(d$m0, d$d1)
})

test_that("Variable types are as expected.", {
  expect_identical(vtype(d$m1), "continuous")
})

test_that("Can mix list of distributions.", {
  a1 <- list(d$d1, d$d2)
  a2 <- list(d$d1, list(d$d2))
  expect_equal(mix(a1), mix(d$d1, d$d2))
  expect_error(mix(a2))
  expect_equal(mix(list(d$d1), list(d$d2)), mix(d$d1, d$d2))
})

test_that("computations are correct", {
  expect_identical(
    eval_density(d$m1, at = -1),
    dnorm(-1) * 0.4
  )
  expect_identical(
    eval_pmf(d$m1, at = c(-1, 1)),
    c(0, 0)
  )
  expect_identical(
    eval_cdf(d$m1, at = -1),
    pnorm(-1) * 0.4
  )
})

test_that("mix handles special cases properly", {
  # When no distributions are provided, it returns NULL with a warning
  expect_warning(result <- mix())
  expect_null(result)

  # When a single distribution is provided, it returns that distribution
  expect_identical(mix(d$d1), d$d1)

  # When there are negative weights, it stops with an error
  expect_error(mix(d$d1, d$d2, weights = c(-1, 1)))

  # When weights have NA values and na.rm = FALSE, it returns NA
  expect_identical(mix(d$d1, d$d2, weights = c(1, NA), na.rm = FALSE), NA)

  # When weights have NA values but na.rm = TRUE, it should work
  expect_s3_class(
    mix(d$d1, d$d2, weights = c(1, NA), na.rm = TRUE),
    "dst"
  )
})

rm("d")
