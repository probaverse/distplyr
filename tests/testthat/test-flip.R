library(distionary)

test_that("flip handles edge cases", {
  ## Normal
  expect_equal(flip(dst_norm(5, 2)), dst_norm(-5, 2))
  expect_equal(flip(dst_norm(-5, 2)), dst_norm(5, 2))
  expect_equal(flip(dst_norm(0, 2)), dst_norm(0, 2))
  ## Uniform
  expect_equal(flip(dst_unif(2, 5)), dst_unif(-5, -2))
  expect_equal(flip(dst_unif(-2, 5)), dst_unif(-5, 2))
  ## Cauchy
  expect_equal(flip(dst_cauchy(5, 2)), dst_cauchy(-5, 2))
  expect_equal(flip(dst_cauchy(-5, 2)), dst_cauchy(5, 2))
  expect_equal(flip(dst_cauchy(0, 2)), dst_cauchy(0, 2))
  ## Student t
  expect_equal(flip(dst_t(5)), dst_t(5))
  expect_equal(flip(dst_t(2.2)), dst_t(2.2))
})
