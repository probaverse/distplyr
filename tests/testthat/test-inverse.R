test_that("Inverse quantile calculated correctly for -'ve and +'ve dist", {
  d <- 1 / distionary::dst_norm(0, 1)
  u <- 0:100 / 100
  u2 <- eval_cdf(d, at = eval_quantile(d, at = u))
  expect_equal(u, u2)
})

test_that("Plateaus in a cdf get recognized", {
  d <- mix(dst_unif(1, 2), dst_unif(3, 4))
  plot(1 / d, "cdf", from = 1 / 4, to = 1, n = 1000)
  eval_quantile(1 / d, at = 0.5)
})


