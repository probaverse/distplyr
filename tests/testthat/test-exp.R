library(testthat)
library(distionary)
library(distplyr)

distributions <- list(
  distionary::dst_norm(0, 1),
  distionary::dst_unif(0.1, 2),
  distionary::dst_exp(1),
  distionary::dst_pois(3),
  distionary::dst_degenerate(0),
  distionary::dst_degenerate(1),
  distionary::dst_degenerate(-1)
)

ppties <- list(
  "cdf",
  "density"
)

test_that("exp_distribution works with various distributions", {
  for (d in distributions) {
    exp_d <- exp(d)
    expect_s3_class(exp_d, "exponential")
    p <- 1:9 / 10
    x <- distionary::eval_quantile(exp_d, at = p)

    for (ppt in ppties) {
      if (distionary:::is_intrinsic(d, ppt)) {
        network_eval_fn <- paste0("distionary:::eval_", ppt, "_from_network")
        expect_equal(
          rlang::exec(network_eval_fn, exp_d),
          distionary::eval_property(exp_d, entry = ppt, at = x)
        )
      }
    }
  }
})

test_that("exp_distribution handles edge cases", {
  # Negative values in the range
  d <- distionary::dst_unif(-1, 1)
  expect_error(exp_distribution(d), "Cannot apply logarithm to a distribution with non-positive values.")

  # Degenerate distribution at zero
  d <- distionary::dst_degenerate(0)
  expect_error(exp_distribution(d), "Cannot apply logarithm to a distribution with non-positive values.")
})
