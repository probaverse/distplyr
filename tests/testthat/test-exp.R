library(testthat)
library(distionary)
library(distplyr)

test_that("exp_distribution works with various distributions", {
  distributions <- list(
    distionary::dst_norm(0, 1),
    distionary::dst_unif(0.1, 2),
    distionary::dst_exp(1),
    distionary::dst_pois(3),
    distionary::dst_empirical(c(0.5, 1, 1.5))
  )
  
  for (d in distributions) {
    exp_d <- exp_distribution(d)
    expect_s3_class(exp_d, "exponential")
    
    # Check CDF
    x_vals <- c(0.5, 1, 2)
    cdf_vals <- eval_cdf(exp_d, at = x_vals)
    expect_true(all(cdf_vals >= 0 & cdf_vals <= 1))
    
    # Check density
    density_vals <- eval_density(exp_d, at = x_vals)
    expect_true(all(density_vals >= 0))
    
    # Check PMF (if applicable)
    if (distionary::vtype(d) == "discrete") {
      pmf_vals <- eval_pmf(exp_d, at = x_vals)
      expect_true(all(pmf_vals >= 0))
    }
    
    # Check quantile
    p_vals <- c(0.1, 0.5, 0.9)
    quantiles <- eval_quantile(exp_d, at = p_vals)
    expect_true(all(quantiles > 0))
    
    # Check realization
    samples <- realize(exp_d, n = 10)
    expect_true(all(samples > 0))
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
