library(testthat)
library(distionary)
library(distplyr)

test_that("flip works with various distributions", {
  distributions <- list(
    distionary::dst_norm(0, 1),
    distionary::dst_unif(-2, 2),
    distionary::dst_pois(3),
    distionary::dst_exp(1),
    distionary::dst_empirical(c(-1, 0, 1))
  )
  
  for (d in distributions) {
    flipped <- flip(d)
    expect_s3_class(flipped, "negated")
    
    # Check CDF
    x_vals <- c(-2, 0, 2)
    cdf_vals <- eval_cdf(flipped, at = x_vals)
    expect_true(all(cdf_vals >= 0 & cdf_vals <= 1))
    
    # Check density
    density_vals <- eval_density(flipped, at = x_vals)
    expect_true(all(density_vals >= 0))
    
    # Check PMF (if applicable)
    if (distionary::vtype(d) == "discrete") {
      pmf_vals <- eval_pmf(flipped, at = x_vals)
      expect_true(all(pmf_vals >= 0))
    }
    
    # Check quantile
    p_vals <- c(0.1, 0.5, 0.9)
    quantiles <- eval_quantile(flipped, at = p_vals)
    expect_true(all(quantiles <= 0))
    
    # Check realization
    samples <- realize(flipped, n = 10)
    expect_true(all(samples <= 0))
  }
})

test_that("flip handles edge cases", {
  # Degenerate distribution
  d <- distionary::dst_degenerate(0)
  flipped <- flip(d)
  expect_s3_class(flipped, "negated")
  expect_equal(eval_cdf(flipped, at = 0), 1)
  expect_equal(eval_density(flipped, at = 0), Inf)
  
  # Empirical distribution with all positive values
  d <- distionary::dst_empirical(c(1, 2, 3))
  flipped <- flip(d)
  expect_s3_class(flipped, "negated")
  expect_equal(eval_pmf(flipped, at = c(-1, -2, -3)), eval_pmf(d, at = c(1, 2, 3)))
})
