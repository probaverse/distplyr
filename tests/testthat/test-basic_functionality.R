library(distionary)

verbs <- list(
  exp = list(
    list(dst_exp(3)),
    list(dst_exp(3) + 1),
    list(dst_exp(3) - 1),
    list(dst_unif(-5, -2)),
    list(dst_t(2)),
    list(dst_gamma(2, 1))
  ),
  log = list(
    list(dst_weibull(1, 1)),
    list(dst_gpd(1, 1)),
    list(dst_unif(0, 2)),
    list(dst_exp(1)),
    list(dst_gamma(2, 1))
  ),
  flip = list(
    list(dst_norm(2, 1)),
    list(dst_unif(-5, -2)),
    list(dst_unif(-5, 2)),
    list(dst_unif(2, 5)),
    list(dst_exp(1)),
    list(dst_gamma(2, 1)),
    list(dst_gamma(2, 1) - 1),
    list(dst_gamma(2, 1) + 1)
  ),
  graft_right = list(
    list(
      dst_norm(0.8, 0.2),
      dst_gpd(1, 1) + 0.8,
      breakpoint = 0.8
    ),
    list(dst_norm(0, 1), dst_exp(1), breakpoint = 0),
    list(
      dst_gamma(3, 1),
      dst_weibull(3, 5),
      breakpoint = 2,
      include = TRUE
    ),
    list(
      dst_gev(0, 1, 1),
      dst_exp(1) + 1,
      breakpoint = 1,
      include = FALSE
    ),
    list(
      dst_gamma(3, 2),
      dst_gpd(1, 1),
      breakpoint = 5,
      include = TRUE
    ),
    list(
      dst_t(5),
      dst_gpd(1, 1),
      breakpoint = 2,
      include = FALSE
    )
  ),
  graft_left = list(
    list(
      dst_norm(0.8, 0.2),
      -dst_gpd(1, 1) - 0.8,
      breakpoint = -0.8
    ),
    list(dst_norm(0, 1), -dst_exp(1), breakpoint = 0),
    list(
      dst_gamma(3, 1),
      dst_weibull(3, 5),
      breakpoint = 2,
      include = TRUE
    ),
    list(
      -dst_gev(0, 1, 1),
      -dst_exp(1) - 1,
      breakpoint = -1,
      include = FALSE
    ),
    list(
      dst_gamma(3, 2),
      dst_gpd(1, 1),
      breakpoint = 5,
      include = TRUE
    ),
    list(
      dst_t(5),
      dst_gpd(1, 1),
      breakpoint = 1,
      include = FALSE
    )
  ),
  invert = list(
    list(dst_gpd(1, -2)),
    list(-dst_gpd(1, -2)),
    list(dst_unif(-5, -2)),
    list(dst_unif(2, 5)),
    list(dst_unif(-2, 2)),
    list(dst_exp(1)),
    list(dst_gamma(2, 1))
  ),
  maximize = list(
    list(dst_norm(0, 2)),
    list(dst_norm(0, 2), draws = 2.5),
    list(dst_exp(3), dst_gamma(2, 1)),
    list(
      dst_exp(3),
      dst_norm(3, 1),
      dst_gamma(2, 1)
    ),
    list(
      dst_exp(3),
      dst_norm(3, 1),
      draws = c(4, 3, 2),
      dst_gamma(2, 1)
    )
  ),
  minimize = list(
    list(dst_norm(0, 2)),
    list(dst_norm(0, 2), draws = 2.5),
    list(dst_exp(3), dst_gamma(2, 1)),
    list(
      dst_exp(3),
      dst_norm(3, 1),
      dst_gamma(2, 1)
    ),
    list(
      dst_exp(3),
      dst_norm(3, 1),
      draws = c(4, 3, 2),
      dst_gamma(2, 1)
    )
  ),
  mix = list(
    list(dst_t(5)),
    list(
      dst_norm(3, 1.3),
      dst_norm(0.8, 0.2),
      weights = 1:2
    ),
    list(dst_gamma(3, 2), dst_norm(3, 1)),
    list(
      dst_exp(3),
      dst_norm(3, 1),
      dst_gamma(2, 1)
    ),
    list(
      dst_exp(3),
      dst_norm(3, 1),
      weights = 1:3,
      dst_gamma(2, 1)
    )
  ),
  multiply = list(
    list(dst_gev(0, 1, 1), constant = 2),
    list(dst_gev(0, 1, 1), constant = 0.5),
    list(dst_unif(-5, -2), constant = 2),
    list(dst_unif(2, 5), constant = 2),
    list(dst_unif(-2, 5), constant = 2),
    list(dst_exp(1), constant = 2),
    list(dst_exp(1) + 1, constant = 2),
    list(dst_gamma(2, 1) - 1, constant = 1)
  ),
  shift = list(
    list(dst_gev(0, 1, 1), constant = 2),
    list(dst_gev(0, 1, 1), constant = -0.5),
    list(dst_gev(0, 1, 1), constant = 0),
    list(dst_unif(-5, -2), constant = 3),
    list(dst_exp(1), constant = -2),
    list(dst_gamma(2, 1), constant = 1)
  ),
  slice_left = list(
    list(dst_gev(0, 1, 1), breakpoint = 0),
    list(dst_norm(3, 3), breakpoint = 2, include = TRUE),
    list(dst_weibull(3, 3), breakpoint = 2, include = FALSE),
    list(dst_gamma(2, 2), breakpoint = -4),
    list(dst_gpd(1, -2), breakpoint = 0.2, include = FALSE),
    list(dst_unif(-5, 2), breakpoint = 0, include = TRUE),
    list(dst_unif(-5, 2), breakpoint = 0, include = FALSE),
    list(dst_exp(1), breakpoint = -2),
    list(dst_gamma(2, 1), breakpoint = 1)
  ),
  slice_right = list(
    list(-dst_gev(0, 1, 1), breakpoint = 0),
    list(dst_norm(3, 3), breakpoint = 2, include = TRUE),
    list(dst_weibull(3, 3), breakpoint = 2, include = FALSE),
    list(-dst_gamma(2, 2), breakpoint = 4),
    list(dst_gpd(1, -2), breakpoint = 0.2, include = FALSE),
    list(dst_unif(-5, 2), breakpoint = 0, include = TRUE),
    list(dst_unif(-5, 2), breakpoint = 0, include = FALSE),
    list(-dst_exp(1), breakpoint = 2),
    list(dst_gamma(2, 1), breakpoint = 1)
  )
)

test_that("exp_distribution works with various distributions", {
  p <- 1:9 / 10
  for (i in seq_along(verbs)) {
    param_list <- verbs[[i]]
    verb <- names(verbs)[i]
    cat(verb, "-----\n")
    i <- 0
    for (params in param_list) {
      i <- i + 1
      print(i)
      d <- rlang::exec(verb, !!!params)
      expect_s3_class(d, "dst")
      suppressWarnings(
        x <- eval_quantile(d, at = p)
      )
      ## Quantile
      if (distionary:::is_intrinsic(d, "quantile") && vtype(d) == "continuous") {
        x2 <- distionary:::eval_quantile_from_network(d, at = p)
        expect_equal(x, x2)
      }
      ## Density
      if (distionary:::is_intrinsic(d, "density") && vtype(d) == "continuous") {
        a <- eval_quantile(d, at = c(0.01, 0.99))
        dens_vals <- eval_density(d, at = x)
        expect_true(all(dens_vals >= 0))
        dens_fun <- distionary:::representation_as_function(d, "density")
        if (inherits(d, "inverse") && a[1] < 0 && a[2] > 0) {
          # Issues with flat cdf around 0 when the distribution contains
          # positive and negative values.
          original_dst <- params[[1]]
          r <- range(original_dst)
          flat <- 1 / r
          int1 <- stats::integrate(dens_fun, lower = a[1], upper = flat[1])
          int2 <- stats::integrate(dens_fun, lower = flat[2], upper = a[2])
          int <- int1$value + int2$value
        } else if (inherits(d, "graft")) {
          brk <- params$breakpoint
          int1 <- stats::integrate(dens_fun, lower = a[1], upper = brk)
          int2 <- stats::integrate(dens_fun, lower = brk, upper = a[2])
          int <- int1$value + int2$value
        } else {
          int <- stats::integrate(dens_fun, lower = a[1], upper = a[2])$value
        }
        expect_equal(int, 0.98, tolerance = 1e-5)

        # Calculate CDF differences by integrating density
        cdf_increments <- numeric(length(x) - 1)
        for (j in seq_along(x)[-1]) {
          # Integrate density between consecutive x points
          integral_result <- stats::integrate(
            dens_fun,
            lower = x[j - 1],
            upper = x[j],
            stop.on.error = FALSE
          )
          cdf_increments[j - 1] <- integral_result$value
        }

        # Calculate derived CDF values
        cdf_derived <- p[1] + cumsum(cdf_increments)

        # Compare with the stored CDF at the same points (excluding first point)
        cdf_stored <- eval_cdf(d, at = x[-1])

        # Allow some tolerance due to numerical integration
        expect_equal(cdf_derived, cdf_stored, tolerance = 1e-4)
      }
      ## Survival
      if (distionary:::is_intrinsic(d, "survival")) {
        survival_x <- eval_survival(d, at = x)
        survival_x2 <- distionary:::eval_survival_from_network(d, at = x)
        expect_equal(survival_x, survival_x2)
      }
      ## Range
      if (distionary:::is_intrinsic(d, "range") && distionary:::is_intrinsic(d, "quantile")) {
        r <- range(d)
        r2 <- distionary:::eval_range_from_network(d)
        expect_equal(r, r2)
      }
      ## Mean
      if (distionary:::is_intrinsic(d, "mean")) {
        mean_x <- mean(d)
        mean_x2 <- distionary:::eval_mean_from_network(d)
        if (is.infinite(mean_x) || is.na(mean_x)) {
          expect_true(is.infinite(mean_x2) || is.na(mean_x2))
        } else {
          expect_equal(mean_x, mean_x2)
        }
      }
      ## Variance
      if (distionary:::is_intrinsic(d, "variance")) {
        variance_x <- variance(d)
        variance_x2 <- distionary:::eval_variance_from_network(d)
        if (is.infinite(variance_x) || is.na(variance_x)) {
          expect_true(is.infinite(variance_x2) || is.na(variance_x2))
        } else {
          expect_equal(variance_x, variance_x2)
        }
      }
      ## Skewness
      if (distionary:::is_intrinsic(d, "skewness")) {
        skewness_x <- skewness(d)
        skewness_x2 <- distionary:::eval_skewness_from_network(d)
        if (is.infinite(skewness_x) || is.na(skewness_x)) {
          expect_true(is.infinite(skewness_x2) || is.na(skewness_x2))
        } else {
          expect_equal(skewness_x, skewness_x2)
        }
      }
      ## Kurtosis
      if (distionary:::is_intrinsic(d, "kurtosis")) {
        kurtosis_x <- kurtosis(d)
        kurtosis_x2 <- distionary:::eval_kurtosis_from_network(d)
        if (is.infinite(kurtosis_x) || is.na(kurtosis_x)) {
          expect_true(is.infinite(kurtosis_x2) || is.na(kurtosis_x2))
        } else {
          expect_equal(kurtosis_x, kurtosis_x2)
        }
      }
      ## Kurtosis Excess
      if (distionary:::is_intrinsic(d, "kurtosis_exc")) {
        kurtosis_exc_x <- kurtosis_exc(d)
        kurtosis_exc_x2 <- distionary:::eval_kurtosis_exc_from_network(d)
        if (is.infinite(kurtosis_exc_x) || is.na(kurtosis_exc_x)) {
          expect_true(is.infinite(kurtosis_exc_x2) || is.na(kurtosis_exc_x2))
        } else {
          expect_equal(kurtosis_exc_x, kurtosis_exc_x2)
        }
      }
      ## Stdev
      if (distionary:::is_intrinsic(d, "stdev")) {
        stdev_x <- stdev(d)
        stdev_x2 <- distionary:::eval_stdev_from_network(d)
        if (is.infinite(stdev_x) || is.na(stdev_x)) {
          expect_true(is.infinite(stdev_x2) || is.na(stdev_x2))
        } else {
          expect_equal(stdev_x, stdev_x2)
        }
      }
    }
  }
})
