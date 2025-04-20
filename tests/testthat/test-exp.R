library(testthat)
library(distionary)
library(distplyr)


verbs <- list(
  exp = list(
    list(distionary::dst_bern(0.8)),
    list(distionary::dst_pois(3)),
    list(distionary::dst_degenerate(0)),
    list(distionary::dst_degenerate(1)),
    list(distionary::dst_degenerate(-1)),
    list(distionary::dst_unif(-5, -2)),
    list(distionary::dst_exp(1)),
    list(distionary::dst_gamma(2, 1))
  ),
  log = list(
    list(distionary::dst_degenerate(1)),
    list(distionary::dst_unif(0, 2)),
    list(distionary::dst_exp(1)),
    list(distionary::dst_gamma(2, 1))
  ),
  flip = list(
    list(distionary::dst_bern(0.8)),
    list(distionary::dst_pois(3)),
    list(distionary::dst_degenerate(0)),
    list(distionary::dst_degenerate(1)),
    list(distionary::dst_degenerate(-1)),
    list(distionary::dst_unif(-5, -2)),
    list(distionary::dst_exp(1)),
    list(distionary::dst_gamma(2, 1))
  ),
  graft_right = list(
    list(distionary::dst_bern(0.8), distionary::dst_exp(1), breakpoint = 0.5),
    list(distionary::dst_norm(0, 1), distionary::dst_exp(1), breakpoint = 0),
    list(
      distionary::dst_pois(3),
      distionary::dst_nbinom(5, 0.5),
      breakpoint = 2,
      include = TRUE
    ),
    list(
      distionary::dst_pois(3),
      distionary::dst_nbinom(5, 0.5),
      breakpoint = 2,
      include = FALSE
    ),
    list(
      distionary::dst_pois(3),
      distionary::dst_gpd(1, 1),
      breakpoint = 5,
      include = TRUE
    ),
    list(
      distionary::dst_pois(3),
      distionary::dst_gpd(1, 1),
      breakpoint = 5,
      include = FALSE
    )
  ),
  graft_left = list(
    list(
      distionary::dst_pois(5),
      distionary::dst_norm(5, 2.2),
      breakpoint = 2,
      include = TRUE
    ),
    list(
      distionary::dst_pois(5),
      distionary::dst_norm(5, 2.2),
      breakpoint = 2,
      include = FALSE
    ),
    list(
      distionary::dst_pois(3),
      distionary::dst_nbinom(5, 0.5),
      breakpoint = 3,
      include = TRUE
    ),
    list(
      distionary::dst_pois(3),
      distionary::dst_nbinom(5, 0.5),
      breakpoint = 3,
      include = FALSE
    ),
    list(distionary::dst_gamma(1, 2), distionary::dst_t(3), breakpoint = 2)
  ),
  invert = list(
    list(distionary::dst_degenerate(1)),
    list(distionary::dst_degenerate(-1)),
    list(distionary::dst_unif(-5, -2)),
    list(distionary::dst_unif(2, 5)),
    list(distionary::dst_unif(-2, 2)),
    list(distionary::dst_exp(1)),
    list(distionary::dst_gamma(2, 1))
  ),
  maximize = list(
    list(distionary::dst_norm(0, 2)),
    list(distionary::dst_norm(0, 2), draws = 2.5),
    list(distionary::dst_exp(3), distionary::dst_gamma(2, 1)),
    list(
      distionary::dst_exp(3),
      distionary::dst_norm(3, 1),
      distionary::dst_gamma(2, 1)
    ),
    list(
      distionary::dst_exp(3),
      distionary::dst_norm(3, 1),
      draws = c(4, 3, 2),
      distionary::dst_gamma(2, 1)
    )
  ),
  minimize = list(
    list(distionary::dst_norm(0, 2)),
    list(distionary::dst_norm(0, 2), draws = 2.5),
    list(distionary::dst_exp(3), distionary::dst_gamma(2, 1)),
    list(
      distionary::dst_exp(3),
      distionary::dst_norm(3, 1),
      distionary::dst_gamma(2, 1)
    ),
    list(
      distionary::dst_exp(3),
      distionary::dst_norm(3, 1),
      draws = c(4, 3, 2),
      distionary::dst_gamma(2, 1)
    )
  ),
  mix = list(
    list(distionary::dst_bern(0.8)),
    list(
      distionary::dst_norm(3, 1.3),
      distionary::dst_norm(0.8, 0.2),
      weights = 1:2
    ),
    list(distionary::dst_gamma(3, 2), distionary::dst_norm(3, 1)),
    list(
      distionary::dst_exp(3),
      distionary::dst_norm(3, 1),
      distionary::dst_gamma(2, 1)
    ),
    list(
      distionary::dst_exp(3),
      distionary::dst_norm(3, 1),
      weights = 1:3,
      distionary::dst_gamma(2, 1)
    )
  ),
  multiply = list(
    list(distionary::dst_bern(0.8), constant = 2),
    list(distionary::dst_pois(3), constant = 0.5),
    list(distionary::dst_degenerate(0), constant = 4),
    list(distionary::dst_degenerate(1), constant = 2),
    list(distionary::dst_degenerate(-1), constant = 2),
    list(distionary::dst_unif(-5, -2), constant = 0),
    list(distionary::dst_exp(1), constant = 2),
    list(distionary::dst_gamma(2, 1), constant = 1)
  ),
  shift = list(
    list(distionary::dst_bern(0.8), constant = 2),
    list(distionary::dst_pois(3), constant = -0.5),
    list(distionary::dst_degenerate(0), constant = 4),
    list(distionary::dst_degenerate(1), constant = -2),
    list(distionary::dst_degenerate(-1), constant = 2),
    list(distionary::dst_unif(-5, -2), constant = 0),
    list(distionary::dst_exp(1), constant = -2),
    list(distionary::dst_gamma(2, 1), constant = 1)
  ),
  slice_left = list(
    list(distionary::dst_bern(0.8), breakpoint = 0.5),
    list(distionary::dst_pois(3), breakpoint = 2, include = TRUE),
    list(distionary::dst_pois(3), breakpoint = 2, include = FALSE),
    list(distionary::dst_degenerate(0), breakpoint = -4),
    list(distionary::dst_degenerate(1), breakpoint = 1, include = FALSE),
    list(distionary::dst_unif(-5, 2), breakpoint = 0, include = TRUE),
    list(distionary::dst_unif(-5, 2), breakpoint = 0, include = FALSE),
    list(distionary::dst_exp(1), breakpoint = -2),
    list(distionary::dst_gamma(2, 1), breakpoint = 1)
  ),
  slice_right = list(
    list(distionary::dst_bern(0.8), breakpoint = 0.5),
    list(distionary::dst_pois(3), breakpoint = 2, include = TRUE),
    list(distionary::dst_pois(3), breakpoint = 2, include = FALSE),
    list(distionary::dst_degenerate(0), breakpoint = 4),
    list(distionary::dst_degenerate(1), breakpoint = 1, include = FALSE),
    list(distionary::dst_unif(-5, 2), breakpoint = 0, include = TRUE),
    list(distionary::dst_unif(-5, 2), breakpoint = 0, include = FALSE),
    list(distionary::dst_unif(-5, 2), breakpoint = 3),
    list(distionary::dst_exp(1), breakpoint = 2),
    list(distionary::dst_gamma(2, 1), breakpoint = 1)
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
        x <- distionary::eval_quantile(d, at = p)
      )
      ## Quantile
      if (distionary:::is_intrinsic(d, "quantile") && distionary::vtype(d) == "continuous") {
        x2 <- distionary:::eval_quantile_from_network(d, at = p)
        expect_equal(x, x2)
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
