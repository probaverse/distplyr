#' @rdname linear_transform
#' @export
invert <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  nm <- distionary::pretty_name(distribution)
  if (nm == "Null") {
    return(distribution)
  }
  p_zero <- distionary::eval_pmf(distribution, at = 0)
  if (p_zero > 0) {
    stop("Cannot invert a distribution for which 0 is a possible outcome.")
  }
  ## BEGIN special simplifications ---------------------------------------------
  if (nm == "Finite") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_empirical(
      1 / p[["outcomes"]], weights = p[["probs"]]
    ))
  } else if (nm == "Degenerate") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_degenerate(1 / p[["location"]]))
  } else if (nm == "Inverse") {
    p <- distionary::parameters(distribution)
    return(p[["distribution"]])
  }
  ## END special simplifications -----------------------------------------------
  r <- range(distribution)
  qf <- function(p) {
    F0 <- distionary::eval_cdf(distribution, at = 0)
    res <- rep(NA_real_, length(p))
    base_quantiles <- distionary::eval_quantile(
      distribution, at = F0 + as.numeric(p > F0) - p
    )
    # p == 1 is a special case when the cdf of `distribution` is flat in a
    # neighbourhood of 0. It should evaluate to the right-hand side of the
    # flat part, but the quantile algorithm evaluates to the left.
    # Flip `distribution` to solve the issue.
    redo_1 <- p == 1 & base_quantiles < 0
    base_quantiles[redo_1] <- -distionary::eval_quantile(
      flip(distribution), at = 1 - F0
    )
    res <- 1 / base_quantiles
    # p == 0 is a special case whenever base_quantiles = 0 and there are
    # negative values to the base distribution. This happens
    # when the cdf is increasing past x=0, because their
    # inverse should actually be -Inf, not Inf.
    if (r[1] < 0) {
      redo_0 <- p == 0 & base_quantiles == 0
      res[redo_0] <- -Inf
    }
    res
  }
  d <- distionary::distribution(
    cdf = function(x) {
      distionary::eval_cdf(distribution, at = 0) -
        distionary::eval_cdf(distribution, at = 1 / x) +
        distionary::eval_pmf(distribution, at = 1 / x) +
        as.numeric(x >= 0)
    },
    density = function(x) {
      distionary::eval_density(distribution, at = 1 / x) / x^2
    },
    pmf = function(x) {
      distionary::eval_pmf(distribution, at = 1 / x)
    },
    quantile = qf,
    realize = function(n) {
      1 / distionary::realize(distribution, n = n)
    },
    .vtype = distionary::vtype(distribution),
    .name = "Inverse",
    .parameters = list(
      distribution = distribution
    )
  )
  if (prod(sign(r)) %in% 0:1) { # Distribution is nonnegative or nonpositive
    d[["range"]] <- rev(1 / r)
  }
  distionary:::new_distribution(d, class = "inverse")
}
