#' Logarithmic Transformation of a Distribution
#'
#' Internal function. Users should use `log()` instead.
#' Returns the distribution of `log(X)` where `X` is a random variable
#' following the input distribution.
#'
#' @param distribution A probability distribution.
#' @param base A positive numeric value specifying the base of the logarithm.
#' Defaults to Euler's constant (natural logarithm).
#' @returns A distribution transformed by the logarithm function.
#' @noRd
log_distribution <- function(distribution, base = exp(1)) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_number(base, finite = TRUE, lower = 0, na.ok = TRUE)
  nm <- distionary::pretty_name(distribution)
  if (nm == "Null") {
    return(distribution)
  }
  if (is.na(base)) {
    return(distionary::dst_null())
  }
  p_neg <- distionary::prob_left(distribution, of = 0, inclusive = TRUE)
  if (p_neg > 0) {
    stop(
      "Cannot apply logarithm to a distribution with non-positive values."
    )
  }
  if (base == 0) {
    return(distionary::dst_degenerate(0))
  }
  if (base != exp(1)) {
    return(log_distribution(distribution) / log(base))
  }
  ## BEGIN special simplifications ---------------------------------------------
  if (nm == "Log Normal") {
    param <- distionary::parameters(distribution)
    mu <- param[["meanlog"]]
    sigma <- param[["sdlog"]]
    return(distionary::dst_norm(mean = mu, sd = sigma))
  }
  if (nm == "Finite") {
    p <- distionary::parameters(distribution)
    outcomes <- p[["outcomes"]]
    probs <- p[["probs"]]
    return(distionary::dst_empirical(
      log(outcomes), weights = probs
    ))
  }
  if (nm == "Degenerate") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_degenerate(log(p[["location"]])))
  }
  if (nm == "Exponentiated") {
    p <- distionary::parameters(distribution)
    inner_dist <- p[["distribution"]]
    return(p[["distribution"]])
  }
  ## END special simplifications -----------------------------------------------
  r <- range(distribution)
  d <- distionary::distribution(
    cdf = function(x) {
      distionary::eval_cdf(distribution, at = exp(x))
    },
    survival = function(x) {
      distionary::eval_survival(distribution, at = exp(x))
    },
    density = function(x) {
      exp_x <- exp(x)
      distionary::eval_density(distribution, at = exp_x) * exp_x
    },
    pmf = function(x) {
      distionary::eval_pmf(distribution, at = exp(x))
    },
    quantile = function(p) {
      log(distionary::eval_quantile(distribution, at = p))
    },
    realize = function(n) {
      log(distionary::realize(distribution, n = n))
    },
    .vtype = distionary::vtype(distribution),
    .name = "Logarithmic",
    .parameters = list(distribution = distribution)
  )
  if (distionary:::is_intrinsic(distribution, "range")) {
    d[["range"]] <- log(r)
  }
  distionary:::new_distribution(d, class = "logarithmic")
}
