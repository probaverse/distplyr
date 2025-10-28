#' Exponential Transformation of a Distribution
#'
#' Internal function. Users should use `exp()` instead.
#' Returns the distribution of `exp(X)` where `X` is a random variable
#' following the input distribution.
#'
#' @param distribution A probability distribution.
#' @return A distribution transformed by the exponential function.
#' Specifically, a distribution with subclass "exponential".
#' @noRd
exp_distribution <- function(distribution) {
  checkmate::assert_class(distribution, "dst")
  nm <- distionary::pretty_name(distribution)
  if (nm == "Null") {
    return(distribution)
  }
  ## BEGIN special simplifications ---------------------------------------------
  if (nm == "Normal") {
    param <- distionary::parameters(distribution)
    meanlog <- param[["mean"]]
    sdlog <- param[["sd"]]
    return(distionary::dst_lnorm(meanlog = meanlog, sdlog = sdlog))
  }
  if (nm == "Finite") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_empirical(
      exp(p[["outcomes"]]), weights = p[["probs"]]
    ))
  }
  if (nm == "Degenerate") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_degenerate(exp(p[["location"]])))
  }
  if (nm == "Logarithmic") {
    p <- distionary::parameters(distribution)
    inner_dist <- p[["distribution"]]
    return(inner_dist)
  }
  ## END special simplifications -----------------------------------------------
  d <- distionary::distribution(
    cdf = function(x) {
      res <- rep(0, length(x))
      res[x > 0] <- distionary::eval_cdf(distribution, at = log(x[x > 0]))
      res
    },
    density = function(x) {
      res <- rep(0, length(x))
      res[x > 0] <- distionary::eval_density(
        distribution, at = log(x[x > 0])
      ) / x[x > 0]
      res
    },
    pmf = function(x) {
      res <- rep(0, length(x))
      res[x > 0] <- distionary::eval_pmf(distribution, at = log(x[x > 0]))
      res
    },
    quantile = function(p) {
      exp(distionary::eval_quantile(distribution, at = p))
    },
    realize = function(n) {
      exp(distionary::realize(distribution, n = n))
    },
    range = exp(range(distribution)),
    .vtype = distionary::vtype(distribution),
    .name = "Exponentiated",
    .parameters = list(
      distribution = distribution
    )
  )
  distionary:::new_distribution(d, class = "exponential")
}
