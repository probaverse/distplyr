#' Inverse Distribution
#'
#' Access the inverse of a distribution. Specifically, if `X` is a random
#' variable coming from a distribution, `invert()` returns the
#' distribution of `1 / X`.
#'
#' @param distribution A probability distribution.
#' @return An inverse distribution.
#' Specifically, a distribution with subclass "inverse".
#' @note An error is returned if the original distribution
#' has 0 as a possible outcome
#' (i.e., `eval_pmf(distribution, at = 0)` is non-zero),
#' because 0 does not have a reciprocal.
#'
#' You can also obtain the inverse distribution by putting
#' the distribution in the denominator of `/`.
#' @seealso `flip()`, `scale()`
#' @details
#' Simplifications for this verb include:
#'
#' - "Finite" distributions become other Finite distributions.
#' @examples
#' 1 / (distionary::dst_pois(3.4) + 1)
#' invert(distionary::dst_norm(0, 1))
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
    quantile = function(p) {
      F0 <- distionary::eval_cdf(distribution, at = 0)
      res <- rep(NA_real_, length(p))
      res[p != 1] <- 1 / distionary::eval_quantile(
        distribution, at = F0 + as.numeric(p[p != 1] > F0) - p[p != 1]
      )
      # p == 1 is a special case when the cdf of `distribution` is flat in a
      # neighbourhood of 0. It should evaluate to the right-hand side of the
      # flat part, but the quantile algorithm evaluates to the left.
      # Flip `distribution` to solve the issue.
      res[p == 1] <- -1 / distionary::eval_quantile(
        flip(distribution), at = F0
      )
      res
    },
    realize = function(n) {
      1 / distionary::realize(distribution, n = n)
    },
    .vtype = distionary::vtype(distribution),
    .name = "Inverse",
    .parameters = list(
      distribution = distribution
    )
  )
  distionary:::new_distribution(d, class = "inverse")
}
