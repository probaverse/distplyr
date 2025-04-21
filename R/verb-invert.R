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
#' @examples
#' 1 / (distionary::dst_pois(3.4) + 1)
#' invert(distionary::dst_norm(0, 1))
#' @export
invert <- function(distribution) {
  if (distionary::vtype(distribution) != "continuous") {
    warning(
      "A non-continuous distribution has been entered into a distplyr verb.\n",
      "At this stage of distplyr's development, some inaccuracies can be\n",
      "expected in these cases, particularly for quantile calculations."
    )
  }
  p_zero <- distionary::eval_pmf(distribution, at = 0)
  if (p_zero > 0) {
    stop("Cannot invert a distribution for which 0 is a possible outcome.")
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
    quantile = function(p) {
      F0 <- distionary::eval_cdf(distribution, at = 0)
      1 / distionary::eval_quantile(
        distribution, at = F0 + as.numeric(p > F0) - p
      )
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
  distionary::new_distribution(d, class = "inverse")
}
