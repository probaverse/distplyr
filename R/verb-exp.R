#' Exponential Transformation of a Distribution
#'
#' Apply the exponential function to a distribution. Specifically, if `X`
#' is a random variable coming from a distribution, `exp_distribution()`
#' returns the distribution of `exp(X)`.
#'
#' @param distribution A probability distribution.
#' @return A distribution transformed by the exponential function.
#' Specifically, a distribution with subclass "exponential".
#' @examples
#' exp_distribution(distionary::dst_norm(0, 1))
#' @export
exp_distribution <- function(distribution) {
  d <- distionary::distribution(
    cdf = function(x) {
      if (x <= 0) return(0)
      distionary::eval_cdf(distribution, at = log(x))
    },
    density = function(x) {
      if (x <= 0) return(0)
      distionary::eval_density(distribution, at = log(x)) / x
    },
    pmf = function(x) {
      distionary::eval_pmf(distribution, at = log(x))
    },
    quantile = function(p) {
      exp(distionary::eval_quantile(distribution, at = p))
    },
    realize = function(n) {
      exp(distionary::realize(distribution, n = n))
    },
    .vtype = distionary::vtype(distribution),
    .name = "Exponential",
    .parameters = list(
      distribution = distribution
    )
  )
  distionary::new_distribution(d, class = "exponential")
}
