#' Exponential Transformation of a Distribution
#'
#' Apply the exponential function to a distribution. Specifically, if `X`
#' is a random variable coming from a distribution, `exp_distribution()`
#' returns the distribution of `exp(X)`.
#'
#' @param distribution A probability distribution.
#' @return A distribution transformed by the exponential function.
#' Specifically, a distribution with subclass "exponential".
#' @noRd
exp_distribution <- function(distribution) {
  if (distionary::vtype(distribution) != "continuous") {
    warning(
      "A non-continuous distribution has been entered into a distplyr verb.\n",
      "At this stage of distplyr's development, some inaccuracies can be\n",
      "expected in these cases, particularly for quantile calculations."
    )
  }
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
    .vtype = distionary::vtype(distribution),
    .name = "Exponentiated",
    .parameters = list(
      distribution = distribution
    )
  )
  distionary::new_distribution(d, class = "exponential")
}
