#' Logarithmic Transformation of a Distribution
#'
#' Apply the logarithm function to a distribution. Specifically, if `X`
#' is a random variable coming from a distribution, `log_distribution()`
#' returns the distribution of `log(X)` with a specified base.
#'
#' @param distribution A probability distribution.
#' @param base A positive numeric value specifying the base of the logarithm.
#' Defaults to Euler's constant (natural logarithm).
#' @return A distribution transformed by the logarithm function.
#' Specifically, a distribution with subclass "logarithmic".
#' @note An error is returned if the original distribution
#' has non-positive values as possible outcomes.
#' @examples
#' log_distribution(distionary::dst_unif(1, 10)) # Natural log
#' log_distribution(distionary::dst_unif(1, 10), base = 10) # Log base 10
#' @export
log_distribution <- function(distribution, base = exp(1)) {
  if (base <= 0) {
    stop("Base must be a positive numeric value.")
  }
  r <- range(distribution)
  if (r[1] < 0) {
    stop("Cannot apply logarithm to a distribution with non-positive values.")
  } else if (r[1] == 0 && distionary::vtype(distribution) != "continuous" &&
             distionary::eval_pmf(distribution, at = 0) > 0) {
    stop("Cannot apply logarithm to a distribution having mass at 0.")
  }
  scale_factor <- 1 / log(base)
  d <- distionary::distribution(
    cdf = function(x) {
      distionary::eval_cdf(distribution, at = exp(x / scale_factor))
    },
    density = function(x) {
      exp_x <- exp(x / scale_factor)
      distionary::eval_density(distribution, at = exp_x) * exp_x / scale_factor
    },
    pmf = function(x) {
      distionary::eval_pmf(distribution, at = exp(x / scale_factor))
    },
    quantile = function(p) {
      log(distionary::eval_quantile(distribution, at = p)) * scale_factor
    },
    realize = function(n) {
      log(distionary::realize(distribution, n = n)) * scale_factor
    },
    .vtype = distionary::vtype(distribution),
    .name = paste("Logarithmic (base", base, ")"),
    .parameters = list(
      distribution = distribution,
      base = base
    )
  )
  if (distionary:::is_intrinsic(distribution, "range")) {
    d[["range"]] <- log(r, base = base)
  }
  distionary::new_distribution(d, class = "logarithmic")
}

#' @export
print.logarithmic <- function(x, ...) {
  base <- distionary::parameters(x)[["base"]]
  if (base == exp(1)) {
    base <- "e"
  }
  attr(x, "name") <- paste("Logarithmic (base", base, ")")
  parameters(x)[["base"]] <- NULL
  print(x, ...)
}
