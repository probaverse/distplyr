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
#' (i.e., `eval_pmf(distribution, at = 0, strict = FALSE)` is non-zero),
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
  p_zero <- distionary::eval_pmf(distribution, at = 0, strict = FALSE)
  if (p_zero > 0) {
    stop("Cannot invert a distribution for which 0 is a possible outcome.")
  }
  dist <- list(
    distribution = distribution
  )
  distionary::new_distribution(
  	dist, variable = distionary::variable(distribution), class = "inverse"
  )
}

#' @export
eval_cdf.inverse <- function(distribution, at) {
  dist <- distribution$distribution
  distionary::eval_cdf(dist, at = 0) -
    distionary::eval_cdf(dist, at = 1 / at) +
    distionary::eval_pmf(dist, at = 1 / at, strict = FALSE) +
    as.numeric(at >= 0)
}

#' @export
eval_density.inverse <- function(distribution, at, strict) {
  distionary::eval_density(
    distribution$distribution, at = 1 / at, strict = strict
  ) / at^2
}

#' @export
eval_pmf.inverse <- function(distribution, at, strict) {
  distionary::eval_pmf(
    distribution$distribution, at = 1 / at, strict = strict
  )
}

#' @export
eval_quantile.inverse <- function(distribution, at) {
  quantile_0 <- distionary::eval_quantile(distribution$distribution, at = 0)
  1 / distionary::eval_quantile(
    distribution, at = quantile_0 + (at > quantile_0) - at
  )
}

#' @export
realise.inverse <- function(distribution, n) {
  1 / distionary::realise(distribution$distribution, n = n)
}
