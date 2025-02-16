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
  tau0 <- distionary::eval_cdf(distribution$distribution, at = 0)
	1 / distionary::eval_quantile(
		distribution$distribution, at = tau0 + (at > tau0) - at
	)
}

#' @export
realise.inverse <- function(distribution, n) {
	1 / distionary::realise(distribution$distribution, n = n)
}
