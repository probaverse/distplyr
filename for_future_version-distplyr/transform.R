#' Transform a Distribution
#'
#' If \code{X} has distribution \code{.dst}, then this function
#' returns the distribution of \code{g(X)}
#'
#' @param .dst Original distribution to transform
#' @param g A (strictly increasing) function, vectorized.
#' @param ginv Inverse function of \code{g}, vectorized.
#' @param gprime,ginvprime Derivative functions of \code{g} and \code{ginv},
#' vectorized.
#' Not required if there is no density, and if there is a density, only
#' one is required.
#' @return Object of class "dst" of the transformed random variable.
#' @details Functions should apply to all real numbers.
#' @export
rv_transform <- function(.dst, g, ginv, gprime, ginvprime) {
	if (missing(ginvprime) && !missing(gprime)) {
		ginvprime <- function(x) 1 / gprime(ginv(x))
	}
	cdf  <- get_cdf(.dst)
	qf   <- get_quantile(.dst)
	rand <- get_randfn(.dst)
	pf   <- get_probfn(.dst)
	.has_pdf <- has_pdf(.dst)
	.has_pmf <- has_pmf(.dst)
	if (isTRUE(.has_pdf)) {
		if (missing(ginvprime)) {
			warning("Didn't provide a derivative of the transformation, ",
					"yet .dst has a density. New distribution will be ",
					"missing this density.")
			new_ddst <- NA
		} else {
			new_ddst <- function(x) pf(ginv(x)) * ginvprime(x)
		}
	} else if (isTRUE(.has_pmf)) {
		new_ddst <- function(x) pf(ginv(x))
	} else {
		new_ddst <- NA
	}
	dst(
		name = paste("Transformed", .dst$name),
		param = NULL,
		fun_cumu  = function(x) cdf(ginv(x)),
		fun_quant = function(x) g(qf(x)),
		fun_rand  = function(n) g(rand(n)),
		fun_prob  = new_ddst,
		prop = list(mean = NA,
					sd   = NA,
					var  = NA,
					has_pdf = .has_pdf,
					has_pmf = .has_pmf)
	)
}
