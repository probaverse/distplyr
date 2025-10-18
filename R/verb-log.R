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
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_number(base, finite = TRUE, lower = 0, na.ok = TRUE)
  nm <- distionary::pretty_name(distribution)
  if (nm == "Null") {
    return(distribution)
  }
  if (is.na(base)) {
    return(distionary::dst_null())
  }
  if (nm == "Log Normal") {
    param <- distionary::parameters(distribution)
    mu <- param[["meanlog"]] / log(base)
    sigma <- param[["sdlog"]] / log(base)
    return(distionary::dst_norm(mean = mu, sd = sigma))
  }
  p_neg <- distionary::prob_left(distribution, of = 0, inclusive = TRUE)
  if (p_neg > 0) {
    warning(
      "Cannot apply logarithm to a distribution with non-positive values. ",
      "Returning a Null distribution."
    )
  }
  r <- range(distribution)
  scale_factor <- 1 / log(base)
  if (base == exp(1)) {
    base_print <- "e"
  } else {
    base_print <- base
  }
  d <- distionary::distribution(
    cdf = function(x) {
      distionary::eval_cdf(distribution, at = exp(x / scale_factor))
    },
    survival = function(x) {
      distionary::eval_survival(distribution, at = exp(x / scale_factor))
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
    .name = paste0("Logarithmic (base ", base_print, ")"),
    .parameters = list(
      distribution = distribution,
      base = base
    )
  )
  if (distionary:::is_intrinsic(distribution, "range")) {
    d[["range"]] <- log(r, base = base)
  }
  distionary:::new_distribution(d, class = "logarithmic")
}
