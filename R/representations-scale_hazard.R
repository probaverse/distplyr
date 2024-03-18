#' @export
eval_hazard.sc_haz <- function(distribution, at) {
  a <- distribution$scale
  a * eval_hazard(distribution$distribution, at = at)
}

#' @export
eval_chf.sc_haz <- function(distribution, at) {
  a <- distribution$scale
  a * eval_chf(distribution$distribution, at = at)
}

#' @export
eval_density.sc_haz <- function(distribution, at) {
  a <- distribution$scale
  d <- distribution$distribution
  f <- eval_density(d, at)
  S <- eval_survival(d, at)
  a * f * S^(a - 1)
}

#' @export
eval_quantile.sc_haz <- function(distribution, at) {
  a <- distribution$scale
  new_tau <- 1 - (1 - at)^(1 / a)
  eval_quantile(distribution$distribution, new_tau)
}

#' @export
eval_survival.sc_haz <- function(distribution, at) {
  a <- distribution$scale
  eval_survival(distribution$distribution, at)^a
}

#' @export
eval_cdf.sc_haz <- function(distribution, at) {
  1 - eval_survival(distribution, at)
}
