#' Enframe a cumulative distribution function (CDF)
#'
#' Evaluates the CDF of the specified distributions, placing the inputs
#' and outputs in a data frame.
#'
#' @inheritParams eval_cdf
#' @param ... Distributions, or possibly a nested list of distributions,
#' inheriting the class `"dst"`, `"bidst"`, or `"multidst"`.
#' Must be of the same dimension.
#' @param arg_name Name(s) of the column(s) containing
#' the function arguments, one per dimension of the distribution. See details.
#' @param fn_prefix Name of the prefix to appear in the function
#' evaluation column(s).
#' @param sep Deprecated; no longer used.
#' @details Column names can be specified directly through
#' `arg_name` (for the input columns) and `fn_prefix` (for the distributions /
#' output columns), one name per column. If only one name is supplied, this
#' name is included as a prefix to the
#' automated naming convention, which arrives at names using the following
#' preferential order:
#'
#' 1. Given names, where applicable (as in `...` and `.l`).
#' 2. Variable names input as arguments.
#' 3. `.x` and `.y`, in the case of bivariate enframing, or `...1`, `...2`,
#'    etc. in the case of multivariate enframing.
#'
#' @return A data frame, or a tibble if the tibble package is accessible, with
#' one column per argument (one in the case of `at`; two in the case
#' of `x`, `y`), and one column per distribution input into `...`.
#' @rdname enframe_cdf
#' @export
enframe_cdf <- function(..., at, arg_name = ".arg", fn_prefix = "cdf",
                        sep = "_") {
  send_fmls_to_enframe("uni", "cdf")
}

#' @rdname enframe_cdf
#' @export
enframe_bi_cdf <- function(..., x, y, arg_name = NULL, fn_prefix = "cdf",
                           sep = "_") {
  send_fmls_to_enframe("bi", "cdf")
}

#' @rdname enframe_cdf
#' @export
enframe_multi_cdf <- function(..., .l, arg_name = NULL, fn_prefix = "cdf",
                              sep = "_") {
  send_fmls_to_enframe("multi", "cdf")
}

#' Convenience Functions for Bivariate Distributions
#'
#' Evaluate bivariate distributions using `x` and `y` arguments, instead
#' of providing a list in the `at` argument of `eval_<representation>()`.
#'
#' @param x,y Numeric vectors specifying where to evaluate the bivariate
#' representation at.
#' @inheritParams eval_cdf
#' @export
eval_bi_cdf <- function(distribution, x, y, ...) {
  eval_cdf(distribution, at = list(x, y), ...)
}

#' @export
eval_bi_density.dst <- function(distribution, x, y, strict = TRUE, ...) {
  stop("Expecting a bivariate distribution; received univariate.")
}

#' @export
eval_bi_density.bidst <- function(distribution, x, y, strict = TRUE, ...) {
  if (all(variable(distribution) == "continuous")) {
    stop("Cannot find this distribution's density function.")
  }
  if (strict) {
    stop("This distribution does not have a density function. ",
         "Maybe you want to evaluate outside of strict mode?")
  } else {
    if (all(variable(distribution) == "discrete")) {
      return(rep(0, vctrs::vec_size_common(x, y)))
    } else {
      stop("Cannot find the derivative of the cdf.")
    }
  }
}

#' @export
eval_bi_density.multidst <- function(distribution, x, y, strict = TRUE, ...) {
  d <- dimension(distribution)
  if (d == 2) {
    return(eval_multi_density(distribution, list(xy[[1]], xy[[2]])))
  }
  stop("Expecting a bivariate distribution; received ", d, "-variate.")
}
