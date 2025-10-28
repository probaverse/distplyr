#' Mathematical Transformations for Distributions
#'
#' Apply mathematical functions like `log()` and `exp()` to probability
#' distributions. If `X` is a random variable following a distribution,
#' these functions return the distribution of the transformed variable.
#'
#' @details
#' These S3 methods extend base R functions to work with distributions.
#'
#' @param x A probability distribution object.
#' @param ... Additional arguments passed to specific methods.
#' @aliases exp.dst log.dst log10.dst sqrt.dst
#' @usage
#' ## S3 method for class 'dst'
#' log(x, base = exp(1))
#'
#' ## S3 method for class 'dst'
#' log10(x)
#'
#' ## S3 method for class 'dst'
#' exp(x)
#'
#' ## S3 method for class 'dst'
#' sqrt(x)
#'
#' @section Supported Functions:
#' \describe{
#'   \item{`log(x, base = exp(1))`}{Returns the distribution of `log(X)`.
#'     The base can be specified (defaults to natural log). An error is returned
#'     if the distribution has non-positive values as possible outcomes.}
#'   \item{`log10(x)`}{Returns the distribution of `log10(X)`. Equivalent to
#'     `log(x, base = 10)`.}
#'   \item{`exp(x)`}{Returns the distribution of `exp(X)`.}
#'   \item{`sqrt(x)`}{Returns the distribution of `sqrt(X)`. Equivalent to
#'     `x^0.5`. Requires all values to be non-negative.}
#' }
#'
#' @section Power Operator:
#' The power operator `^` also works with distributions (see [Ops.dst()]).
#' When raising a distribution to a numeric power (e.g., `dst^2`), it uses
#' the relationship `X^a = exp(a * log(X))`, combining both exponential and
#' logarithmic transformations.
#'
#' @return A transformed distribution object.
#'
#' @examples
#' # Logarithmic transformations
#' d <- distionary::dst_unif(1, 10)
#' log(d)              # Natural log
#' log(d, base = 10)   # Log base 10
#' log10(d)            # Also log base 10
#' sqrt(d)             # Square root of uniform
#'
#' # Exponential transformation
#' d2 <- distionary::dst_norm(0, 1)
#' d3 <- distionary::dst_beta(5, 4)
#' exp(d2)             # Log-normal distribution
#' exp(d3)             # No simplification
#'
#' # These can be combined
#' log(exp(d2))        # Returns back to normal distribution
#' log(exp(d3))        # Still returns d3.
#' 5^(log(d3, base = 5)) # Still returns d3.
#' @seealso
#' - [Ops.dst()] for the `^` operator and other arithmetic operations
#' - [shift()], [multiply()], [flip()], [invert()] for linear transformations
#' @export
Math.dst <- function(x, ...) {
  op <- .Generic[[1]]
  switch(op,
    `log` = {
      log_distribution(x, ...)
    },
    `log10` = {
      ellipsis::check_dots_empty()
      log_distribution(x, base = 10)
    },
    `exp` = {
      ellipsis::check_dots_empty()
      exp_distribution(x)
    },
    `sqrt` = {
      ellipsis::check_dots_empty()
      x^0.5
    },
    stop("Operation currently not supported.")
  )
}
