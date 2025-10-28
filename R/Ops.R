#' Arithmetic Operations for Distributions
#'
#' Apply arithmetic operators to probability distributions. These operations
#' transform distributions in intuitive ways, treating them similarly to
#' numeric values.
#'
#' @details
#' These S3 methods extend arithmetic operators to work with distributions.
#'
#' @param d A probability distribution.
#' @param a A numeric value.
#' @aliases +.dst -.dst *.dst /.dst ^.dst
#' @usage
#' ## S3 methods for class 'dst'
#' d + a
#' d - a
#' -d
#' d * a
#' d / a
#' a / d
#' d ^ a
#' a ^ d
#' @section Supported Operators:
#' \describe{
#'   \item{`d + a` or `a + d`}{Shifts the distribution by adding constant `a`.
#'     Equivalent to [shift()].}
#'   \item{`d - a`}{Shifts the distribution by subtracting constant `a`.
#'     Equivalent to `shift(d, -a)`.}
#'   \item{`-d`}{Flips the distribution (negation). Equivalent to [flip()].}
#'   \item{`d * a` or `a * d`}{Scales the distribution by multiplying by
#'     constant `a`. Equivalent to [multiply()].}
#'   \item{`d / a`}{Scales the distribution by dividing by constant `a`.
#'     Equivalent to `multiply(d, 1/a)`.}
#'   \item{`a / d`}{Returns the distribution of `a / X` (reciprocal scaling).
#'     For `a = 1`, equivalent to [invert()].}
#'   \item{`d ^ a`}{Raises the distribution to power `a`. For positive
#'     distributions only, computed as `exp(a * log(d))`.}
#'   \item{`a ^ d`}{Returns the distribution of `a^X`. Requires positive
#'     base `a`.}
#' }
#'
#' @section Power Operator Details:
#' The power operator `^` deserves special attention:
#' - When the **base is a distribution** (e.g., `dst_beta(1, 1)^2`),
#'   it computes the distribution of `X^a` using the transformation
#'   `exp(a * log(X))`. This requires all values in the distribution to be
#'   positive.
#' - When the **exponent is a distribution** (e.g., `2^dst_norm(0, 1)`),
#'   it computes the distribution of `a^X` using `exp(X * log(a))`.
#'   The base `a` must be positive.
#'
#' These implementations internally use both [Math.dst()] methods `log()`
#' and `exp()`.
#'
#' @return A transformed distribution object.
#'
#' @examples
#' d <- distionary::dst_beta(3, 2)
#'
#' # Shifting and scaling
#' d + 10          # Shift right by 10
#' d * 2           # Scale by 2
#' 3 * d - 5       # Scale then shift
#'
#' # Power operations
#' exp(d)          # e^X: exponential of Beta
#' 2^d             # 2^X: base 2 raised to Beta
#'
#' # With positive distributions
#' d_pos <- distionary::dst_unif(1, 2)
#' d_pos^2         # X^2: uniform squared
#' d_pos^0.5       # sqrt(X): square root
#' sqrt(d_pos)     # Equivalent to d_pos^0.5
#'
#' @seealso
#' - [shift()], [multiply()], [flip()], [invert()] for the underlying
#'   transformation functions
#' - [Math.dst()] for `log()`, `exp()`, and `sqrt()` functions
#' @export
Ops.dst <- function(e1, e2) {
  op <- .Generic[[1]]
  switch(op,
    `+` = {
      if (missing(e2)) {
        return(e1)
      }
      e1_is_dst <- distionary::is_distribution(e1)
      e2_is_dst <- distionary::is_distribution(e2)
      if (e1_is_dst && e2_is_dst) {
        stop(
          "Binary operations of two distributions is currently not supported."
        )
      }
      if (e1_is_dst) {
        shift(e1, e2)
      } else {
        shift(e2, e1)
      }
    },
    `-` = {
      if (missing(e2)) {
        return(flip(e1))
      }
      e1_is_dst <- distionary::is_distribution(e1)
      e2_is_dst <- distionary::is_distribution(e2)
      if (e1_is_dst && e2_is_dst) {
        stop(
          "Binary operations of two distributions is currently not supported."
        )
      }
      if (e1_is_dst) {
        shift(e1, -e2)
      } else {
        shift(flip(e2), e1)
      }
    },
    `*` = {
      e1_is_dst <- distionary::is_distribution(e1)
      e2_is_dst <- distionary::is_distribution(e2)
      if (e1_is_dst && e2_is_dst) {
        stop(
          "Binary operations of two distributions is currently not supported."
        )
      }
      if (e1_is_dst) {
        multiply(e1, e2)
      } else {
        multiply(e2, e1)
      }
    },
    `/` = {
      e1_is_dst <- distionary::is_distribution(e1)
      e2_is_dst <- distionary::is_distribution(e2)
      if (e1_is_dst && e2_is_dst) {
        stop(
          "Binary operations of two distributions is currently not supported."
        )
      }
      if (e1_is_dst) {
        multiply(e1, 1 / e2)
      } else {
        d <- e2
        cnst <- e1
        if (cnst == 0) {
          if (distionary::eval_pmf(d, at = 0) > 0) {
            stop("Division by zero is undefined.")
          }
        	return(distionary::dst_degenerate(0))
        }
        multiply(invert(d), cnst)
      }
    },
    `^` = {
      e1_is_dst <- distionary::is_distribution(e1)
      e2_is_dst <- distionary::is_distribution(e2)
      if (e1_is_dst && e2_is_dst) {
        stop(
          "Power operations of two distributions is currently not supported."
        )
      }
      if (e1_is_dst) {
        ## Distribution base, numeric exponent
        if (e2 == 0) {
          return(distionary::dst_degenerate(1))
        }
        if (e2 == 1) {
          return(e1)
        }
        if (distionary::eval_cdf(e1, at = 0) == 0) {
          return(exp_distribution(multiply(log_distribution(e1), e2)))
        }
        stop(
          "Sorry, but power operations are currently not supported for ",
          "distributions with non-positive values."
        )
      }
      if (is.numeric(e1) && e2_is_dst) {
        ## Numeric base, distribution exponent
        if (e1 <= 0) {
          stop("Base must be positive when exponentiating a distribution.")
        }
        return(exp_distribution(multiply(e2, log(e1))))
      }
      stop("Invalid operands for `^`. One operand must be a distribution.")
    }
  )
}
