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
