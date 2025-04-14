#' @export
Ops.dst <- function(e1, e2) {
  op <- .Generic[[1]]
  switch(op,
    `+` = {
      if (distionary::is_distribution(e1)) {
        shift(e1, e2)
      } else {
        shift(e2, e1)
      }
    },
    `-` = {
      if (missing(e2)) {
        flip(e1)
      } else if (distionary::is_distribution(e1)) {
        shift(e1, -e2)
      } else {
        shift(flip(e2), e1)
      }
    },
    `*` = {
      if (distionary::is_distribution(e1)) {
        d <- e1
        cnst <- e2
      } else {
        d <- e2
        cnst <- e1
      }
      multiply(d, cnst)
    },
    `/` = {
      if (distionary::is_distribution(e1)) {
        multiply(e1, 1 / e2)
      } else {
        d <- e2
        cnst <- e1
        if (cnst == 0) {
        	distionary::dst_degenerate(0)
        } else if (cnst < 0) {
          invert(flip(multiply(d, -cnst)))
        } else {
          invert(multiply(d, 1 / cnst))
        }
      }
    },
    `^` = {
      if (distionary::is_distribution(e1)) {
        if (missing(e2) || !is.numeric(e2)) {
          stop("Exponent must be a numeric value.")
        }
        if (e2 == 0) {
          distionary::dst_degenerate(1)
        } else if (e2 == 1) {
          e1
        } else {
          exp_distribution(multiply(log_distribution(e1), e2))
        }
      } else if (is.numeric(e1) && distionary::is_distribution(e2)) {
        if (e1 <= 0) {
          stop("Base must be positive for exponentiation.")
        }
        exp_distribution(multiply(e2, log(e1)))
      } else {
        stop("Invalid operands for `^`. One operand must be a distribution.")
      }
    },
    `log` = {
      checkmate::assert_numeric(e2, len = 1)
      if (missing(e2)) {
        log_distribution(e1)
      } else if (e2 > 0) {
        multiply(log_distribution(e1), 1 / log(e2))
      } else {
        stop("Base must be a positive numeric value.")
      }
    },
    `log10` = {
      multiply(log_distribution(e1), 1 / log(10))
    },
    `exp` = {
      exp_distribution(e1)
    },
    stop("Operation currently not supported.")
  )
}
