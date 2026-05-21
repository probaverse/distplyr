#' Conditional Distributions
#'
#' `slice_left()` removes probability to the left of some breakpoint,
#' conditioning the random variable to be bigger than the breakpoint.
#' `slice_right()` does the opposite: removes probability to the right,
#' conditioning to be smaller than the breakpoint.
#'
#' @param distribution Distribution to slice.
#' @param breakpoint Point at which to slice (single numeric).
#' @param include Logical; should the breakpoint be removed as well?
#' This is only realistically relevant if the
#' breakpoint has a non-zero probability of occurrence.
#' @param ... Other arguments to pass to specific methods. Currently unused.
#' @return A conditional distribution.
#' @examples
#' library(magrittr)
#' distionary::dst_norm(0, 1) %>%
#'   slice_left(-2) %>%
#'   slice_right(2) %>%
#'   distionary::enframe_cdf(at = -3:3)
#'
#' d <- distionary::dst_empirical(c(2, 5, 6, 9, 11))
#' d %>%
#'   slice_left(5) %>%
#'   distionary::eval_pmf(at = 5)
#' d %>%
#'   slice_left(5, include = TRUE) %>%
#'   distionary::eval_pmf(at = 5)
#' @rdname slice
#' @export
slice_left <- function(distribution, breakpoint, include = TRUE, ...) {
  UseMethod("slice_left")
}

#' @export
slice_left.dst <- function(distribution, breakpoint, include = TRUE, ...) {
  rng <- distionary::range(distribution)
  left <- distionary::rng[1L]
  right <- rng[2L]
  if (breakpoint < left) {
    return(distribution)
  }
  all_sliced <- FALSE
  if (breakpoint > right) {
    all_sliced <- TRUE
  }
  if (breakpoint == right) {
    if (include) {
      all_sliced <- TRUE
    } else {
      p <- distionary::eval_pmf(
        distribution,
        at = breakpoint,
        strict = FALSE
      )
      if (p == 0) {
        all_sliced <- TRUE
      } else {
        return(distionary::dst_degenerate(breakpoint))
      }
    }
  }
  if (all_sliced) {
    warning("Sliced off entire distribution. Returning NULL.")
    return(NULL)
  }
  l <- list(
    distribution = distribution,
    breakpoint = breakpoint,
    include = include
  )
  v <- distionary::variable(distribution)
  if (v == "mixed") {
    v <- "unknown" # For now. Need to evaluate cumulative discrete probs.
  }
  distionary::new_distribution(l, variable = v, class = "slice_left")
}

#' @export
slice_left.finite <- function(distribution, breakpoint, include = TRUE, ...) {
  right_discretes <- distionary::next_discrete(
    distribution,
    from = breakpoint,
    n = Inf,
    include_from = !include
  )
  if (!length(right_discretes)) {
    warning("Sliced off entire distribution. Returning NULL.")
    return(NULL)
  }
  right_probs <- distionary::eval_pmf(distribution, at = right_discretes)
  distionary::dst_empirical(right_discretes, weights = right_probs)
}
