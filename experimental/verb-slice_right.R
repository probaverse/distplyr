#' @rdname slice
#' @export
slice_right <- function(distribution, breakpoint, include = TRUE, ...) {
  UseMethod("slice_right")
}

#' @export
slice_right.dst <- function(distribution, breakpoint, include = TRUE, ...) {
  rng <- range(distribution)
  left <- rng[1L]
  right <- rng[2L]
  if (breakpoint > right) {
    return(distribution)
  }
  all_sliced <- FALSE
  if (breakpoint < left) {
    all_sliced <- TRUE
  }
  if (breakpoint == left) {
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
  distionary::new_distribution(l, variable = v, class = "slice_right")
}

#' @export
slice_right.finite <- function(distribution, breakpoint, include = TRUE, ...) {
  left_discretes <- distionary::prev_discrete(
    distribution,
    from = breakpoint,
    n = Inf,
    include_from = !include
  )
  if (!length(left_discretes)) {
    warning("Sliced off entire distribution. Returning NULL.")
    return(NULL)
  }
  left_probs <- distionary::eval_pmf(distribution, at = left_discretes)
  distionary::dst_empirical(left_discretes, weights = left_probs)
}
