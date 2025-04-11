#' Graft Distribution
#'
#' Replace a distribution's tail.
#' `graft_left()` takes a base distribution and grafts a distribution
#' to the left of some breakpoint; `graft_right()` grafts a distribution
#' to the right. The distribution being grafted is sliced at a breakpoint
#' and fit to the base distribution also at that breakpoint.
#' @param distribution Base distribution
#' @param graft The distribution being grafted.
#' @param breakpoint The location of the graft
#' @param include Logical; include the breakpoint in the base distribution?
#' @return Graft distribution object, which is a special type of mixture
#' distribution.
#' @examples
#' set.seed(1)
#' x <- stats::rcauchy(100)
#' base <- distionary::dst_empirical(x)
#' q <- distionary::eval_quantile(base, at = 0.9)
#' right <- distionary::dst_gpd(q, 5, 1)
#' g <- graft_right(base, right, breakpoint = q)
#' #plot(g, "cdf", n = 1001, to = 34)
#' #plot(base, "cdf", n = 1001, lty = 2, add = TRUE)
#' @rdname graft
#' @export
graft_right <- function(distribution, graft, breakpoint, include = FALSE) {
  p_left <- distionary::prob_left(
    distribution, of = breakpoint, inclusive = include
  )
  if (p_left == 1) {
    return(distribution)
  }
  if (p_left == 0) {
    return(slice_left(graft, breakpoint = breakpoint, include = include))
  }
  left <- slice_right(distribution, breakpoint = breakpoint, include = !include)
  right <- slice_left(graft, breakpoint = breakpoint, include = include)
  mixture <- mix(left, right, weights = c(p_left, 1 - p_left))
  mixture$components$breakpoint <- breakpoint
  mixture$components$include <- include
  new_graft(mixture)
}

#' @rdname graft
#' @export
graft_left <- function(distribution, graft, breakpoint, include = FALSE) {
  p_right <- distionary::prob_right(
    distribution, of = breakpoint, inclusive = include
  )
  if (p_right == 1) {
    return(distribution)
  }
  if (p_right == 0) {
    return(slice_right(graft, breakpoint = breakpoint, include = include))
  }
  left <- slice_right(graft, breakpoint = breakpoint, include = include)
  right <- slice_left(distribution, breakpoint = breakpoint, include = !include)
  mixture <- mix(left, right, weights = c(1 - p_right, p_right))
  mixture$components$breakpoint <- breakpoint
  mixture$components$include <- include
  new_graft(mixture)
}

#' @param object Object to be tested
#' @rdname graft
#' @export
is_graft <- function(object) inherits(object, "graft")


#' Constructor function for graft distributions
#'
#' @param object A special mixture of conditional distributions.
#' @note A graft distribution is a special case of a mixture distribution.
#' @inheritParams new_mixture
new_graft <- function(object, ..., class = character()) {
  new_mixture(
    object, variable = distionary::variable(object),
    class = c(class, "graft")
  )
}
