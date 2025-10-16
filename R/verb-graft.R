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
#' base <- distionary::dst_norm(0, 1)
#' q <- distionary::eval_quantile(base, at = 0.9)
#' right <- q + distionary::dst_gp(1, 0.3)
#' g <- graft_right(base, right, breakpoint = q)
#' plot(g, "cdf", n = 1001)
#' plot(base, "cdf", n = 1001, lty = 2, col = "green", add = TRUE)
#' @rdname graft
#' @export
graft_right <- function(distribution, graft, breakpoint, include = FALSE) {
  if (distionary::vtype(distribution) != "continuous" ||
      distionary::vtype(graft) != "continuous") {
    warning(
      "A non-continuous distribution has been entered into a distplyr verb.\n",
      "At this stage of distplyr's development, some inaccuracies can be\n",
      "expected in these cases, particularly for quantile calculations."
    )
  }
  p_left <- distionary::prob_left(
    distribution, of = breakpoint, inclusive = include
  )

  if (p_left == 1) {
    return(distribution)
  }
  if (p_left == 0) {
    return(slice_left(graft, breakpoint = breakpoint, include = include))
  }
  left <- slice_right(
    distribution, breakpoint = breakpoint, include = !include
  )
  right <- slice_left(
    graft, breakpoint = breakpoint, include = include
  )
  attach_graft_ends(
    left, right,
    p_left = p_left,
    p_right = 1 - p_left,
    breakpoint = breakpoint
  )
}

#' @rdname graft
#' @export
graft_left <- function(distribution, graft, breakpoint, include = FALSE) {
  if (distionary::vtype(distribution) != "continuous" ||
      distionary::vtype(graft) != "continuous") {
    warning(
      "A non-continuous distribution has been entered into a distplyr verb.\n",
      "At this stage of distplyr's development, some inaccuracies can be\n",
      "expected in these cases, particularly for quantile calculations."
    )
  }
  p_right <- distionary::prob_right(
    distribution, of = breakpoint, inclusive = include
  )
  if (p_right == 1) {
    return(distribution)
  }
  if (p_right == 0) {
    return(slice_right(graft, breakpoint = breakpoint, include = include))
  }
  left <- slice_right(
    graft, breakpoint = breakpoint, include = include
  )
  right <- slice_left(
    distribution, breakpoint = breakpoint, include = !include
  )
  attach_graft_ends(
    left, right,
    p_left = 1 - p_right,
    p_right = p_right,
    breakpoint = breakpoint
  )
}

#' @noRd
attach_graft_ends <- function(left, right, p_left, p_right, breakpoint) {
  mixture <- mix(left, right, weights = c(p_left, p_right))
  parameters(mixture)[["breakpoint"]] <- breakpoint
  mixture[["quantile"]] <- function(p) {
    p_cutoff <- p_left
    res <- numeric(0L)
    for (i in seq_along(p)) {
      if (is.na(p[i])) {
        res[i] <- NA_real_
      } else if (p[i] <= p_cutoff) {
        new_p <- p[i] / p_cutoff
        this_d <- left
        res[i] <- distionary::eval_quantile(this_d, at = new_p)
      } else {
        new_p <- (p[i] - p_cutoff) / (1 - p_cutoff)
        this_d <- right
        res[i] <- distionary::eval_quantile(this_d, at = new_p)
      }
    }
    res
  }
  distionary::new_distribution(mixture, class = "graft")
}
