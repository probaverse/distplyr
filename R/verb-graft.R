#' Graft Distribution
#'
#' Replace a distribution's tail with a hard cut at a threshold. For a
#' continuous transition instead, see [smooth_graft_right()] and
#' [smooth_graft_left()].
#' `graft_left()` takes a base distribution and grafts a tail
#' to the left of the threshold; `graft_right()` grafts a tail
#' to the right. The tail being grafted is trimmed at `threshold`
#' and fit to the base distribution also at that value.
#' @param distribution Base (body) distribution.
#' @param tail The tail distribution being grafted on.
#' @param threshold Value on the real line where the tail is attached.
#' @param include Logical; include `threshold` in the base distribution?
#' @param ... Currently unused; must be empty.
#' @return Graft distribution object, which is a special type of mixture
#' distribution.
#' @examples
#' base <- distionary::dst_norm(0, 1)
#' q <- distionary::eval_quantile(base, at = 0.9)
#' right <- q + distionary::dst_gp(1, 0.3)
#' g <- graft_right(base, right, threshold = q)
#' # plot(g, "cdf", n = 1001)
#' # plot(base, "cdf", n = 1001, lty = 2, col = "green", add = TRUE)
#' @rdname graft
#' @export
graft_right <- function(distribution, tail, threshold, ..., include = FALSE) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_class(tail, "dst")
  checkmate::assert_number(threshold, finite = TRUE, na.ok = FALSE)
  rlang::check_dots_empty()
  p_connect <- distionary::prob_right(
    distribution,
    of = threshold,
    inclusive = !include
  )
  if (p_connect == 0) {
    return(distribution)
  }
  graft_trimmed <- trim_left(
    tail,
    of = threshold,
    include = include
  )
  if (p_connect == 1) {
    return(graft_trimmed)
  }
  base_trimmed <- trim_right(
    distribution,
    of = threshold,
    include = !include
  )
  attach_graft_ends(
    base_trimmed,
    graft_trimmed,
    p_left = 1 - p_connect,
    p_right = p_connect
  )
}

#' @rdname graft
#' @export
graft_left <- function(distribution, tail, threshold, ..., include = FALSE) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_class(tail, "dst")
  checkmate::assert_number(threshold, finite = TRUE, na.ok = FALSE)
  rlang::check_dots_empty()
  p_connect <- distionary::prob_left(
    distribution,
    of = threshold,
    inclusive = !include
  )
  if (p_connect == 0) {
    return(distribution)
  }
  graft_trimmed <- trim_right(
    tail,
    of = threshold,
    include = include
  )
  if (p_connect == 1) {
    return(graft_trimmed)
  }
  base_trimmed <- trim_left(
    distribution,
    of = threshold,
    include = !include
  )
  attach_graft_ends(
    graft_trimmed,
    base_trimmed,
    p_left = p_connect,
    p_right = 1 - p_connect
  )
}

#' @noRd
attach_graft_ends <- function(left, right, p_left, p_right) {
  mixture <- mix(left, right, weights = c(p_left, p_right))
  mixture[["quantile"]] <- function(p) {
    p_cutoff <- p_left
    res <- numeric(length(p))
    for (i in seq_along(p)) {
      if (is.na(p[i])) {
        res[i] <- NA_real_
      } else if (p[i] <= p_cutoff) {
        new_p <- p[i] / p_cutoff
        res[i] <- distionary::eval_quantile(left, at = new_p)
      } else {
        new_p <- (p[i] - p_cutoff) / (1 - p_cutoff)
        res[i] <- distionary::eval_quantile(right, at = new_p)
      }
    }
    res
  }
  distionary:::new_distribution(mixture, class = "graft")
}
