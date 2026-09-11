#' Graft a tail onto a distribution
#'
#' Replace one end of a distribution with a different model of that end.
#' `graft_right()` keeps the body of `body` below `of` and hands everything
#' above it to `tail`; `graft_left()` does the same at the lower end. The
#' result is a *graft*: the tail is trimmed at `of`, and both pieces are
#' weighted so that the body's own probability of reaching past `of` is the
#' share the tail receives.
#'
#' @details
#' # Where the knot goes
#'
#' `of` is the knot, and the body and the tail each decide separately what
#' to do with the probability sitting exactly on it. That is two questions,
#' not one, so there are two arguments:
#'
#' - `knot_body_action` --- what the body does with its own mass at the
#'   knot: `"discard"` it (the default, stopping the body just below the
#'   knot), `"keep"` it, or `"split"` it in half.
#' - `knot_tail_action` --- the same question for the tail: `"keep"` its
#'   mass at the knot (the default, starting the tail at the knot rather
#'   than above it), `"discard"` it, or `"split"` it.
#'
#' Mass the body does not retain is not destroyed; it passes into the
#' tail's share of the total, which is why every combination of the two
#' still gives a distribution. Both arguments matter only where the
#' distribution in question carries mass exactly at `of`, so for a pair of
#' continuous distributions they make no difference at all.
#'
#' `trim_left()` and `trim_right()` settle the same question with a single
#' `knot_action`, having only one distribution to ask about.
#'
#' @param body Distribution supplying the part of the range that is kept.
#' @param tail Distribution supplying the replacement end.
#' @param of Value on the real line where the tail is attached: the knot.
#' @param ... Currently unused; must be empty.
#' @param knot_body_action What the body does with its probability at the
#' knot: `"discard"`, `"keep"`, or `"split"`. See Details.
#' @param knot_tail_action What the tail does with its probability at the
#' knot: `"keep"`, `"discard"`, or `"split"`. See Details.
#' @return A graft: a distribution made of the body below the knot and the
#' tail above it (or the other way round, for `graft_left()`), which is a
#' special type of mixture distribution.
#' @seealso [trim_left()] and [trim_right()], which discard an end rather
#' than replacing it.
#' @examples
#' body <- distionary::dst_norm(0, 1)
#' q <- distionary::eval_quantile(body, at = 0.9)
#' upper <- q + distionary::dst_gp(1, 0.3)
#' g <- graft_right(body, upper, of = q)
#' # plot(g, "cdf", n = 1001)
#' # plot(body, "cdf", n = 1001, lty = 2, col = "green", add = TRUE)
#' @rdname graft
#' @export
graft_right <- function(body, tail, of, ...,
                        knot_body_action = c("discard", "keep", "split"),
                        knot_tail_action = c("keep", "discard", "split")) {
  checkmate::assert_class(body, "dst")
  checkmate::assert_class(tail, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  rlang::check_dots_empty()
  knot_body_action <- rlang::arg_match0(
    knot_body_action, c("discard", "keep", "split"), "knot_body_action"
  )
  knot_tail_action <- rlang::arg_match0(
    knot_tail_action, c("keep", "discard", "split"), "knot_tail_action"
  )
  # Whatever of the knot the body does not retain belongs to the tail's
  # share, which is what keeps the two weights summing to 1 whichever way
  # the two actions are set.
  p_connect <- distionary::prob_right(body, of = of, inclusive = FALSE) +
    knot_mass(body, of) - knot_retained(body, of, knot_body_action)
  if (p_connect == 0) {
    return(body)
  }
  tail_trimmed <- trim_left(tail, of = of, knot_action = knot_tail_action)
  if (p_connect == 1) {
    return(tail_trimmed)
  }
  body_trimmed <- trim_right(body, of = of, knot_action = knot_body_action)
  attach_graft_ends(
    body_trimmed,
    tail_trimmed,
    p_left = 1 - p_connect,
    p_right = p_connect
  )
}

#' @rdname graft
#' @export
graft_left <- function(body, tail, of, ...,
                       knot_body_action = c("discard", "keep", "split"),
                       knot_tail_action = c("keep", "discard", "split")) {
  checkmate::assert_class(body, "dst")
  checkmate::assert_class(tail, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  rlang::check_dots_empty()
  knot_body_action <- rlang::arg_match0(
    knot_body_action, c("discard", "keep", "split"), "knot_body_action"
  )
  knot_tail_action <- rlang::arg_match0(
    knot_tail_action, c("keep", "discard", "split"), "knot_tail_action"
  )
  p_connect <- distionary::prob_left(body, of = of, inclusive = FALSE) +
    knot_mass(body, of) - knot_retained(body, of, knot_body_action)
  if (p_connect == 0) {
    return(body)
  }
  tail_trimmed <- trim_right(tail, of = of, knot_action = knot_tail_action)
  if (p_connect == 1) {
    return(tail_trimmed)
  }
  body_trimmed <- trim_left(body, of = of, knot_action = knot_body_action)
  attach_graft_ends(
    tail_trimmed,
    body_trimmed,
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
  distionary:::new_distribution(mixture, name = "Graft", class = "graft")
}
