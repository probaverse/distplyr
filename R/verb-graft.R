#' Graft a tail onto a distribution
#'
#' Replace one end of a distribution with a different model of that end.
#' `graft_right()` keeps `body` below `of` --- the knot --- and hands
#' everything above it to the tail; `graft_left()` does the same at the
#' lower end. The tail's share of the total is the body's own probability of
#' reaching past the knot.
#'
#' @details
#' # Two ways to hand over the tail
#'
#' Name exactly one of `tail_excess` and `tail_absolute`. Neither has a
#' default, because nothing in a distribution says which scale it is on.
#'
#' - `tail_excess` is measured from the knot: the distribution of `X - of`,
#'   whose zero is the knot. It is **moved from zero to the knot**, by
#'   adding `of` to it, so a generalised Pareto living on `[0, Inf)` becomes
#'   a tail living on `[of, Inf)`. All of its probability must lie on one
#'   side of zero: at or above for `graft_right()`, at or below for
#'   `graft_left()`.
#' - `tail_absolute` is on the body's scale already and stays where it is,
#'   conditioned on falling beyond the knot. Anything placed by hand goes
#'   here --- `multiply(ratio, of)`, for a model of `X / of`, say.
#'
#' A tail that is already in place but happens to sit above zero cannot be
#' told apart from a model of excesses, so `tail_excess` accepts it. If it
#' starts exactly at `of`, the likeliest case, you get a warning.
#'
#' # Where the knot goes
#'
#' `knot` names the side that probability sitting exactly on the knot
#' belongs to: `"body"` (the default), `"tail"`, or `"split"` for half each,
#' the mid-p convention. Naming one side names the other, so the knot is
#' counted once; what a side does not take passes into the other's share.
#' None of this has any effect unless there is mass exactly at `of`, as
#' there never is in a continuous distribution.
#'
#' The default leaves the body alone up to and including the knot, and gives
#' the tail `prob_right(body, of, inclusive = FALSE)`, the probability of
#' exceeding it. That is the convention peaks-over-threshold is written in,
#' where the excess `X - of` is conditioned on `X > of` strictly and an
#' excess of exactly zero does not arise.
#'
#' @param body Distribution supplying the part of the range that is kept.
#' @param of Value on the real line where the tail is attached: the knot.
#' @param ... Currently unused; must be empty.
#' @param tail_excess Distribution of the tail measured from the knot, moved
#' from zero to `of`. Name either this or `tail_absolute`, not both.
#' @param tail_absolute Distribution of the tail on the body's own scale,
#' left where it is and conditioned beyond the knot. Name either this or
#' `tail_excess`, not both.
#' @param knot Which side probability sitting exactly on the knot belongs
#' to: the `"body"` (the default), the `"tail"`, or `"split"` between them.
#' @return A graft: the body on one side of the knot and the tail on the
#' other, which is a special type of mixture distribution.
#' @seealso [trim_left()] and [trim_right()], which discard an end rather
#' than replacing it.
#' @examples
#' body <- distionary::dst_norm(0, 1)
#' u <- distionary::eval_quantile(body, at = 0.9)
#'
#' # Excesses over `u`, living on [0, Inf): moved to start at `u`.
#' graft_right(body, of = u, tail_excess = distionary::dst_gp(1, 0.3))
#'
#' # The same graft, placed by hand instead.
#' moved <- shift(distionary::dst_gp(1, 0.3), u)
#' graft_right(body, of = u, tail_absolute = moved)
#'
#' # A model on the body's scale, conditioned above `u`.
#' graft_right(body, of = u, tail_absolute = distionary::dst_norm(1, 3))
#' @rdname graft
#' @export
graft_right <- function(
  body,
  of,
  ...,
  tail_excess,
  tail_absolute,
  knot = c("body", "tail", "split")
) {
  checkmate::assert_class(body, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  rlang::check_dots_empty()
  knot <- rlang::arg_match0(knot, c("body", "tail", "split"), "knot")
  tail <- trim_left(
    graft_tail(
      excess = if (missing(tail_excess)) NULL else tail_excess,
      absolute = if (missing(tail_absolute)) NULL else tail_absolute,
      of = of,
      side = "right"
    ),
    of = of,
    knot = knot_trim(knot, "tail")
  )
  # Whatever of the knot the body does not retain belongs to the tail's
  # share, which is what keeps the two weights summing to 1 wherever the
  # knot is sent.
  p_connect <- distionary::prob_right(body, of = of, inclusive = FALSE) +
    knot_mass(body, of) -
    knot_retained(body, of, knot_trim(knot, "body"))
  if (p_connect == 0) {
    return(body)
  }
  if (p_connect == 1) {
    return(tail)
  }
  body_trimmed <- trim_right(body, of = of, knot = knot_trim(knot, "body"))
  attach_graft_ends(
    body_trimmed,
    tail,
    p_left = 1 - p_connect,
    p_right = p_connect
  )
}

#' @rdname graft
#' @export
graft_left <- function(
  body,
  of,
  ...,
  tail_excess,
  tail_absolute,
  knot = c("body", "tail", "split")
) {
  checkmate::assert_class(body, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  rlang::check_dots_empty()
  knot <- rlang::arg_match0(knot, c("body", "tail", "split"), "knot")
  tail <- trim_right(
    graft_tail(
      excess = if (missing(tail_excess)) NULL else tail_excess,
      absolute = if (missing(tail_absolute)) NULL else tail_absolute,
      of = of,
      side = "left"
    ),
    of = of,
    knot = knot_trim(knot, "tail")
  )
  p_connect <- distionary::prob_left(body, of = of, inclusive = FALSE) +
    knot_mass(body, of) -
    knot_retained(body, of, knot_trim(knot, "body"))
  if (p_connect == 0) {
    return(body)
  }
  if (p_connect == 1) {
    return(tail)
  }
  body_trimmed <- trim_left(body, of = of, knot = knot_trim(knot, "body"))
  attach_graft_ends(
    tail,
    body_trimmed,
    p_left = p_connect,
    p_right = 1 - p_connect
  )
}

#' Put the tail on the body's scale.
#'
#' One of `excess` and `absolute` carries a distribution and the other is
#' `NULL`, standing for the argument the caller left out. An excess is moved
#' from zero to the knot; a tail already on the body's scale is left alone.
#' The verb trims whichever comes back.
#' @noRd
graft_tail <- function(excess, absolute, of, side) {
  if (!xor(is.null(excess), is.null(absolute))) {
    stop(
      "Name one of `tail_excess` or `tail_absolute`.\n",
      "`tail_excess` is measured from the knot, and is moved there.\n",
      "`tail_absolute` is on the body's scale, and is conditioned there.",
      call. = FALSE
    )
  }
  if (is.null(excess)) {
    checkmate::assert_class(absolute, "dst")
    return(absolute)
  }
  checkmate::assert_class(excess, "dst")
  check_excess_side(excess, of = of, side = side)
  shift(excess, of)
}

#' The trim one side of the graft receives, given where the knot goes.
#'
#' The side `knot` names keeps it, the other gives it up, and `"split"`
#' halves it on both. So the knot cannot be counted twice or lost.
#' @noRd
knot_trim <- function(knot, side) {
  if (knot == "split") {
    return("split")
  }
  if (knot == side) "keep" else "discard"
}

#' Refuse an excess model sitting on the wrong side of zero.
#'
#' An excess is a distance from the knot: non-negative for `graft_right()`,
#' non-positive for `graft_left()`. One starting exactly at `of` is likely a
#' fit whose location is the threshold rather than zero, which moving would
#' start at twice `of` --- worth a word, though it could be coincidence.
#' @noRd
check_excess_side <- function(excess, of, side) {
  ends <- range(excess)
  near <- if (side == "right") ends[[1L]] else ends[[2L]]
  if (is.na(near)) {
    return(invisible(excess))
  }
  wrong_side <- if (side == "right") near < 0 else near > 0
  if (wrong_side) {
    beyond <- if (side == "right") "below" else "above"
    stop(
      "`tail_excess` places probability ",
      beyond,
      " zero.\n",
      "It is measured from the knot, so it cannot reach past it.\n",
      "A tail on the body's own scale goes in `tail_absolute`.",
      call. = FALSE
    )
  }
  if (of != 0 && near == of) {
    warning(
      "`tail_excess` already starts at `of`, and is moved to it\n",
      "again, starting the tail at twice `of`. A tail already in\n",
      "place goes in `tail_absolute`.",
      call. = FALSE
    )
  }
  invisible(excess)
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
