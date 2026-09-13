#' Graft a tail onto a distribution
#'
#' Replace one end of a distribution with a different model of that end.
#' `graft_right()` keeps `body` below `of` and hands everything above it to
#' the tail; `graft_left()` does the same at the lower end. The result is a
#' *graft*: both pieces are weighted so that the body's own probability of
#' reaching past `of` is the share the tail receives.
#'
#' @details
#' # The scale the tail is on
#'
#' A tail model arrives on one of two scales, and nothing in the
#' distribution itself says which, so exactly one of two arguments has to be
#' named. Neither has a default: a graft has enough going on that the scale
#' of the tail is worth saying out loud at every call site.
#'
#' - `tail_excess` is measured from the knot. A generalised Pareto fitted to
#'   the excesses over a threshold is the usual case: its zero *is* the
#'   knot, and the tail is moved there. It may place no probability on the
#'   far side of zero --- none below it for `graft_right()`, none above it
#'   for `graft_left()`.
#' - `tail_absolute` is on the same scale as the body and stays where it is.
#'   It is conditioned on falling beyond the knot.
#'
#' A tail the caller has already placed goes in `tail_absolute`, whatever
#' put it there. A multiplicative model of the tail, say of `X / of`, is
#' placed by `multiply(ratio, of)`, which lands it at the knot; conditioning
#' it there then does nothing.
#'
#' `tail_excess` cannot catch every instance of the opposite mistake, a tail
#' already on the body's scale that places no probability below zero --- a
#' lognormal, or a generalised Pareto whose location is the threshold rather
#' than zero. The second of those starts exactly at `of`, and is warned
#' about; the rest cannot be told apart from a model of excesses that
#' happens to start above zero, as an empirical one does.
#'
#' # Where the knot goes
#'
#' The knot is one point, and it belongs to one side. `knot_action` says
#' what the *body* does with its probability sitting exactly there ---
#' `"keep"` it (the default), `"discard"` it, or `"split"` it in half ---
#' and the tail takes the opposite action, so that the knot is neither
#' counted twice nor lost:
#'
#' | `knot_action` | the body | the tail |
#' | --- | --- | --- |
#' | `"keep"` | keeps the knot | gives it up |
#' | `"discard"` | gives it up | keeps the knot |
#' | `"split"` | keeps half | keeps half |
#'
#' Splitting is its own opposite: both sides take half, the mid-p convention
#' on each. Any of this matters only where there is mass exactly at `of`,
#' which is nowhere in a continuous distribution.
#'
#' Under the default the graft leaves the body alone up to and including the
#' knot --- every probability there is the body's own, unscaled --- and the
#' tail's share is exactly `prob_right(body, of, inclusive = FALSE)`, the
#' probability of exceeding the knot. That is the exceedance probability
#' peaks-over-threshold is written in terms of, where the excess `X - of` is
#' conditioned on `X > of` strictly; the tail being trimmed of the knot is
#' the same convention, under which an excess of exactly zero does not
#' arise. The two pieces are a partition: `(-Inf, of]` and `(of, Inf)`.
#'
#' To hand the knot to the tail instead, ask the body to `"discard"` it.
#' A `tail_excess` with an atom at zero then keeps that atom, an excess of
#' zero being the event `X = of`.
#'
#' @param body Distribution supplying the part of the range that is kept.
#' @param of Value on the real line where the tail is attached: the knot.
#' @param ... Currently unused; must be empty.
#' @param tail_excess Distribution of the tail measured from the knot, which
#' is moved there. Name either this or `tail_absolute`, not both.
#' @param tail_absolute Distribution of the tail on the body's own scale,
#' which stays where it is and is conditioned on falling beyond the knot.
#' Name either this or `tail_excess`, not both.
#' @param knot_action What the body does with its own probability at the
#' knot: `"keep"` it (the default), `"discard"` it, or `"split"` it. The
#' tail takes the opposite action, so the knot is counted once. See Details.
#' @return A graft: a distribution made of the body below the knot and the
#' tail above it (or the other way round, for `graft_left()`), which is a
#' special type of mixture distribution.
#' @seealso [trim_left()] and [trim_right()], which discard an end rather
#' than replacing it.
#' @examples
#' body <- distionary::dst_norm(0, 1)
#' u <- distionary::eval_quantile(body, at = 0.9)
#'
#' # A model of the excesses over `u`, moved to the knot.
#' excess <- distionary::dst_gp(1, 0.3)
#' graft_right(body, of = u, tail_excess = excess)
#'
#' # A model on the body's own scale, conditioned above `u`.
#' graft_right(body, of = u, tail_absolute = distionary::dst_norm(1, 3))
#' @rdname graft
#' @export
graft_right <- function(body, of, ..., tail_excess, tail_absolute,
                        knot_action = c("keep", "discard", "split")) {
  checkmate::assert_class(body, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  rlang::check_dots_empty()
  knot_action <- rlang::arg_match0(
    knot_action, c("keep", "discard", "split"), "knot_action"
  )
  tail <- trim_left(
    graft_tail(
      excess = if (missing(tail_excess)) NULL else tail_excess,
      absolute = if (missing(tail_absolute)) NULL else tail_absolute,
      of = of,
      side = "right"
    ),
    of = of,
    knot_action = knot_opposite(knot_action)
  )
  # Whatever of the knot the body does not retain belongs to the tail's
  # share, which is what keeps the two weights summing to 1 however
  # `knot_action` is set.
  p_connect <- distionary::prob_right(body, of = of, inclusive = FALSE) +
    knot_mass(body, of) - knot_retained(body, of, knot_action)
  if (p_connect == 0) {
    return(body)
  }
  if (p_connect == 1) {
    return(tail)
  }
  body_trimmed <- trim_right(body, of = of, knot_action = knot_action)
  attach_graft_ends(
    body_trimmed,
    tail,
    p_left = 1 - p_connect,
    p_right = p_connect
  )
}

#' @rdname graft
#' @export
graft_left <- function(body, of, ..., tail_excess, tail_absolute,
                       knot_action = c("keep", "discard", "split")) {
  checkmate::assert_class(body, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  rlang::check_dots_empty()
  knot_action <- rlang::arg_match0(
    knot_action, c("keep", "discard", "split"), "knot_action"
  )
  tail <- trim_right(
    graft_tail(
      excess = if (missing(tail_excess)) NULL else tail_excess,
      absolute = if (missing(tail_absolute)) NULL else tail_absolute,
      of = of,
      side = "left"
    ),
    of = of,
    knot_action = knot_opposite(knot_action)
  )
  p_connect <- distionary::prob_left(body, of = of, inclusive = FALSE) +
    knot_mass(body, of) - knot_retained(body, of, knot_action)
  if (p_connect == 0) {
    return(body)
  }
  if (p_connect == 1) {
    return(tail)
  }
  body_trimmed <- trim_left(body, of = of, knot_action = knot_action)
  attach_graft_ends(
    tail,
    body_trimmed,
    p_left = p_connect,
    p_right = 1 - p_connect
  )
}

#' Put the tail on the body's scale, whichever way it was handed over.
#'
#' Exactly one of `excess` and `absolute` carries a distribution; the other
#' is `NULL`, standing for the argument the caller left out. An excess model
#' is moved so that its zero lands on the knot; one already on the body's
#' scale is left where it is. Either way the caller gets a tail on the
#' body's scale, which the verb then trims at the knot.
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

#' The knot action the tail takes, given the body's.
#'
#' The knot is one point and belongs to one side of the graft, so the two
#' sides take opposite actions: what the body keeps the tail gives up, and
#' what the body gives up the tail keeps. Splitting is its own opposite ---
#' both sides take half, the mid-p convention on each.
#' @noRd
knot_opposite <- function(knot_action) {
  switch(knot_action, keep = "discard", discard = "keep", split = "split")
}

#' Refuse an excess model sitting on the wrong side of zero.
#'
#' An excess is a distance from the knot, so for `graft_right()` it is
#' non-negative and for `graft_left()` non-positive. A model that starts
#' exactly at `of` is the fingerprint of a fit whose location is the
#' threshold rather than zero: moving it to the knot would start the tail at
#' twice `of`, which is worth a word even though it could be a coincidence.
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
      "`tail_excess` places probability ", beyond, " zero.\n",
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
