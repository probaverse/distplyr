# Support inference -----------------------------------------------------------
#
# Working out the support of a verb's result splits in two.
#
# The set operations --- union, restriction, transformation, atom edits ---
# depend on nothing but the supports being combined, and are
# `distionary::support_*()`. What needs the *distributions* themselves is
# here. Whether the boundary of a maximum carries mass is the example: it
# cannot be read off the supports, because it turns on how the distributions
# place their probability at that point. That is verb semantics rather than
# set theory, so it sits with the verbs.
#
# distionary's support operations are closed: one that removes everything
# returns `distionary::empty_support()`, not `NULL`. A `NULL` support means
# only "this distribution has no structured support", which in practice is
# the Null distribution.
#
# Every distribution declares a support, and every verb short-circuits on the
# Null distribution before reaching this file. So neither helper below can
# legitimately meet an input without a support, and neither has a `NULL` to
# hand back to `distribution()`, which rejects one. They fail where that
# invariant breaks instead, rather than passing on a `NULL` that would
# surface further down as a puzzling error.

#' The structured supports of a list of distributions.
#'
#' Every distribution carries a support, so a missing one means a Null
#' distribution reached a verb that should have short-circuited on it.
#' @noRd
input_supports <- function(dsts) {
  supports <- lapply(dsts, distionary::support)
  if (any(vapply(supports, is.null, logical(1L)))) {
    stop(
      "Internal error: a distribution reached the support\n",
      "inference without a support.\n",
      "Please report this at\n",
      "https://github.com/probaverse/distplyr/issues."
    )
  }
  supports
}

#' Support of `max` (or `min`) of independent distributions.
#'
#' The support is the union of the inputs' supports, restricted to
#' `[new_lo, new_hi]`, where (for a max) `new_lo` is the largest left endpoint
#' and `new_hi` the largest right endpoint. Every union-atom strictly inside the
#' interval has positive mass; only the binding endpoint (`new_lo` for a max,
#' `new_hi` for a min) can fail to be an atom of the result.
#'
#' That endpoint is an atom iff `P(M = endpoint) = prod_i F_i(endpoint)^{d_i}`
#' is positive (the strictly-below term vanishes because some distribution
#' starts exactly there). A factor is zero only for a distribution that *starts*
#' (max) / *ends* (min) exactly at the endpoint *without* an atom there. So the
#' endpoint survives iff every such boundary-touching distribution has an atom
#' there --- a structural check needing no cdf evaluation, and independent of
#' the number of draws.
#' @noRd
extreme_support <- function(dsts, supports, type = c("max", "min")) {
  type <- match.arg(type)
  ranges <- lapply(dsts, range)
  los <- vapply(ranges, function(x) x[[1L]], numeric(1L))
  his <- vapply(ranges, function(x) x[[2L]], numeric(1L))
  if (type == "max") {
    new_lo <- max(los)
    new_hi <- max(his)
    boundary <- new_lo
    touching <- which(los == boundary)
  } else {
    new_lo <- min(los)
    new_hi <- min(his)
    boundary <- new_hi
    touching <- which(his == boundary)
  }
  s <- distionary::support_restrict(
    distionary::support_union(supports),
    from = new_lo,
    to = new_hi
  )
  if (distionary::is_empty_support(s)) {
    # `new_lo` is a left endpoint of one of the inputs and `new_hi` a right
    # endpoint, with `new_lo <= new_hi`, so the restriction keeps at least
    # the binding endpoint. An empty result means the inputs disagree with
    # their own ranges.
    stop(
      "Internal error: the support of a maximum or minimum\n",
      "came out empty.\n",
      "Please report this at\n",
      "https://github.com/probaverse/distplyr/issues."
    )
  }
  keep_boundary <- all(vapply(
    touching,
    function(i) distionary::support_has_atom(supports[[i]], boundary),
    logical(1L)
  ))
  if (!keep_boundary) {
    s <- distionary::support_drop_atoms(s, boundary)
  }
  s
}
