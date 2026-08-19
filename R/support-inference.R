# Support inference -----------------------------------------------------------
#
# What remains here after the support algebra moved to distionary: the parts
# that need the *distributions*, not just their supports. Whether the boundary
# of a maximum carries mass depends on the distributions themselves, so it is
# verb semantics rather than set theory, and belongs with the verbs.
#
# The algebra itself --- union, restriction, transformation, atom edits --- is
# `distionary::support_*()`. Note that those operations are closed: one that
# removes everything returns `distionary::empty_support()`, not `NULL`. `NULL`
# here means only "this distribution has no structured support".
#
# The guiding principle in the verbs is unchanged: only set `.support` when it
# can be computed exactly from the inputs' supports; otherwise leave it `NULL`
# and let the distribution fall back.

#' The structured supports of a list of distributions, or `NULL` if any is
#' missing one.
#' @noRd
input_supports <- function(dsts) {
  supports <- lapply(dsts, distionary::support)
  if (any(vapply(supports, is.null, logical(1L)))) {
    return(NULL)
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
    return(NULL)
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
