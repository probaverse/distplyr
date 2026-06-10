# Support algebra ------------------------------------------------------------
#
# Helpers for propagating a distribution's structured support (from distionary)
# through distplyr's verbs. A support decomposes into an atomic part (a
# `discretes` object) and a continuous part (a union of closed intervals, stored
# as a two-column matrix). These helpers transform, union, and restrict that
# decomposition, building on distionary's exported support API
# (`continuous()`, `discrete()`, `mixed()`, `atoms()`, `continuous_part()`).
#
# The guiding principle in the verbs: only set `.support` when it can be
# computed exactly from the inputs' supports. If any input lacks a structured
# support (`support()` is `NULL`), fall back to the previous `range`/`.vtype`
# behaviour.

#' Assemble a support from an atomic part and a continuous interval matrix.
#'
#' Picks the right distionary constructor based on which parts are non-empty.
#' Returns `NULL` if both parts are empty (no support).
#' @noRd
build_support <- function(atoms, intervals) {
  has_atoms <- discretes::num_discretes(atoms) > 0
  has_cont <- nrow(intervals) > 0
  if (has_atoms && has_cont) {
    distionary::mixed(atoms = atoms, continuous = intervals)
  } else if (has_atoms) {
    distionary::discrete(atoms)
  } else if (has_cont) {
    distionary::continuous(intervals)
  } else {
    NULL
  }
}

#' An empty continuous-interval matrix.
#' @noRd
no_intervals <- function() {
  m <- matrix(numeric(0), ncol = 2L)
  colnames(m) <- c("lower", "upper")
  m
}

#' Transform a support under a strictly monotonic map.
#'
#' Applies `fun` (with inverse `inv`) to the atoms (via
#' `discretes::dsct_transform()`) and to the endpoints of each continuous
#' interval. For a decreasing map the interval endpoints are swapped so that
#' `lower <= upper`.
#'
#' @param support A support object.
#' @param fun,inv The map and its inverse (both vectorised).
#' @param increasing Whether `fun` is increasing (`TRUE`) or decreasing.
#' @param domain,range Domain and range of `fun`, passed to `dsct_transform()`.
#' @returns A support object.
#' @noRd
transform_support <- function(
  support,
  fun,
  inv,
  increasing = TRUE,
  domain = c(-Inf, Inf),
  range = c(-Inf, Inf)
) {
  atoms <- distionary::atoms(support)
  intervals <- distionary::continuous_part(support)
  dir <- if (increasing) "increasing" else "decreasing"
  if (discretes::num_discretes(atoms) > 0) {
    atoms <- discretes::dsct_transform(
      atoms,
      fun = fun,
      inv = inv,
      domain = domain,
      range = range,
      dir = dir
    )
  }
  if (nrow(intervals) > 0) {
    lo <- fun(intervals[, "lower"])
    hi <- fun(intervals[, "upper"])
    intervals <- if (increasing) cbind(lo, hi) else cbind(hi, lo)
  }
  build_support(atoms, intervals)
}

#' Union of a list of supports (for mixtures).
#'
#' Unions the atomic parts (`discretes::dsct_union()`) and the continuous parts
#' (stacked and merged into canonical form by `distionary::continuous()`).
#' @noRd
union_support <- function(supports) {
  atom_list <- lapply(supports, distionary::atoms)
  atoms <- Reduce(discretes::dsct_union, atom_list)
  interval_list <- lapply(supports, distionary::continuous_part)
  intervals <- do.call(rbind, interval_list)
  if (is.null(intervals) || nrow(intervals) == 0) {
    intervals <- no_intervals()
  } else {
    # Canonicalise (sort + merge) by round-tripping through continuous().
    intervals <- distionary::continuous_part(distionary::continuous(intervals))
  }
  build_support(atoms, intervals)
}

#' Restrict a support to an interval `[from, to]` (for truncation).
#'
#' Keeps the atoms within the interval (`discretes::dsct_keep()`, honouring the
#' open/closed `include_*` flags) and clips each continuous interval to
#' `[from, to]`.
#' @noRd
restrict_support <- function(
  support,
  from = -Inf,
  to = Inf,
  include_from = TRUE,
  include_to = TRUE
) {
  atoms <- distionary::atoms(support)
  intervals <- distionary::continuous_part(support)
  if (discretes::num_discretes(atoms) > 0) {
    atoms <- discretes::dsct_keep(
      atoms,
      from = from,
      to = to,
      include_from = include_from,
      include_to = include_to
    )
  }
  if (nrow(intervals) > 0) {
    lo <- pmax(intervals[, "lower"], from)
    hi <- pmin(intervals[, "upper"], to)
    keep <- lo <= hi
    intervals <- cbind(lower = lo[keep], upper = hi[keep])
    if (nrow(intervals) == 0) {
      intervals <- no_intervals()
    }
  }
  build_support(atoms, intervals)
}

#' Drop a single atom (a boundary point) from a support.
#' @noRd
drop_atom <- function(support, value) {
  atoms <- distionary::atoms(support)
  intervals <- distionary::continuous_part(support)
  if (discretes::num_discretes(atoms) > 0) {
    atoms <- discretes::dsct_drop(atoms, from = value, to = value)
  }
  build_support(atoms, intervals)
}

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

#' Is `value` an atom of `support`?
#' @noRd
atom_present <- function(support, value) {
  atoms <- distionary::atoms(support)
  if (discretes::num_discretes(atoms) == 0) {
    return(FALSE)
  }
  isTRUE(discretes::has_discretes(atoms, value))
}

#' Does `support` place mass at `value` --- is `value` an atom or inside a
#' continuous interval (as opposed to a gap of the support)?
#' @noRd
support_contains <- function(support, value) {
  intervals <- distionary::continuous_part(support)
  in_interval <- nrow(intervals) > 0 &&
    any(value >= intervals[, "lower"] & value <= intervals[, "upper"])
  in_interval || atom_present(support, value)
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
#' there --- a structural check needing no cdf evaluation, and independent of the
#' number of draws.
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
  s <- restrict_support(union_support(supports), from = new_lo, to = new_hi)
  if (is.null(s)) {
    return(NULL)
  }
  keep_boundary <- all(vapply(
    touching,
    function(i) atom_present(supports[[i]], boundary),
    logical(1L)
  ))
  if (!keep_boundary) {
    s <- drop_atom(s, boundary)
  }
  s
}
