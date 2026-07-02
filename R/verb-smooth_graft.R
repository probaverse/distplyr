#' Smoothly Graft a Tail onto a Distribution
#'
#' Replace a distribution's tail with a smooth transition into another
#' distribution, instead of the hard cut made by [graft_right()] and
#' [graft_left()]. `smooth_graft_right()` hands the body over to a right tail;
#' `smooth_graft_left()` hands it over to a left tail. The handover is governed
#' by a `weight` function and follows the hazard-mixture construction of Coia
#' and De Michele (the "smooth graft"): the body and tail are mixed on the
#' hazard (right graft) or the reverse hazard (left graft) rather than on the
#' density, so the result is continuous---no jump or kink in the density, the
#' hazard, or any risk measure read across the transition---with no normalizing
#' constant to compute.
#'
#' @param distribution The body distribution. May be continuous, discrete, or
#' mixed; in particular it may be an empirical distribution
#' ([distionary::dst_empirical()]), which a density mixture cannot use.
#' @param tail The tail distribution to graft on. Taken continuous (typically a
#' generalised Pareto distribution, [distionary::dst_gp()], shifted to its
#' location).
#' @param weight The tail weight: a vectorised function on the real line taking
#' values in `[0, 1]`, giving the share handed to the `tail`. For
#' `smooth_graft_right()` it should run from near zero on the body up to near
#' one in the right tail (non-decreasing); for `smooth_graft_left()` it should
#' run from near one in the left tail down to near zero on the body
#' (non-increasing). See Details.
#' @param ... Currently unused; must be empty.
#' @param threshold The anchor where the construction starts. Below it (for a
#' right graft; above it for a left graft) the result is exactly the body; from
#' it onward the body is handed over to the `tail`. Defaults to `-Inf`
#' (`smooth_graft_right()`) or `Inf` (`smooth_graft_left()`), the whole-range
#' mode in which the `weight` alone carries the transition over the whole body.
#' @param weight_deriv Optional derivative of `weight`, as a vectorised
#' function. Supplying it avoids a numerical derivative and is more accurate.
#' @details
#' Write \eqn{w} for `weight` and \eqn{\bar w = 1 - w}. For a right graft, with
#' body survival \eqn{\bar B}, tail survival \eqn{\bar T}, and densities \eqn{b},
#' \eqn{t}, the construction mixes the survivals and densities,
#' \deqn{M(x) = \bar w(x)\bar B(x) + w(x)\bar T(x), \qquad
#'       m(x) = \bar w(x) b(x) + w(x) t(x),}
#' and the survival and density are
#' \deqn{S(x) = M(x)\, C(x), \qquad f(x) = m(x)\, C(x),}
#' sharing the correction factor
#' \deqn{C(x) = \exp\left\{-\int_{x_0}^x
#'        \frac{w'(s)\,[\bar T(s) - \bar B(s)]}{M(s)}\, ds\right\},}
#' anchored at \eqn{x_0}. At an atom \eqn{a} of the body the graft places mass
#' \eqn{\bar w(a)\, C(a)\, \mathcal{B}(\{a\})}, where \eqn{\mathcal{B}(\{a\})}
#' is the body's mass there. The hazard \eqn{h = m / M} carries no correction
#' factor and is exact and cheap (`distionary::eval_hazard()`).
#'
#' With a finite `threshold`, \eqn{x_0} is that threshold: the body and tail are
#' conditioned to survive to it, the construction runs above it, and the body is
#' reinstated below (a left graft is the mirror image). With the default
#' infinite `threshold`, \eqn{x_0} is the end of the combined support and the
#' construction runs over the whole body.
#'
#' The left graft is the mirror image throughout, built on the CDFs \eqn{F_B},
#' \eqn{F_T} in place of the survivals.
#'
#' A constant `weight` removes the correction factor (\eqn{C \equiv 1}) and
#' recovers the static mixture; the transition is what a varying weight buys.
#'
#' Because the `tail` (a GPD) has a density jump at its own location, the graft's
#' density is smooth there only if `weight` is near zero at that location; the
#' survival, hazard, and risk measures are smooth regardless. This is a property
#' of the chosen weight, not enforced by the construction.
#' @return A smooth graft distribution object, a distribution with a continuous
#' transition between `distribution` and `tail`.
#' @examples
#' library(distionary)
#' body <- dst_norm(0, 1)
#' q <- eval_quantile(body, at = 0.9)
#' tail <- q + dst_gp(scale = 1, shape = 0.3)
#' # Logistic handover centred at q.
#' w <- function(x) stats::plogis(x, location = q, scale = 0.3)
#' g <- smooth_graft_right(body, tail, weight = w)
#' enframe_cdf(g, at = seq(-2, 5, by = 0.5))
#' @rdname smooth_graft
#' @export
smooth_graft_right <- function(distribution, tail, weight, ...,
                               threshold = -Inf, weight_deriv = NULL) {
  ellipsis::check_dots_empty()
  new_smooth_graft(
    distribution, tail, weight, weight_deriv, threshold,
    side = "right"
  )
}

#' @rdname smooth_graft
#' @export
smooth_graft_left <- function(distribution, tail, weight, ...,
                              threshold = Inf, weight_deriv = NULL) {
  ellipsis::check_dots_empty()
  new_smooth_graft(
    distribution, tail, weight, weight_deriv, threshold,
    side = "left"
  )
}

#' Construct a smooth graft distribution
#'
#' Dispatcher for [smooth_graft_right()] and [smooth_graft_left()]. An infinite
#' `threshold` is the whole-range construction ([smooth_graft_core()]); a finite
#' one conditions the body and tail to the graft side of the threshold, builds
#' the construction there, and reinstates the body on the other side.
#' @param side `"right"` or `"left"`.
#' @noRd
new_smooth_graft <- function(distribution, tail, weight, weight_deriv,
                             threshold, side) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_class(tail, "dst")
  checkmate::assert_function(weight)
  checkmate::assert_function(weight_deriv, null.ok = TRUE)
  checkmate::assert_number(threshold, na.ok = FALSE)
  body <- distribution
  w <- weight
  wp <- if (is.null(weight_deriv)) numeric_derivative(weight) else weight_deriv

  base_params <- function(core_anchor) {
    list(
      body = body, tail = tail, weight = w, weight_deriv = wp,
      threshold = threshold, anchor = core_anchor, side = side
    )
  }

  if (!is.finite(threshold)) {
    core <- smooth_graft_core(body, tail, w, wp, side)
    return(build_smooth_graft(core$reps, core$support, base_params(core$anchor)))
  }

  # Finite threshold: condition both body and tail to the graft side of the
  # threshold (the paper's B(x0) = T(x0) = 1), build there, reinstate the body.
  if (side == "right") {
    cond_body <- trim_left(body, of = threshold, include = TRUE)
    cond_tail <- trim_left(tail, of = threshold, include = TRUE)
    mass <- distionary::prob_right(body, of = threshold, inclusive = FALSE)
  } else {
    cond_body <- trim_right(body, of = threshold, include = TRUE)
    cond_tail <- trim_right(tail, of = threshold, include = TRUE)
    mass <- distionary::prob_left(body, of = threshold, inclusive = FALSE)
  }
  if (mass == 0) {
    # The graft side carries no body mass: the tail is never reached.
    return(body)
  }
  core <- smooth_graft_core(cond_body, cond_tail, w, wp, side)
  if (mass == 1) {
    # All of the body lies on the graft side; nothing to reinstate.
    return(build_smooth_graft(core$reps, core$support, base_params(threshold)))
  }
  reps <- reinstate_body(body, core$reps, threshold, mass, side)
  support_out <- reinstate_support(body, core$support, threshold, side)
  build_smooth_graft(reps, support_out, base_params(threshold))
}

#' Whole-range smooth graft: the core construction
#'
#' Builds the representation closures for the smooth graft over the whole body
#' (anchored at the end of the combined support). The right graft works on
#' survival functions; the left graft is the mirror image on CDFs. Both share
#' the correction-factor machinery. The finite-threshold mode calls this on the
#' conditioned body and tail.
#'
#' @returns A list with `reps` (the named representation closures), `support`
#' (the combined support, possibly `NULL`), and `anchor` (the reference point).
#' @noRd
smooth_graft_core <- function(body, tail, w, wp, side) {
  # Combined support and the anchor x0 where the construction starts (C = 1).
  # The right graft anchors at the lower end of the support, the left at the
  # upper end.
  supports <- input_supports(list(body, tail))
  support_out <- if (!is.null(supports)) union_support(supports) else NULL
  anchor <- if (!is.null(support_out)) {
    if (side == "right") support_min(support_out) else support_max(support_out)
  } else {
    if (side == "right") -Inf else Inf
  }
  body_atoms <- if (!is.null(support_out)) {
    discretes::get_discretes_in(
      distionary::atoms(support_out), from = -Inf, to = Inf
    )
  } else {
    numeric(0)
  }

  # The running mixtures. For "right" we mix survivals (P0 = survival); for
  # "left" we mix CDFs (P0 = cdf). `Pb`, `Pt` are those component functions,
  # `bd`, `td` the component densities.
  Pb <- if (side == "right") {
    function(x) distionary::eval_survival(body, at = x)
  } else {
    function(x) distionary::eval_cdf(body, at = x)
  }
  Pt <- if (side == "right") {
    function(x) distionary::eval_survival(tail, at = x)
  } else {
    function(x) distionary::eval_cdf(tail, at = x)
  }
  bd <- function(x) safe_density(body, x)
  td <- function(x) distionary::eval_density(tail, at = x)

  running_mix <- function(x) (1 - w(x)) * Pb(x) + w(x) * Pt(x) # M
  running_dens <- function(x) (1 - w(x)) * bd(x) + w(x) * td(x) # m

  # Correction-factor integrand g(s) = w'(s) [Pt(s) - Pb(s)] / M(s).
  integrand <- function(s) {
    M <- running_mix(s)
    out <- wp(s) * (Pt(s) - Pb(s)) / M
    out[!is.finite(out)] <- 0
    out
  }
  # J(x) = integral of g from the anchor to x, so that the correction factor is
  # C(x) = exp(-J(x)). Each construction quantity is `value * C`; we form it as
  # exp(log(value) - J) so that an underflowing `value` and an overflowing `C`
  # (which together stay finite) never collide as 0 * Inf in the far tail.
  jfun <- make_jfun(integrand, anchor, body_atoms, side)
  scale_by_corr <- function(value, j) {
    out <- numeric(length(value))
    pos <- is.finite(value) & value > 0
    out[pos] <- exp(log(value[pos]) - j[pos])
    out
  }

  primary <- function(x) { # survival (right) or cdf (left)
    p <- scale_by_corr(running_mix(x), jfun(x))
    pmin(pmax(p, 0), 1)
  }

  reps <- list(
    density = function(x) scale_by_corr(running_dens(x), jfun(x)),
    # Mass at a body atom a: wbar(a) C(a) B({a}).
    pmf = function(x) scale_by_corr((1 - w(x)) * safe_pmf(body, x), jfun(x))
  )
  if (side == "right") {
    reps$survival <- primary
    reps$cdf <- function(x) 1 - primary(x)
    # h = m / M carries no correction factor (it cancels), so the hazard is
    # exact and cheap -- but only meaningful when the body is continuous (with
    # atoms the distribution is mixed and has no ordinary hazard). It holds for
    # the right graft, where M mixes survivals; for the left graft m / M is the
    # reverse hazard, so the true hazard is left to the density and survival.
    if (distionary::vtype(body) == "continuous") {
      reps$hazard <- function(x) pmax(running_dens(x) / running_mix(x), 0)
    }
  } else {
    reps$cdf <- primary
    reps$survival <- function(x) 1 - primary(x)
  }
  list(reps = reps, support = support_out, anchor = anchor)
}

#' Reinstate the body on the non-graft side of a finite threshold
#'
#' Wraps the conditional construction's representations (`core_reps`, supported
#' on the graft side of `threshold`) so that the result equals the body on the
#' other side and the conditional construction, scaled by the body `mass` on the
#' graft side, on the graft side. The threshold atom (if any) stays with the
#' body.
#' @noRd
reinstate_body <- function(body, core_reps, threshold, mass, side) {
  # The blend anchors on the *primary* representation -- survival for a right
  # graft, CDF for a left graft -- because the primary is the body's own on the
  # non-graft side and goes to zero away from it, so the two pieces join with no
  # leftover mass. (Blending the survival of a left graft would drop the body
  # mass sitting above the threshold.) `in_body` marks the body side; the
  # threshold atom is kept with the body, which puts the boundary on the body
  # side for the CDF and the pmf and on the graft side for the survival, exactly
  # matching right-continuity.
  blend <- function(body_fun, core_fun, in_body, core_scale = mass) {
    function(x) {
      out <- numeric(length(x))
      b <- in_body(x)
      if (any(b)) out[b] <- body_fun(x[b])
      if (any(!b)) out[!b] <- core_scale * core_fun(x[!b])
      out
    }
  }
  if (side == "right") {
    in_body_primary <- function(x) x < threshold
    in_body_mass <- function(x) x <= threshold
    body_primary <- function(x) distionary::eval_survival(body, at = x)
    core_primary <- core_reps$survival
  } else {
    in_body_primary <- function(x) x >= threshold
    in_body_mass <- function(x) x >= threshold
    body_primary <- function(x) distionary::eval_cdf(body, at = x)
    core_primary <- core_reps$cdf
  }
  primary <- blend(body_primary, core_primary, in_body_primary)
  reps <- list(
    density = blend(
      function(x) safe_density(body, x), core_reps$density, in_body_primary
    ),
    pmf = blend(
      function(x) safe_pmf(body, x), core_reps$pmf, in_body_mass
    )
  )
  if (side == "right") {
    reps$survival <- primary
    reps$cdf <- function(x) 1 - primary(x)
  } else {
    reps$cdf <- primary
    reps$survival <- function(x) 1 - primary(x)
  }
  # The hazard is not mass-scaled (the scale cancels in f / S); carry it through
  # only when the core supplies one (continuous body, right graft).
  if (!is.null(core_reps$hazard)) {
    reps$hazard <- blend(
      function(x) distionary::eval_hazard(body, at = x),
      core_reps$hazard, in_body_primary, core_scale = 1
    )
  }
  reps
}

#' Combined support after reinstating the body across a finite threshold
#' @noRd
reinstate_support <- function(body, core_support, threshold, side) {
  body_support <- distionary::support(body)
  if (is.null(body_support) || is.null(core_support)) {
    return(NULL)
  }
  body_side <- if (side == "right") {
    restrict_support(body_support, to = threshold, include_to = TRUE)
  } else {
    restrict_support(body_support, from = threshold, include_from = TRUE)
  }
  if (is.null(body_side)) {
    return(core_support)
  }
  union_support(list(body_side, core_support))
}

#' Assemble a smooth graft distribution object from its representations
#' @noRd
build_smooth_graft <- function(reps, support, params) {
  d <- distionary::distribution(
    cdf = reps$cdf,
    survival = reps$survival,
    density = reps$density,
    pmf = reps$pmf,
    hazard = reps$hazard,
    .support = support,
    .name = "Smooth Graft",
    .parameters = params
  )
  distionary:::new_distribution(d, class = "smooth_graft")
}

#' @export
print.smooth_graft <- function(x, ...) {
  params <- distionary::parameters(x)
  side <- params[["side"]]
  arrow <- if (side == "right") "right tail" else "left tail"
  cat("Smooth Graft Distribution\n")
  cat("\nBody: ", distionary::pretty_name(params[["body"]]), "\n", sep = "")
  cat("Tail (", arrow, "): ",
    distionary::pretty_name(params[["tail"]]), "\n",
    sep = ""
  )
  threshold <- params[["threshold"]]
  if (is.finite(threshold)) {
    cat("Threshold: ", format(threshold), "\n", sep = "")
  } else {
    cat("Threshold: none (whole-range weight)\n")
  }
  invisible(x)
}

#' Numerical derivative of a vectorised scalar function
#'
#' Central difference with a step that adapts to the magnitude of `x`, used when
#' the user does not supply the weight's derivative.
#' @noRd
numeric_derivative <- function(f) {
  function(x) {
    h <- 1e-5 * pmax(1, abs(x))
    (f(x + h) - f(x - h)) / (2 * h)
  }
}

#' Continuous-part density, zero where the distribution has none
#'
#' A purely discrete distribution (e.g. an empirical body) has no density, and
#' `eval_density()` errors on it; its continuous-part density is zero.
#' @noRd
safe_density <- function(distribution, x) {
  if (distionary::vtype(distribution) == "discrete") {
    return(numeric(length(x)))
  }
  out <- tryCatch(
    distionary::eval_density(distribution, at = x),
    error = function(e) numeric(length(x))
  )
  out
}

#' Probability mass function, zero where the distribution has no atoms
#'
#' A purely continuous distribution has no atoms; `eval_pmf()` may error on it.
#' @noRd
safe_pmf <- function(distribution, x) {
  if (distionary::vtype(distribution) == "continuous") {
    return(numeric(length(x)))
  }
  tryCatch(
    distionary::eval_pmf(distribution, at = x),
    error = function(e) numeric(length(x))
  )
}

#' Build a cached evaluator for the correction-factor integral
#'
#' Returns a function `jfun(x)` giving \eqn{J(x) = \int_{x_0}^x g(s)\, ds}, where
#' `x0` is the `anchor` and `g` the integrand. \eqn{J} is what the correction
#' factor exponentiates: \eqn{C = \exp(-J)}.
#'
#' The integrand steps at each atom of a discrete or mixed body but is smooth
#' between atoms, so the atoms are integration breakpoints. To keep repeated
#' evaluation cheap---quantile inversion calls the CDF, hence \eqn{J}, hundreds
#' of times---the cumulative integral up to each atom is computed once, lazily,
#' and cached. A query for arbitrary `x` then costs a single integration from
#' the nearest cached atom (no atom lies strictly between them) out to `x`. With
#' a continuous body (no atoms) each query is a single integration from the
#' anchor.
#'
#' @param g The integrand, a vectorised function.
#' @param anchor The reference point \eqn{x_0} (possibly infinite).
#' @param atoms Numeric vector of body atom locations (breakpoints).
#' @param side `"right"` (anchor is the lower limit) or `"left"` (upper limit).
#' @returns A function of a numeric vector `x` returning \eqn{J(x)}.
#' @noRd
make_jfun <- function(g, anchor, atoms, side) {
  atoms <- sort(unique(atoms))
  n <- length(atoms)
  cache <- new.env(parent = emptyenv())
  cache$built <- FALSE
  # J at each atom, accumulated outward from the anchor (the segment between two
  # consecutive breakpoints holds no atom, so each is a smooth integration).
  build <- function() {
    j <- numeric(n)
    if (side == "right") {
      prev <- anchor
      acc <- 0
      for (i in seq_len(n)) {
        acc <- acc + segment_integral(g, prev, atoms[i])
        j[i] <- acc
        prev <- atoms[i]
      }
    } else {
      prev <- anchor
      acc <- 0
      for (i in rev(seq_len(n))) {
        acc <- acc - segment_integral(g, atoms[i], prev)
        j[i] <- acc
        prev <- atoms[i]
      }
    }
    cache$j_atom <- j
    cache$built <- TRUE
  }
  function(x) {
    out <- numeric(length(x))
    out[is.na(x)] <- NA_real_
    idx <- which(!is.na(x))
    if (length(idx) == 0L) {
      return(out)
    }
    if (n == 0L) {
      # Continuous body: accumulate over the sorted query points in one sweep
      # away from the anchor, so each point adds a small smooth segment to a
      # running total rather than re-integrating from the anchor.
      ord <- idx[order(x[idx], decreasing = (side == "left"))]
      prev <- anchor
      acc <- 0
      for (i in ord) {
        xi <- x[i]
        if (!isTRUE(xi == prev)) {
          seg <- segment_integral(g, min(prev, xi), max(prev, xi))
          acc <- if (side == "right") acc + seg else acc - seg
          prev <- xi
        }
        out[i] <- acc
      }
      return(out)
    }
    if (!cache$built) {
      build()
    }
    for (i in idx) {
      xi <- x[i]
      if (side == "right") {
        k <- sum(atoms <= xi) # largest atom not exceeding x
        ref <- if (k > 0L) atoms[k] else anchor
        base <- if (k > 0L) cache$j_atom[k] else 0
        out[i] <- base + segment_integral(g, ref, xi)
      } else {
        above <- which(atoms >= xi) # smallest atom not below x
        if (length(above) > 0L) {
          k <- above[1L]
          ref <- atoms[k]
          base <- cache$j_atom[k]
        } else {
          ref <- anchor
          base <- 0
        }
        out[i] <- base - segment_integral(g, xi, ref)
      }
    }
    out
  }
}

#' Integrate the correction integrand over one segment, returning 0 on failure.
#' @noRd
segment_integral <- function(g, lower, upper) {
  if (isTRUE(lower == upper)) {
    return(0)
  }
  val <- tryCatch(
    stats::integrate(g, lower = lower, upper = upper, rel.tol = 1e-8)$value,
    error = function(e) NA_real_
  )
  if (is.na(val)) 0 else val
}
