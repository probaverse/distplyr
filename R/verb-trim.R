#' Probability the distribution places exactly at the knot.
#'
#' Zero wherever there is no atom there, including everywhere in a
#' continuous distribution.
#' @noRd
knot_mass <- function(distribution, of) {
  m <- distionary::eval_pmf(distribution, at = of)
  if (length(m) != 1L || is.na(m)) 0 else m
}

#' The share of the knot's mass a trim retains.
#'
#' The three actions differ only in this number, which is why they share a
#' code path: "keep" retains all of it, "discard" none, "split" half.
#' @noRd
knot_retained <- function(distribution, of, knot_action) {
  m <- knot_mass(distribution, of)
  switch(knot_action, keep = m, discard = 0, split = m / 2)
}

#' Trim (condition) a distribution
#'
#' Discard the probability lying to one side of a value, and scale up what
#' remains so that it sums to 1 again. `trim_left()` discards the
#' probability below `of`, giving the distribution of the variable
#' conditioned on landing above it; `trim_right()` discards the probability
#' above `of`, conditioning on landing below.
#'
#' @details
#' # What `knot_action` does
#'
#' `of` is the knot: the point the trim cuts at. `knot_action` says what
#' becomes of the probability sitting exactly on it, and matters only when
#' the knot carries mass of its own, as an atom does. Where there is no
#' mass exactly at `of` --- anywhere in a continuous distribution --- all
#' three actions give the same answer.
#'
#' - `"discard"` (default) throws the knot away with the side being
#'   trimmed, so `trim_left(d, of)` keeps outcomes strictly greater than
#'   `of`.
#' - `"keep"` retains it, so `trim_left(d, of)` keeps outcomes greater
#'   than *or equal to* `of`.
#' - `"split"` retains half of it. This is the mid-p convention used in
#'   discrete inference, where a boundary atom is shared evenly between
#'   the two sides rather than assigned wholly to one.
#'
#' Whatever is retained is renormalised along with the rest, so the result
#' is always a distribution in its own right.
#'
#' # Values of `of` with nothing beside them
#'
#' If `of` falls in a gap in the support, the trim takes effect where the
#' support resumes. Trimming a distribution living on `[1, 2]` and `[4, 5]`
#' to the left of 3 gives one living on `[4, 5]`.
#'
#' If the trim leaves no probability at all --- trimming a Uniform(0, 1) to
#' the left of 2, say --- the result is the Null distribution,
#' `distionary::dst_null()`.
#'
#' @param distribution Distribution to trim.
#' @param of Value on the real line defining where to trim (single numeric).
#' @param knot_action What to do with the probability sitting exactly on
#' `of`: `"discard"` it with the trimmed side (the default), `"keep"` it,
#' or `"split"` it evenly between the two sides. Only has an effect where
#' `of` carries probability. See Details.
#' @param ... Currently unused; must be empty.
#' @return The conditional distribution, renormalised to total probability
#' 1; or the Null distribution, if the trim leaves nothing behind.
#' @seealso [graft_left()] and [graft_right()], which replace a tail
#' rather than discarding it.
#' @examples
#' d <- distionary::dst_norm(0, 1)
#' d <- trim_left(d, -2)
#' d <- trim_right(d, 2)
#' distionary::enframe_cdf(d, at = -3:3)
#'
#' # A Poisson has an atom at 5, so the knot is visible there.
#' d <- distionary::dst_pois(3)
#' distionary::eval_pmf(trim_left(d, 5), at = 5)
#' distionary::eval_pmf(trim_left(d, 5, knot_action = "keep"), at = 5)
#' distionary::eval_pmf(trim_left(d, 5, knot_action = "split"), at = 5)
#' @rdname trim
#' @export
trim_left <- function(distribution, of, ...,
                      knot_action = c("discard", "keep", "split")) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  rlang::check_dots_empty()
  knot_action <- rlang::arg_match0(
    knot_action, c("discard", "keep", "split"), "knot_action"
  )
  # A Null distribution has no probability to keep or discard; trimming it
  # leaves it Null, as every other verb does.
  if (is.na(distribution)) {
    return(distribution)
  }
  # A graft is a mixture in all but name, and trims the same way.
  if (distionary::pretty_name(distribution) %in% c("Mixture", "Graft")) {
    params <- distionary::parameters(distribution)
    components <- params[["distributions"]]
    mix_probs <- params[["probs"]]
    probs_kept <- vapply(
      components,
      function(d) {
        distionary::prob_right(d, of = of, inclusive = FALSE) +
          knot_retained(d, of, knot_action)
      },
      FUN.VALUE = numeric(1L)
    )
    new_mix_weights <- probs_kept * mix_probs
    if (all(new_mix_weights == 0)) {
      return(distionary::dst_null())
    }
    # A component entirely trimmed away becomes a Null distribution, which
    # would null the whole mixture; drop dead components instead.
    keep <- new_mix_weights > 0
    trimmed_components <- lapply(components[keep], function(d) {
      suppressWarnings(trim_left(d, of = of, knot_action = knot_action))
    })
    return(do.call(
      mix,
      c(trimmed_components, list(weights = new_mix_weights[keep]))
    ))
  }
  if (distionary::pretty_name(distribution) == "Finite") {
    parms <- distionary::parameters(distribution)
    outs <- parms[["outcomes"]]
    probs <- parms[["probs"]]
    share <- switch(knot_action, keep = 1, discard = 0, split = 0.5)
    probs[outs == of] <- probs[outs == of] * share
    keep <- outs >= of & probs > 0
    probs <- probs[keep]
    outs <- outs[keep]
    if (length(outs) == 0) {
      return(distionary::dst_null())
    }
    return(distionary::dst_empirical(outs, weights = probs))
  }
  retained <- knot_retained(distribution, of, knot_action)
  p_kept <- distionary::prob_right(distribution, of = of, inclusive = FALSE) +
    retained
  if (p_kept == 1) {
    return(distribution)
  }
  if (p_kept == 0) {
    return(distionary::dst_null())
  }
  support_in <- distionary::support(distribution)
  if (is.null(support_in)) {
    stop(
      "Trimming requires the distribution's support. ",
      "Specify `.support` when building the distribution."
    )
  }
  # The knot stays in the support whenever any of its mass is retained.
  # If `of` falls on a flat region (a gap in the support), the restriction
  # shifts the lower endpoint to where the support resumes.
  support_out <- distionary::support_restrict(
    support_in,
    from = of,
    to = Inf,
    include_from = retained > 0 || knot_action == "keep"
  )
  if (distionary::is_empty_support(support_out)) {
    return(distionary::dst_null())
  }
  lower_endpoint <- range(support_out)[[1L]]
  cdf_of <- distionary::eval_cdf(distribution, at = of)
  # Mass at or below the knot that the trim keeps, as a probability of the
  # trimmed distribution. Zero unless some of the knot's mass is retained.
  p_at_knot <- retained / p_kept
  d <- distionary::distribution(
    cdf = function(x) {
      res <- (retained + distionary::eval_cdf(distribution, at = x) -
        cdf_of) / p_kept
      res[x < of] <- 0
      pmin(pmax(res, 0), 1)
    },
    # Scaling the base survival, rather than taking one minus the CDF above.
    # The two agree on paper, but the far tail is where they part: there the
    # base CDF is one to the last bit it can hold, and subtracting it from
    # one leaves nothing of the probability that is still out there. The
    # knot's retained mass sits at `of`, never above `x`, so it plays no
    # part here.
    survival = function(x) {
      res <- distionary::eval_survival(distribution, at = x) / p_kept
      res[x < of] <- 1
      pmin(pmax(res, 0), 1)
    },
    density = function(x) {
      pdf <- distionary::eval_density(distribution, at = x) / p_kept
      pdf[x < of] <- 0
      if (knot_action == "discard") {
        pdf[x == of] <- 0
      }
      pdf
    },
    pmf = function(x) {
      pmf <- distionary::eval_pmf(distribution, at = x) / p_kept
      pmf[x < of] <- 0
      pmf[x == of] <- retained / p_kept
      pmf
    },
    quantile = function(p) {
      res <- rep(NA_real_, length(p))
      at_knot <- !is.na(p) & p <= p_at_knot
      beyond <- !is.na(p) & p > p_at_knot
      res[at_knot] <- of
      if (any(beyond)) {
        res[beyond] <- distionary::eval_quantile(
          distribution, at = cdf_of + (p[beyond] - p_at_knot) * p_kept
        )
      }
      # On a flat region the base left-inverse lands at the left side of the
      # gap; the trimmed distribution starts where its support does.
      low <- !is.na(res) & res < lower_endpoint
      res[low] <- lower_endpoint
      res
    },
    .support = support_out,
    .name = "Left-Trimmed",
    .parameters = list(
      distribution = distribution,
      of = of,
      knot_action = knot_action
    )
  )
  distionary:::new_distribution(d, class = "trim_left")
}

#' @rdname trim
#' @export
trim_right <- function(distribution, of, ...,
                       knot_action = c("discard", "keep", "split")) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  rlang::check_dots_empty()
  knot_action <- rlang::arg_match0(
    knot_action, c("discard", "keep", "split"), "knot_action"
  )
  # A Null distribution has no probability to keep or discard; trimming it
  # leaves it Null, as every other verb does.
  if (is.na(distribution)) {
    return(distribution)
  }
  # A graft is a mixture in all but name, and trims the same way.
  if (distionary::pretty_name(distribution) %in% c("Mixture", "Graft")) {
    params <- distionary::parameters(distribution)
    components <- params[["distributions"]]
    mix_probs <- params[["probs"]]
    probs_kept <- vapply(
      components,
      function(d) {
        distionary::prob_left(d, of = of, inclusive = FALSE) +
          knot_retained(d, of, knot_action)
      },
      FUN.VALUE = numeric(1L)
    )
    new_mix_weights <- probs_kept * mix_probs
    if (all(new_mix_weights == 0)) {
      return(distionary::dst_null())
    }
    # A component entirely trimmed away becomes a Null distribution, which
    # would null the whole mixture; drop dead components instead.
    keep <- new_mix_weights > 0
    trimmed_components <- lapply(components[keep], function(d) {
      suppressWarnings(trim_right(d, of = of, knot_action = knot_action))
    })
    return(do.call(
      mix,
      c(trimmed_components, list(weights = new_mix_weights[keep]))
    ))
  }
  if (distionary::pretty_name(distribution) == "Finite") {
    parms <- distionary::parameters(distribution)
    outs <- parms[["outcomes"]]
    probs <- parms[["probs"]]
    share <- switch(knot_action, keep = 1, discard = 0, split = 0.5)
    probs[outs == of] <- probs[outs == of] * share
    keep <- outs <= of & probs > 0
    probs <- probs[keep]
    outs <- outs[keep]
    if (length(outs) == 0) {
      return(distionary::dst_null())
    }
    return(distionary::dst_empirical(outs, weights = probs))
  }
  retained <- knot_retained(distribution, of, knot_action)
  p_kept <- distionary::prob_left(distribution, of = of, inclusive = FALSE) +
    retained
  if (p_kept == 1) {
    return(distribution)
  }
  if (p_kept == 0) {
    return(distionary::dst_null())
  }
  support_in <- distionary::support(distribution)
  if (is.null(support_in)) {
    stop(
      "Trimming requires the distribution's support. ",
      "Specify `.support` when building the distribution."
    )
  }
  # The knot stays in the support whenever any of its mass is retained.
  # If `of` falls on a flat region (a gap in the support), the restriction
  # shifts the upper endpoint to where the support leaves off.
  support_out <- distionary::support_restrict(
    support_in,
    from = -Inf,
    to = of,
    include_to = retained > 0 || knot_action == "keep"
  )
  if (distionary::is_empty_support(support_out)) {
    return(distionary::dst_null())
  }
  upper_endpoint <- range(support_out)[[2L]]
  d <- distionary::distribution(
    cdf = function(x) {
      cdf <- distionary::eval_cdf(distribution, at = x) / p_kept
      pmin(cdf, 1)
    },
    density = function(x) {
      pdf <- distionary::eval_density(distribution, at = x) / p_kept
      pdf[x > of] <- 0
      if (knot_action == "discard") {
        pdf[x == of] <- 0
      }
      pdf
    },
    pmf = function(x) {
      pmf <- distionary::eval_pmf(distribution, at = x) / p_kept
      pmf[x > of] <- 0
      pmf[x == of] <- retained / p_kept
      pmf
    },
    quantile = function(p) {
      res <- distionary::eval_quantile(distribution, at = p * p_kept)
      # Mirror of the trim_left clamp, for safety at the upper endpoint.
      high <- !is.na(res) & res > upper_endpoint
      res[high] <- upper_endpoint
      res
    },
    .support = support_out,
    .name = "Right-Trimmed",
    .parameters = list(
      distribution = distribution,
      of = of,
      knot_action = knot_action
    )
  )
  distionary:::new_distribution(d, class = "trim_right")
}
