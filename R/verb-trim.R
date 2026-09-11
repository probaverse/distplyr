#' Trim (condition) a distribution
#'
#' Discard the probability lying to one side of a value, and scale up what
#' remains so that it sums to 1 again. `trim_left()` discards the
#' probability below `of`, giving the distribution of the variable
#' conditioned on landing above it; `trim_right()` discards the probability
#' above `of`, conditioning on landing below.
#'
#' @details
#' # What `include` does
#'
#' `include` settles what happens *at* `of` itself, and it is named for the
#' side being trimmed away: `include = TRUE`, the default, counts `of` as
#' part of what is discarded, so `trim_left(d, of)` keeps only outcomes
#' strictly greater than `of`. With `include = FALSE`, `of` is kept, and
#' `trim_left(d, of)` keeps outcomes greater than *or equal to* `of`.
#'
#' The choice only shows when `of` carries probability of its own, as an
#' atom does. Where there is no mass exactly at `of` --- anywhere in a
#' continuous distribution --- both settings give the same answer.
#'
#' Beware that `graft_left()` and `graft_right()` read their `include`
#' the other way about: theirs says whether `of` stays with the base
#' distribution, so there `TRUE` keeps it.
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
#' @param include Logical; should `of` be discarded along with the side
#' being trimmed away? Defaults to `TRUE`. Makes a difference only where
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
#' # A Poisson has an atom at 5, so `include` is visible there.
#' # The default discards that atom; `include = FALSE` keeps it.
#' d <- distionary::dst_pois(3)
#' distionary::eval_pmf(trim_left(d, 5), at = 5)
#' distionary::eval_pmf(trim_left(d, 5, include = FALSE), at = 5)
#' @rdname trim
#' @export
trim_left <- function(distribution, of, ..., include = TRUE) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  checkmate::assert_logical(include, len = 1L, any.missing = FALSE)
  rlang::check_dots_empty()
  # A Null distribution has no probability to keep or discard; trimming it
  # leaves it Null, as every other verb does.
  if (is.na(distribution)) {
    return(distribution)
  }
  if (distionary::pretty_name(distribution) == "Mixture") {
    params <- distionary::parameters(distribution)
    components <- params[["distributions"]]
    mix_probs <- params[["probs"]]
    probs_kept <- vapply(
      components,
      function(d) {
        distionary::prob_right(d, of = of, inclusive = !include)
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
      suppressWarnings(trim_left(d, of = of, include = include))
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
    if (include) {
      rm <- outs <= of
    } else {
      rm <- outs < of
    }
    probs <- probs[!rm]
    outs <- outs[!rm]
    if (length(outs) == 0) {
      return(distionary::dst_null())
    }
    return(distionary::dst_empirical(outs, weights = probs))
  }
  p_kept <- distionary::prob_right(
    distribution, of = of, inclusive = !include
  )
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
  # `include = TRUE` means `of` is removed, so the kept support excludes it.
  # If `of` falls on a flat region (a gap in the support), the restriction
  # shifts the lower endpoint to where the support resumes.
  support_out <- distionary::support_restrict(
    support_in, from = of, to = Inf, include_from = !include
  )
  if (distionary::is_empty_support(support_out)) {
    return(distionary::dst_null())
  }
  lower_endpoint <- range(support_out)[[1L]]
  d <- distionary::distribution(
    cdf = function(x) {
      cdf <- 1 - distionary::eval_survival(distribution, at = x) / p_kept
      pmax(cdf, 0)
    },
    survival = function(x) {
      s <- distionary::eval_survival(distribution, at = x) / p_kept
      pmin(s, 1)
    },
    density = function(x) {
      pdf <- distionary::eval_density(distribution, at = x) / p_kept
      if (include) {
        pdf[x <= of] <- 0
      } else {
        pdf[x < of] <- 0
      }
      pdf
    },
    pmf = function(x) {
      pmf <- distionary::eval_pmf(distribution, at = x) / p_kept
      if (include) {
        pmf[x <= of] <- 0
      } else {
        pmf[x < of] <- 0
      }
      pmf
    },
    quantile = function(p) {
      res <- distionary::eval_quantile(
        distribution, at = (1 - p_kept) + p * p_kept
      )
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
      include = include
    )
  )
  distionary:::new_distribution(d, class = "trim_left")
}

#' @rdname trim
#' @export
trim_right <- function(distribution, of, ..., include = TRUE) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  checkmate::assert_logical(include, len = 1L, any.missing = FALSE)
  rlang::check_dots_empty()
  # A Null distribution has no probability to keep or discard; trimming it
  # leaves it Null, as every other verb does.
  if (is.na(distribution)) {
    return(distribution)
  }
  if (distionary::pretty_name(distribution) == "Mixture") {
    params <- distionary::parameters(distribution)
    components <- params[["distributions"]]
    mix_probs <- params[["probs"]]
    probs_kept <- vapply(
      components,
      function(d) {
        distionary::prob_left(d, of = of, inclusive = !include)
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
      suppressWarnings(trim_right(d, of = of, include = include))
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
    if (include) {
      rm <- outs >= of
    } else {
      rm <- outs > of
    }
    probs <- probs[!rm]
    outs <- outs[!rm]
    if (length(outs) == 0) {
      return(distionary::dst_null())
    }
    return(distionary::dst_empirical(outs, weights = probs))
  }
  p_kept <- distionary::prob_left(
    distribution, of = of, inclusive = !include
  )
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
  # `include = TRUE` means `of` is removed, so the kept support excludes it.
  # If `of` falls on a flat region (a gap in the support), the restriction
  # shifts the upper endpoint to where the support leaves off.
  support_out <- distionary::support_restrict(
    support_in, from = -Inf, to = of, include_to = !include
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
      if (include) {
        pdf[x >= of] <- 0
      } else {
        pdf[x > of] <- 0
      }
      pdf
    },
    pmf = function(x) {
      pmf <- distionary::eval_pmf(distribution, at = x) / p_kept
      if (include) {
        pmf[x >= of] <- 0
      } else {
        pmf[x > of] <- 0
      }
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
      include = include
    )
  )
  distionary:::new_distribution(d, class = "trim_right")
}
