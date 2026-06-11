#' Trim (condition) a distribution
#'
#' `trim_left()` removes probability to the left of some value,
#' conditioning the random variable to be bigger than that value.
#' `trim_right()` does the opposite: removes probability to the right,
#' conditioning to be smaller than that value.
#'
#' @param distribution Distribution to trim.
#' @param of Value on the real line defining where to trim (single numeric).
#' @param include Logical; should `of` be removed from the support as well?
#' This is only realistically relevant if `of` has a non-zero probability
#' of occurrence.
#' @param ... Currently unused.
#' @return A conditional distribution.
#' @examples
#' d <- distionary::dst_norm(0, 1)
#' d <- trim_left(d, -2)
#' d <- trim_right(d, 2)
#' distionary::enframe_cdf(d, at = -3:3)
#'
#' d <- distionary::dst_pois(3)
#' distionary::eval_pmf(trim_left(d, 5), at = 5)
#' distionary::eval_pmf(trim_left(d, 5, include = FALSE), at = 5)
#' @rdname trim
#' @export
trim_left <- function(distribution, of, ..., include = TRUE) {
  checkmate::assert_class(distribution, "dst")
  checkmate::assert_number(of, finite = TRUE, na.ok = FALSE)
  checkmate::assert_logical(include, len = 1L, any.missing = FALSE)
  ellipsis::check_dots_empty()
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
  support_out <- restrict_support(
    support_in, from = of, to = Inf, include_from = !include
  )
  if (is.null(support_out)) {
    return(distionary::dst_null())
  }
  lower_endpoint <- support_min(support_out)
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
  ellipsis::check_dots_empty()
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
  support_out <- restrict_support(
    support_in, from = -Inf, to = of, include_to = !include
  )
  if (is.null(support_out)) {
    return(distionary::dst_null())
  }
  upper_endpoint <- support_max(support_out)
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
