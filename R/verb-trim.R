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
#' distionary::dst_norm(0, 1) |>
#'   trim_left(-2) |>
#'   trim_right(2) |>
#'   distionary::enframe_cdf(at = -3:3)
#'
#' d <- distionary::dst_pois(3)
#' d |>
#'   trim_left(5) |>
#'   distionary::eval_pmf(at = 5)
#' d |>
#'   trim_left(5, include = FALSE) |>
#'   distionary::eval_pmf(at = 5)
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
    trimmed_components <- lapply(components, function(d) {
      suppressWarnings(trim_left(d, of = of, include = include))
    })
    if (all(new_mix_weights == 0)) {
      return(distionary::dst_null())
    }
    return(do.call(mix, c(trimmed_components, list(weights = new_mix_weights))))
  }
  if (distionary::pretty_name(distribution) == "Empirical") {
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
  right <- range(distribution)[2L]
  p_kept <- distionary::prob_right(
    distribution, of = of, inclusive = !include
  )
  if (p_kept == 1) {
    return(distribution)
  }
  if (p_kept == 0) {
    return(distionary::dst_null())
  }
  v <- distionary::vtype(distribution)
  if (v == "mixed") {
    v <- "unknown" # For now.
  }
  ## If `of` is on a flat part of the CDF, this will inaccurately
  ## specify the left endpoint of the distribution (and the 0-quantile).
  ## If this is the case, do not specify range, and adjust quantile function
  ## to calculate 0-quantile using the quantile algorithm.
  dens_at_break <- distionary::eval_density(distribution, at = of)
  if (dens_at_break == 0) {
    ## Trim occurs on flat part
    stop("Trim occurs on flat part. This is not supported.")
  }

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
      distionary::eval_quantile(distribution, at = (1 - p_kept) + p * p_kept)
    },
    range = c(of, right),
    .vtype = v,
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
    trimmed_components <- lapply(components, function(d) {
      suppressWarnings(trim_right(d, of = of, include = include))
    })
    if (all(new_mix_weights == 0)) {
      return(distionary::dst_null())
    }
    return(do.call(mix, c(trimmed_components, list(weights = new_mix_weights))))
  }
  if (distionary::pretty_name(distribution) == "Empirical") {
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
  left <- range(distribution)[1L]
  p_kept <- distionary::prob_left(
    distribution, of = of, inclusive = !include
  )
  if (p_kept == 1) {
    return(distribution)
  }
  if (p_kept == 0) {
    return(distionary::dst_null())
  }
  v <- distionary::vtype(distribution)
  if (v == "mixed") {
    v <- "unknown" # For now.
  }
  ## If `of` is on a flat part of the CDF, this will inaccurately
  ## specify the left endpoint of the distribution (and the 0-quantile).
  ## If this is the case, do not specify range, and adjust quantile function
  ## to calculate 0-quantile using the quantile algorithm.
  dens_at_break <- distionary::eval_density(distribution, at = of)
  if (dens_at_break == 0) {
    ## Trim occurs on flat part
    stop("Trim occurs on flat part. This is not supported.")
  }
  
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
      distionary::eval_quantile(distribution, at = p * p_kept)
    },
    range = c(left, of),
    .vtype = v,
    .name = "Right-Trimmed",
    .parameters = list(
      distribution = distribution,
      of = of,
      include = include
    )
  )
  distionary:::new_distribution(d, class = "trim_right")
}
