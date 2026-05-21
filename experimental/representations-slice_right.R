#' @export
eval_cdf.slice_right <- function(distribution, at) {
  with(distribution, {
    p_kept <- distionary::prob_left(
      distribution,
      of = breakpoint,
      inclusive = !include
    )
    cdf <- distionary::eval_cdf(distribution, at = at) / p_kept
    pmin(cdf, 1)
  })
}

#' @export
eval_density.slice_right <- function(distribution, at, strict = TRUE) {
  with(distribution, {
    p_kept <- distionary::prob_left(
      distribution,
      of = breakpoint,
      inclusive = !include
    )
    pdf <- distionary::eval_density(
      distribution,
      at = at,
      strict = strict
    ) /
      p_kept
    if (include) {
      pdf[at >= breakpoint] <- 0
    } else {
      pdf[at > breakpoint] <- 0
    }
    pdf
  })
}

#' @export
eval_pmf.slice_right <- function(distribution, at, strict = TRUE) {
  with(distribution, {
    p_kept <- distionary::prob_left(
      distribution,
      of = breakpoint,
      inclusive = !include
    )
    pmf <- distionary::eval_pmf(
      distribution,
      at = at,
      strict = strict
    ) /
      p_kept
    if (include) {
      pmf[at >= breakpoint] <- 0
    } else {
      pmf[at > breakpoint] <- 0
    }
    pmf
  })
}

#' @export
eval_quantile.slice_right <- function(distribution, at) {
  with(distribution, {
    p_kept <- distionary::prob_left(
      distribution,
      of = breakpoint,
      inclusive = !include
    )
    distionary::eval_quantile(distribution, at = at * p_kept)
  })
}
