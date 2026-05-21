#' @export
eval_cdf.slice_left <- function(distribution, at) {
  with(distribution, {
    p_kept <- distionary::prob_right(
      distribution,
      of = breakpoint,
      inclusive = !include
    )
    cdf <- 1 - distionary::eval_survival(distribution, at = at) / p_kept
    pmax(cdf, 0)
  })
}

#' @export
eval_survival.slice_left <- function(distribution, at) {
  with(distribution, {
    p_kept <- distionary::prob_right(
      distribution,
      of = breakpoint,
      inclusive = !include
    )
    s <- distionary::eval_survival(distribution, at = at) / p_kept
    pmin(s, 1)
  })
}

#' @export
eval_density.slice_left <- function(distribution, at, strict) {
  with(distribution, {
    p_kept <- distionary::prob_right(
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
      pdf[at <= breakpoint] <- 0
    } else {
      pdf[at < breakpoint] <- 0
    }
    pdf
  })
}

#' @export
eval_pmf.slice_left <- function(distribution, at, strict) {
  with(distribution, {
    p_kept <- distionary::prob_right(
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
      pmf[at <= breakpoint] <- 0
    } else {
      pmf[at < breakpoint] <- 0
    }
    pmf
  })
}

#' @export
eval_quantile.slice_left <- function(distribution, at) {
  with(distribution, {
    p_kept <- distionary::prob_right(
      distribution,
      of = breakpoint,
      inclusive = !include
    )
    distionary::eval_quantile(
      distribution,
      at = (1 - p_kept) + at * p_kept
    )
  })
}
