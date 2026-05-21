#' Calculate the Internal Correction Term (Tracking Integral)
#'
#' Evaluates the integral component of the cumulative hazard function numerically.
#'
#' @param x Vector of evaluation points.
#' @param x0 Scalar anchor/graft point.
#' @param d_E A distionary object for the body distribution.
#' @param d_G A distionary object for the tail distribution.
#' @param w Function for the weight function (takes x, x0).
#' @param w_prime Function for the first derivative of the weight function (takes x, x0).
#'
#' @return A vector of integrated correction values corresponding to each x.
stt_correction_integral <- function(x, x0, d_E, d_G, w, w_prime) {
  # Condition both baseline distributions on being above x0
  d_E_cond <- trim_left(d_E, of = x0)
  d_G_cond <- trim_left(d_G, of = x0)

  # The integrand function utilizing distionary evaluation methods
  integrand <- function(t) {
    sf_E <- distionary::eval_survival(d_E_cond, at = t)
    sf_G <- distionary::eval_survival(d_G_cond, at = t)

    num <- w_prime(t, x0) * (sf_E - sf_G)
    den <- w(t, x0) * sf_E + (1 - w(t, x0)) * sf_G

    out <- num / den
    out[is.nan(out) | is.infinite(out)] <- 0
    return(out)
  }

  # Vectorize the integration over the input vector x
  sapply(x, function(cur_x) {
    if (cur_x <= x0) {
      return(0)
    }

    res <- tryCatch(
      {
        integrate(integrand, lower = x0, upper = cur_x, rel.tol = 1e-8)$value
      },
      error = function(e) {
        warning("Numerical integration failed at x = ", cur_x, "; returning 0.")
        return(0)
      }
    )
    return(res)
  })
}

#' Smooth Tail Transition: Hazard Function h(x)
#' @export
hstt <- function(x, x0, d_E, d_G, w, w_prime) {
  res <- numeric(length(x))

  # Condition both baseline distributions on being above x0
  d_E_cond <- trim_left(d_E, of = x0)
  d_G_cond <- trim_left(d_G, of = x0)

  # Below x0, it matches the body hazard directly
  idx_below <- x <= x0
  if (any(idx_below)) {
    res[idx_below] <- distionary::eval_hazard(d_E, at = x[idx_below])
  }

  # Above x0, evaluate the continuous-limit hazard mixture formula
  idx_above <- !idx_below
  if (any(idx_above)) {
    num <- w(x[idx_above], x0) *
      distionary::eval_density(d_E_cond, at = x[idx_above]) +
      (1 - w(x[idx_above], x0)) *
        distionary::eval_density(d_G_cond, at = x[idx_above])
    den <- w(x[idx_above], x0) *
      distionary::eval_survival(d_E_cond, at = x[idx_above]) +
      (1 - w(x[idx_above], x0)) *
        distionary::eval_survival(d_G_cond, at = x[idx_above])
    res[idx_above] <- num / den
  }

  return(res)
}

#' Smooth Tail Transition: Cumulative Hazard Function H(x)
#' @export
Hstt <- function(x, x0, d_E, d_G, w, w_prime) {
  res <- numeric(length(x))

  # Condition both baseline distributions on being above x0
  d_E_cond <- trim_left(d_E, of = x0)
  d_G_cond <- trim_left(d_G, of = x0)

  idx_below <- x <= x0
  if (any(idx_below)) {
    res[idx_below] <- distionary::eval_chf(d_E, at = x[idx_below])
  }

  idx_above <- !idx_below
  if (any(idx_above)) {
    # Analytical mixture base + tracking integral
    sf_mixture <- w(x[idx_above], x0) *
      distionary::eval_survival(d_E_cond, at = x[idx_above]) +
      (1 - w(x[idx_above], x0)) *
        distionary::eval_survival(d_G_cond, at = x[idx_above])

    base_term <- -log(sf_mixture)
    integral_term <- stt_correction_integral(
      x[idx_above],
      x0,
      d_E,
      d_G,
      w,
      w_prime
    )
    res[idx_above] <- base_term + integral_term
  }

  return(res)
}

#' Smooth Tail Transition: Survival Function S(x)
#' @export
sstt <- function(x, x0, d_E, d_G, w, w_prime) {
  exp(-Hstt(x, x0, d_E, d_G, w, w_prime))
}

#' Smooth Tail Transition: Probability Density Function f(x)
#' @export
dstt <- function(x, x0, d_E, d_G, w, w_prime) {
  res <- numeric(length(x))

  idx_below <- x <= x0
  if (any(idx_below)) {
    res[idx_below] <- distionary::eval_density(d_E, at = x[idx_below])
  }

  idx_above <- !idx_below
  if (any(idx_above)) {
    mixture_density <- w(x[idx_above], x0) *
      distionary::eval_density(d_E_cond, at = x[idx_above]) +
      (1 - w(x[idx_above], x0)) *
        distionary::eval_density(d_G_cond, at = x[idx_above])
    integral_term <- stt_correction_integral(
      x[idx_above],
      x0,
      d_E,
      d_G,
      w,
      w_prime
    )
    res[idx_above] <- mixture_density * exp(-integral_term)
  }

  return(res)
}
