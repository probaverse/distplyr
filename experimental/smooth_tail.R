#' Calculate the Internal Correction Term (Tracking Integral)
#'
#' Evaluates the integral component of the cumulative hazard function numerically.
#'
#' @param x Vector of evaluation points.
#' @param x0 Scalar anchor/graft point.
#' @param sf_E Function for the body survival function (takes x, x0).
#' @param pdf_E Function for the body density function (takes x, x0).
#' @param sf_G Function for the tail survival function (takes x, x0).
#' @param pdf_G Function for the tail density function (takes x, x0).
#' @param w Function for the weight function (takes x, x0).
#' @param w_prime Function for the first derivative of the weight function (takes x, x0).
#'
#' @return A vector of integrated correction values corresponding to each x.
stt_correction_integral <- function(x, x0, sf_E, pdf_E, sf_G, pdf_G, w, w_prime) {
  
  # The integrand function to be passed to numerical quadrature
  integrand <- function(t) {
    num <- w_prime(t, x0) * (sf_E(t, x0) - sf_G(t, x0))
    den <- w(t, x0) * sf_E(t, x0) + (1 - w(t, x0)) * sf_G(t, x0)
    
    # Handle potential 0/0 or boundary numerical edge cases gracefully
    out <- num / den
    out[is.nan(out) | is.infinite(out)] <- 0
    return(out)
  }
  
  # Vectorize the integration over the input vector x
  sapply(x, function(cur_x) {
    if (cur_x <= x0) return(0)
    
    # Perform adaptive numerical integration from x0 to cur_x
    res <- tryCatch({
      integrate(integrand, lower = x0, upper = cur_x, rel.tol = 1e-8)$value
    }, error = function(e) {
      warning("Numerical integration failed at x = ", cur_x, "; returning 0.")
      return(0)
    })
    return(res)
  })
}

#' Smooth Tail Transition: Hazard Function h(x)
#' @export
hstt <- function(x, x0, sf_E, pdf_E, sf_G, pdf_G, w, w_prime) {
  res <- numeric(length(x))
  
  # Below x0, the distribution matches the baseline body hazard directly
  idx_below <- x <= x0
  if (any(idx_below)) {
    res[idx_below] <- pdf_E(x[idx_below], x0) / sf_E(x[idx_below], x0)
  }
  
  # Above x0, evaluate the continuous-limit hazard mixture formula
  idx_above <- !idx_below
  if (any(idx_above)) {
    num <- w(x[idx_above], x0) * pdf_E(x[idx_above], x0) + (1 - w(x[idx_above], x0)) * pdf_G(x[idx_above], x0)
    den <- w(x[idx_above], x0) * sf_E(x[idx_above], x0) + (1 - w(x[idx_above], x0)) * sf_G(x[idx_above], x0)
    res[idx_above] <- num / den
  }
  
  return(res)
}

#' Smooth Tail Transition: Cumulative Hazard Function H(x)
#' @export
Hstt <- function(x, x0, sf_E, pdf_E, sf_G, pdf_G, w, w_prime) {
  res <- numeric(length(x))
  
  idx_below <- x <= x0
  if (any(idx_below)) {
    res[idx_below] <- -log(sf_E(x[idx_below], x0))
  }
  
  idx_above <- !idx_below
  if (any(idx_above)) {
    # Analytical baseline + tracking integral
    base_term <- -log(w(x[idx_above], x0) * sf_E(x[idx_above], x0) + (1 - w(x[idx_above], x0)) * sf_G(x[idx_above], x0))
    integral_term <- stt_correction_integral(x[idx_above], x0, sf_E, pdf_E, sf_G, pdf_G, w, w_prime)
    res[idx_above] <- base_term + integral_term
  }
  
  return(res)
}

#' Smooth Tail Transition: Survival Function S(x)
#' @export
sstt <- function(x, x0, sf_E, pdf_E, sf_G, pdf_G, w, w_prime) {
  # S(x) = exp(-H(x))
  exp(-Hstt(x, x0, sf_E, pdf_E, sf_G, pdf_G, w, w_prime))
}

#' Smooth Tail Transition: Probability Density Function f(x)
#' @export
dstt <- function(x, x0, sf_E, pdf_E, sf_G, pdf_G, w, w_prime) {
  res <- numeric(length(x))
  
  idx_below <- x <= x0
  if (any(idx_below)) {
    res[idx_below] <- pdf_E(x[idx_below], x0)
  }
  
  idx_above <- !idx_below
  if (any(idx_above)) {
    # f(x) = [w(x)e(x) + w_bar(x)g(x)] * exp(-Integral)
    mixture_density <- w(x[idx_above], x0) * pdf_E(x[idx_above], x0) + (1 - w(x[idx_above], x0)) * pdf_G(x[idx_above], x0)
    integral_term <- stt_correction_integral(x[idx_above], x0, sf_E, pdf_E, sf_G, pdf_G, w, w_prime)
    res[idx_above] <- mixture_density * exp(-integral_term)
  }
  
  return(res)
}

#' Smooth Tail Transition: Cumulative Distribution Function F(x)
#' @export
pstt <- function(x, x0, sf_E, pdf_E, sf_G, pdf_G, w, w_prime) {
  1 - sstt(x, x0, sf_E, pdf_E, sf_G, pdf_G, w, w_prime)
}