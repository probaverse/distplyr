#' Mixture Distributions
#'
#' Create a mixture distribution.
#'
#' @inheritParams dots_to_dsts
#' @param weights Vector of weights corresponding to the distributions;
#' or, single numeric for equal weights.
#' @param na.rm Remove `NA` distributions and `NA` weights? `TRUE` if yes;
#' default is `FALSE`.
#' @return A mixture distribution -- an empty distribution if any weights
#' are \code{NA} and `na.rm = FALSE`, the default.
#' @examples
#' library(distionary)
#' a <- dst_norm(0, 1)
#' b <- dst_norm(5, 2)
#' m1 <- mix(a, b, weights = c(1, 4))
#' #plot(m1)
#' vtype(m1)
#'
#' c <- dst_pois(6)
#' m2 <- mix(a, b, c, weights = c(0.2, 0.5, 0.3))
#' #plot(m2, n = 1001)
#' vtype(m2)
#' @export
mix <- function(..., weights = 1, na.rm = FALSE) {
  dsts <- dots_to_dsts(..., na.rm = na.rm)
  n_dsts <- length(dsts)
  if (n_dsts == 0) {
    warning("Received no distributions. Returning NULL.")
    return(NULL)
  }
  if (n_dsts == 1) {
    return(dsts[[1L]])
  }
  weights <- vctrs::vec_recycle(weights, size = n_dsts)
  if (any(weights < 0, na.rm = TRUE)) {
    stop("Weights must not be negative.")
  }
  probs <- weights / sum(weights, na.rm = TRUE)
  na_probs <- is.na(probs)
  if (any(na_probs)) {
    if (!na.rm) {
      return(NA)
    }
    probs <- probs[!na_probs]
    dsts <- dsts[!na_probs]
  }
  zero_probs <- probs == 0
  if (any(zero_probs)) {
    probs <- probs[!zero_probs]
    dsts <- dsts[!zero_probs]
  }
  if (length(probs) == 1L) {
    return(dsts[[1L]])
  }
  r <- lapply(dsts, range)
  r1 <- min(vapply(r, \(x) x[1], FUN.VALUE = numeric(1L)))
  r2 <- max(vapply(r, \(x) x[2], FUN.VALUE = numeric(1L)))
  var_type <- vapply(dsts, distionary::vtype, FUN.VALUE = character(1L))
  var_unique <- unique(var_type)
  if ("unknown" %in% var_type) {
    var_unique <- "unknown"
  } else if (length(var_unique) > 1L) {
    var_unique <- "mixed"
  }
  ## Make distribution object
  d <- distionary::distribution(
    cdf = function(x) {
      cdf_vals <- lapply(dsts, distionary::eval_cdf, at = x)
      p_times_cdfs <- mapply(`*`, probs, cdf_vals, SIMPLIFY = FALSE)
      Reduce(`+`, p_times_cdfs)
    },
    survival = function(x) {
      surv_vals <- lapply(dsts, distionary::eval_survival, at = x)
      p_times_survs <- mapply(`*`, probs, surv_vals, SIMPLIFY = FALSE)
      Reduce(`+`, p_times_survs)
    },
    pmf = function(x) {
      pmf_vals <- lapply(dsts, distionary::eval_pmf, at = x)
      p_times_f <- mapply(`*`, probs, pmf_vals, SIMPLIFY = FALSE)
      Reduce(`+`, p_times_f)
    },
    density = function(x) {
      density_vals <- lapply(dsts, distionary::eval_density, at = x)
      p_times_f <- mapply(`*`, probs, density_vals, SIMPLIFY = FALSE)
      Reduce(`+`, p_times_f)
    },
    realise = function(n) {
      if (n == 0) {
        return(numeric())
      }
      k <- length(dsts)
      id <- sample(1:k, size = n, replace = TRUE, prob = probs)
      vapply(
        id,
        \(i) distionary::realise(dsts[[i]], n = 1),
        FUN.VALUE = numeric(1L)
      )
    },
    range = c(r1, r2),
    .vtype = var_unique,
    .name = "Mixture",
    .parameters = list(
      distributions = dsts,
      probs = probs
    )
  )
  new_mixture(d)
}


#' @export
print.mixture <- function(x, ...) {
  cat("Mixture Distribution\n")
  cat("\nComponents: ")
  cat("\n")
  params <- distionary::parameters(x)
  params[["distributions"]] <- vapply(
    params[["distributions"]], pretty_name, FUN.VALUE = character(1L)
  )
  if (requireNamespace("tibble", quietly = TRUE)) {
    df <- tibble::as_tibble(params)
  } else {
    df <- as.data.frame(params)
  }
  print(df, ...)
}

