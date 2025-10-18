#' Mixture Distributions
#'
#' Create a mixture distribution.
#'
#' @inheritParams dots_to_dsts
#' @param weights Vector of weights corresponding to the distributions;
#' or, single numeric for equal weights.
#' @param na.rm Remove `NA` distributions and `NA` weights? `TRUE` if yes;
#' default is `FALSE`.
#' @return A mixture distribution.
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
mix <- function(...,
                weights = 1,
                na_action_dst = c("null", "drop", "fail"),
                na_action_w = c("null", "drop", "fail")) {
  preprocess <- pair_dots_num(
    ...,
    num = weights,
    na_action_dst = na_action_dst,
    na_action_num = na_action_w
  )
  if (distionary::is_distribution(preprocess)) {
    return(preprocess)
  }
  dsts <- preprocess$dsts
  weights <- preprocess$num
  if (length(weights) == 1) {
    return(dsts[[1]])
  }
  probs <- weights / sum(weights, na.rm = TRUE)
  ## Special cases: all finite distributions
  all_finite <- all(vapply(
    dsts, \(d) distionary::pretty_name(d) == "Finite", FUN.VALUE = logical(1L)
  ))
  if (all_finite) {
    x <- lapply(dsts, \(d) distionary::parameters(d)[["outcomes"]])
    p <- lapply(dsts, \(d) distionary::parametres(d)[["probs"]])
    pnew <- Map(`*`, probs, p)
    l <- aggregate_weights(unlist(x), unlist(pnew))
    return(distionary::dst_empirical(l[["y"]], weights = l[["weight"]]))
  }
  ## END Special cases
  rm("weights", "preprocess") # Encl. env. makes it difficult to test equality
  na_probs <- is.na(probs)
  r <- lapply(dsts, range)
  r1 <- min(vapply(r, \(x) x[1], FUN.VALUE = numeric(1L)))
  r2 <- max(vapply(r, \(x) x[2], FUN.VALUE = numeric(1L)))
  var_type <- vapply(dsts, distionary::vtype, FUN.VALUE = character(1L))
  var_unique <- unique(var_type)
  if (length(var_unique) == 1) {
    v <- var_unique
  } else if ("unknown" %in% var_type) {
    v <- "unknown"
  } else {
    v <- "mixed"
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
    .vtype = v,
    .name = "Mixture",
    .parameters = list(
      distributions = dsts,
      probs = probs
    )
  )
  distionary:::new_distribution(d, class = "mixture")
}


#' @export
print.mixture <- function(x, ...) {
  cat("Mixture Distribution\n")
  cat("\nComponents: ")
  cat("\n")
  params <- distionary::parameters(x)
  params[["distributions"]] <- vapply(
    params[["distributions"]],
    distionary::pretty_name,
    FUN.VALUE = character(1L)
  )
  if (requireNamespace("tibble", quietly = TRUE)) {
    df <- tibble::as_tibble(params)
  } else {
    df <- as.data.frame(params)
  }
  print(df, ...)
}
