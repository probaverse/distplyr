#' Mixture Distributions
#'
#' Create a mixture distribution, which can be thought of as an average of
#' multiple distributions (in terms of their CDF, density, PMF, or survival
#' functions, for example).
#' Data drawn from a mixture distribution involves
#' two steps: first randomly selecting the distribution to draw from, followed
#' by the random selection from that distribution.
#'
#' @param ... Distribution objects, or list of distributions.
#' @param weights Vector of weights corresponding to the distributions;
#' or, single numeric for equal weights. When normalized, they correspond to
#' the probabilities of selecting each distribution.
#' @param na_action_dst,na_action_w What should be done with null
#' distributions in `...` and `NA` in `weights`?
#' Character vector of length 1:
#' one of "fail", "null" (default), or "drop". See details.
#' @details
#' Distributions in `...` and the `weights` vector are recycled to have the
#' same length, but only if one of them has length 1
#' (via `vctrs::vec_recycle_common()`).
#'
#' `na_action_dst` and `na_action_w` specify the NA action for distributions
#' and weights. "NA" here means either `NA` in the `weights` vector, or
#' a Null distribution (`distionary::dst_null()`) in the distributions.
#' Options are, in order of precedence:
#'
#' - `"fail"`: Throw an error in the presence of NAs.
#' - `"null"`: Return a Null distribution in the presence of NAs.
#' - `"drop"`: Remove distribution-weight pairs having an NA value
#'
#' @returns A mixture distribution.
#' @examples
#' library(distionary)
#' a <- dst_norm(0, 1)
#' b <- dst_norm(5, 2)
#' m1 <- mix(a, b, weights = c(1, 4))
#' plot(a, col = "red", lty = 2, from = -3, to = 11)
#' plot(b, add = TRUE, col = "blue", lty = 2)
#' plot(m1, add = TRUE)
#' legend(
#'   "topright",
#'    legend = c("Mixture", "N(0,1)", "N(5,2)"),
#'    col = c("black", "red", "blue"),
#'    lty = c(1, 2, 2)
#' )
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
  ## BEGIN special simplifications ---------------------------------------------
  all_finite <- all(vapply(
    dsts,
    function(d) distionary::pretty_name(d) == "Finite",
    FUN.VALUE = logical(1L)
  ))
  if (all_finite) {
    x <- lapply(dsts, function(d) distionary::parameters(d)[["outcomes"]])
    p <- lapply(dsts, function(d) distionary::parameters(d)[["probs"]])
    pnew <- Map(`*`, probs, p)
    l <- distionary:::aggregate_weights(unlist(x), unlist(pnew))
    return(distionary::dst_empirical(l[["y"]], weights = l[["weight"]]))
  }
  ## END special simplifications -----------------------------------------------
  rm("weights", "preprocess") # Encl. env. makes it difficult to test equality
  r <- lapply(dsts, range)
  r1 <- min(vapply(r, function(x) x[1], FUN.VALUE = numeric(1L)))
  r2 <- max(vapply(r, function(x) x[2], FUN.VALUE = numeric(1L)))
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
        function(i) distionary::realise(dsts[[i]], n = 1),
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
