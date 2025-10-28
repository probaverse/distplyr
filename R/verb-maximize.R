#' Extremum of Several Distributions
#'
#' @description
#' For a collection of distributions, obtain the distributions of the
#' maximum (`maximize()`) and minimum (`minimize()`) from independent draws of
#' each component distribution.
#'
#' Aliases `maximise()` and `minimise()` are also provided.
#'
#' @param ... Distribution objects, or list of distributions.
#' @param draws Number of draws from each distribution considered in the
#' maximum (possibly not integer, but never negative).
#' Either a single numeric applying to all distributions in `...`,
#' or a vector matching the number of distributions in `...`.
#' @param na_action_dst,na_action_draws What should be done with Null
#' distributions in `...` and `NA` in `draws`?
#' Character vector of length 1:
#' one of "fail", "null" (default), or "drop". See details.
#' @return A distribution of class `"max"`.
#' @details
#' To give an example of what distribution is returned, if X1 and X2 are
#' two random variables with distributions D1 and D2 respectively, then
#' `maximize(D1, D2, draws = c(2, 3))` returns the distribution of
#' `max(X1, X1, X2, X2, X2)`.
#'
#' Distributions in `...` and the `draws` vector are recycled to have the
#' same length, but only if one of them has length 1
#' (via `vctrs::vec_recycle_common()`).
#'
#' `na_action_dst` and `na_action_draws` specify the NA action for distributions
#' and `draws`. "NA" here means either `NA` in the `draws` vector, or
#' a Null distribution (`distionary::dst_null()`) in the distributions.
#' Options are, in order of precedence:
#'
#' - `"fail"`: Throw an error in the presence of NAs.
#' - `"null"`: Return a Null distribution in the presence of NAs.
#' - `"drop"`: Remove distribution-weight pairs having an NA value
#'
#' Simplifications made in these functions include the following:
#'
#' - If any distributions are entirely to the left (right) of others,
#'   then they are removed from consideration in `maximize()` (`minimize()`).
#' - If all Finite distributions are input, the result is also a Finite
#'   distribution.
#' - If the same distribution is input multiple times, their corresponding
#'   draws are summed.
#' @examples
#' # One is always more extreme than the other in this case.
#' d1 <- dst_unif(-1, 2)
#' d2 <- dst_unif(5, 6)
#' maximize(d1, d2) # d2
#' minimize(d1, d2) # d1
#'
#' # Visualizing the maximum and minimum
#' d3 <- dst_norm(4, 1)
#' d4 <- dst_exp(0.3)
#'
#' dmax <- maximize(d3, d4, draws = 1:2)
#' dmin <- minimize(d3, d4, draws = 1:2)
#'
#' # Maximum
#' plot(d3, col = "blue", lty = 2, from = 0, to = 14)
#' plot(d4, col = "red", lty = 2, add = TRUE)
#' plot(dmax, add = TRUE, n = 1000)
#' legend(
#'  "topright",
#'  legend = c("Maximum", "N(4,1)", "Exp(0.3)"),
#'  col = c("black", "blue", "red"),
#'  lty = c(1, 2, 2)
#' )
#'
#' # Minimum
#' plot(d3, col = "blue", lty = 2, from = 0, to = 10)
#' plot(d4, col = "red", lty = 2, add = TRUE)
#' plot(dmin, add = TRUE, n = 1000)
#' legend(
#'   "topright",
#'   legend = c("Minimum", "N(4,1)", "Exp(0.3)"),
#'   col = c("black", "blue", "red"),
#'   lty = c(1, 2, 2)
#' )
#'
#' @rdname extremum
#' @export
maximize <- function(...,
                     draws = 1,
                     na_action_dst = c("null", "drop", "fail"),
                     na_action_draws = c("null", "drop", "fail")) {
  preprocess <- pair_dots_num(
    ...,
    num = draws,
    na_action_dst = na_action_dst,
    na_action_num = na_action_draws
  )
  if (distionary::is_distribution(preprocess)) {
    return(preprocess)
  }
  simplification <- maximize_simplifications(
    dsts = preprocess$dsts,
    draws = preprocess$num
  )
  rm("preprocess")
  if (distionary::is_distribution(simplification)) {
    return(simplification)
  }
  dsts <- simplification$dsts
  draws <- simplification$draws
  ## Variable type determination
  vars <- unique(unlist(lapply(dsts, distionary::vtype)))
  if (length(vars) == 1) {
    v <- vars
  } else if (identical(sort(vars), c("continuous", "discrete"))) {
    v <- "mixed"
  } else {
    # Would need to dig more to know for sure.
    v <- "unknown"
  }
  r <- lapply(dsts, range)
  r <- Reduce(pmax, r)
  cdf <- function(x) {
    prob_lefts <- lapply(dsts, distionary::eval_cdf, at = x)
    contributions <- Map(`^`, prob_lefts, draws)
    Reduce(`*`, contributions)
  }
  d <- distionary::distribution(
    cdf = cdf,
    density = function(x) {
      # formula: cdf * (sum draws_j f_j / F_j)
      full_cdf <- cdf(x)
      cdfs <- lapply(dsts, distionary::eval_cdf, at = x)
      pdfs <- lapply(dsts, distionary::eval_density, at = x)
      divide_if_nonzero <- function(draws, pdf, cdf) {
        res <- draws * pdf / cdf
        res[pdf == 0] <- 0
        res
      }
      ratios <- Map(divide_if_nonzero, draws, pdfs, cdfs)
      ratios_sum <- Reduce(`+`, ratios)
      ratios_sum * full_cdf
    },
    range = r,
    .vtype = v,
    .name = "Maximum",
    .parameters = list(
      distributions = dsts,
      draws = draws
    )
  )
  if (all(draws %% 1 == 0)) {
    # All draws are integers.
    d[["realise"]] <- function(n) {
      iid_sample <- numeric(0L)
      for (i in seq_len(n)) {
        iid_sample_list <- Map(distionary::realise, dsts, draws)
        iid_sample[i] <- max(unlist(iid_sample_list))
      }
      iid_sample
    }
  }
  distionary:::new_distribution(d, class = "maximum")
}

#' Function to handle specific simplifications for maximize. For example.
#' distributions entirely to the left of others get removed; if all finite
#' distributions are input, then the result is also finite; etc.
#'
#' This is useful so that the variables used in these algorithms don't show up
#' in the enclosing environment of the resulting distribution's representations,
#' which makes it hard to do unit tests.
#'
#' Outputs a distribution if a simplification was possible, or a reduced list
#' of dsts and draws otherwise.
#' @noRd
maximize_simplifications <- function(dsts, draws) {
  ## The largest minimum becomes the new overall minimum.
  r <- lapply(dsts, range)
  mins <- vapply(r, function(r_) r_[1L], FUN.VALUE = numeric(1L))
  maxs <- vapply(r, function(r_) r_[2L], FUN.VALUE = numeric(1L))
  new_min <- max(mins)
  # All distributions with max less than or equal to the new min go away,
  # except if one of those distributions happens to be degenerate AND no
  # other distributions have a min at the new min (then that degenerate
  # distribution stays and becomes the maximum).
  if (any(maxs <= new_min)) {
    leading_dsts <- dsts[mins == new_min]
    leading_degen <- vapply(
      leading_dsts,
      \(d) distionary::eval_pmf(d, at = new_min) == 1,
      FUN.VALUE = logical(1L)
    )
    if (all(leading_degen)) {
      return(distionary::dst_degenerate(new_min))
    }
    cull <- maxs <= new_min
    dsts <- dsts[!cull]
    draws <- draws[!cull]
  }
  if (length(draws) == 1 && draws == 1) {
    return(dsts[[1]])
  }
  ## Simplification based on distribution type
  all_finite <- all(vapply(
    dsts, \(d) distionary::pretty_name(d) == "Finite", FUN.VALUE = logical(1L)
  ))
  if (all_finite) {
    x <- lapply(dsts, \(d) distionary::parameters(d)[["outcomes"]])
    x <- unique(unlist(x))
    upper <- lapply(dsts, distionary::prob_left, of = x, inclusive = TRUE)
    lower <- lapply(dsts, distionary::prob_left, of = x, inclusive = FALSE)
    contributions_upper <- Map(`^`, upper, draws)
    contributions_lower <- Map(`^`, lower, draws)
    cdf_upper <- Reduce(`*`, contributions_upper)
    cdf_lower <- Reduce(`*`, contributions_lower)
    new_probs <- cdf_upper - cdf_lower
    return(distionary::dst_empirical(x, weights = new_probs))
  }
  list(
    dsts = dsts,
    draws = draws
  )
}

#' @rdname extremum
#' @usage NULL
#' @export
maximise <- function(...,
                     draws = 1,
                     na_action_dst = c("null", "drop", "fail"),
                     na_action_draws = c("null", "drop", "fail")) {
  maximize(
    ...,
    draws = draws,
    na_action_dst = na_action_dst,
    na_action_draws = na_action_draws
  )
}
