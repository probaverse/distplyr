#' Max Value of Several Distributions
#'
#' For a collection of distributions, this function provides the
#' distribution of the maximum value from independent draws of
#' each component distribution.
#'
#' @inheritParams dots_to_dsts
#' @param draws Number of draws from each distribution considered in the
#' maximum. Either a single numeric applying to all distributions in `...`,
#' or a vector matching the number of distributions in `...`.
#' @return A distribution of class `"max"`.
#' @details To use precise language, if `X1`, ..., `Xp` are
#' `p` independent random variables corresponding to the distributions
#' in `...`, then the distribution returned is of `max(X1, ..., Xp)`.
#' Distributions and `draws` are recycled to the same length using
#' `vctrs::vec_recycle_common()`.
#' @rdname maximum
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
    realise = function(n) {
      iid_sample <- numeric(0L)
      for (i in seq_len(n)) {
        iid_sample_list <- Map(distionary::realise, dsts, draws)
        iid_sample[i] <- max(unlist(iid_sample_list))
      }
      iid_sample
    },
    range = r,
    .vtype = v,
    .name = "Maximum",
    .parameters = list(
      distributions = dsts,
      draws = draws
    )
  )
  distionary:::new_distribution(d, class = "maximum")
}

# Function to handle specific simplifications for maximize. For example.
# distributions entirely to the left of others get removed; if all finite
# distributions are input, then the result is also finite; etc.
#
# This is useful so that the variables used in these algorithms don't show up
# in the enclosing environment of the resulting distribution's representations,
# which makes it hard to do unit tests.
#
# Outputs a distribution if a simplification was possible, or a reduced list
# of dsts and draws otherwise.
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
    upper <- lapply(dsts, distionary::prob_right, of = x, inclusive = TRUE)
    lower <- lapply(dsts, distionary::prob_right, of = x, inclusive = FALSE)
    contributions_upper <- Map(`^`, upper, draws)
    contributions_lower <- Map(`^`, lower, draws)
    surv_upper <- Reduce(`*`, contributions_upper)
    surv_lower <- Reduce(`*`, contributions_lower)
    new_probs <- surv_upper - surv_lower
    return(distionary::dst_empirical(x, weights = new_probs))
  }
  list(
    dsts = dsts,
    draws = draws
  )
}

#' @rdname maximum
#' @export
maximise <- function(..., draws = 1) maximize(..., draws = draws)
