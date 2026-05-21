#' @rdname extremum
#' @export
minimize <- function(...,
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
  simplification <- minimize_simplifications(
    dsts = preprocess$dsts,
    draws = preprocess$num
  )
  rm("preprocess")
  if (distionary::is_distribution(simplification)) {
    return(simplification)
  }
  dsts <- simplification$dsts
  draws <- simplification$draws
  # Variable type determination
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
  r <- Reduce(pmin, r)
  survival <- function(x) {
    prob_rights <- lapply(dsts, distionary::eval_survival, at = x)
    contributions <- Map(`^`, prob_rights, draws)
    Reduce(`*`, contributions)
  }
  d <- distionary::distribution(
    cdf = function(x) 1 - survival(x),
    survival = survival,
    density = function(x) {
      # formula: survival * (sum draws_j f_j / surv_j)
      full_surv <- survival(x)
      survs <- lapply(dsts, distionary::eval_survival, at = x)
      pdfs <- lapply(dsts, distionary::eval_density, at = x)
      divide_if_nonzero <- function(draws, pdf, cdf) {
        res <- draws * pdf / cdf
        res[pdf == 0] <- 0
        res
      }
      ratios <- Map(divide_if_nonzero, draws, pdfs, survs)
      ratios_sum <- Reduce(`+`, ratios)
      ratios_sum * full_surv
    },
    range = r,
    .name = "Minumum",
    .vtype = v,
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
        iid_sample_list <- Map(realise, dsts, draws)
        iid_sample[i] <- min(unlist(iid_sample_list))
      }
      iid_sample
    }
  }
  distionary:::new_distribution(d, class = "minimum")
}

# Function to handle specific simplifications for minimize. For example.
# distributions entirely to the right of others get removed; if all finite
# distributions are input, then the result is also finite; etc.
#
# This is useful so that the variables used in these algorithms don't show up
# in the enclosing environment of the resulting distribution's representations,
# which makes it hard to do unit tests.
#
# Outputs a distribution if a simplification was possible, or a reduced list
# of dsts and draws otherwise.
minimize_simplifications <- function(dsts, draws) {
  ## The smallest maximum becomes the new overall maximum.
  r <- lapply(dsts, range)
  mins <- vapply(r, function(r_) r_[1L], FUN.VALUE = numeric(1L))
  maxs <- vapply(r, function(r_) r_[2L], FUN.VALUE = numeric(1L))
  new_max <- min(maxs)
  # All distributions with max less than or equal to the new min go away,
  # except if one of those distributions happens to be degenerate AND no
  # other distributions have a min at the new min (then that degenerate
  # distribution stays and becomes the maximum).
  if (any(mins >= new_max)) {
    leading_dsts <- dsts[maxs == new_max]
    leading_degen <- vapply(
      leading_dsts,
      function(d) distionary::eval_pmf(d, at = new_max) == 1,
      FUN.VALUE = logical(1L)
    )
    if (all(leading_degen)) {
      return(distionary::dst_degenerate(new_max))
    }
    cull <- mins >= new_max
    dsts <- dsts[!cull]
    draws <- draws[!cull]
  }
  if (length(draws) == 1 && draws == 1) {
    return(dsts[[1]])
  }
  ## Simplification based on distribution type
  all_finite <- all(vapply(
    dsts,
    function(d) distionary::pretty_name(d) == "Finite",
    FUN.VALUE = logical(1L)
  ))
  if (all_finite) {
    x <- lapply(dsts, function(d) distionary::parameters(d)[["outcomes"]])
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

#' @rdname extremum
#' @usage NULL
#' @export
minimise <- function(...,
                     draws = 1,
                     na_action_dst = c("null", "drop", "fail"),
                     na_action_draws = c("null", "drop", "fail")) {
  minimize(
    ...,
    draws = draws,
    na_action_dst = na_action_dst,
    na_action_draws = na_action_draws
  )
}

