#' Convert Distributions in Ellipsis to Flat List
#'
#' Flattens distributions and lists of distributions placed in `...`
#' into a flat list. This is useful for dplyr-like workflows where distributions
#' are stored in a list-column in a tibble.
#'
#' @param ... Distribution objects, or list of distributions.
#' @returns A list of distributions contained in the `...`, with NULL
#' entries discarded. If no distributions are present, returns `list()`.
#' @details An error is thrown if `...` contains non-distributions.
#' This function is essentially a wrapper around `unlist()`.
#'
#' These work:
#'
#' dots_to_dsts(dst_exp(1), dst_norm(0, 1))
#' dots_to_dsts(list(dst_exp(1), dst_norm(0, 1)))
#' dots_to_dsts(dst_exp(1), list(dst_norm(0, 1), dst_pois(3)))
#'
#' This does not -- too many nested lists.
#'
#' dots_to_dsts(list(list(dst_exp(1), dst_norm(0, 1))))
#' dots_to_dsts(list(list(dst_exp(1)), dst_norm(0, 1)))
#' @noRd
dots_to_dsts <- function(...) {
  dsts <- rlang::list2(...)
  # Flatten while preserving distribution structure (i.e., without untangling
  # distributions that are lists themselves).
  # Note that `unlist()` untangles a distribution object if it's at the top
  # level, because it itself is a list. Wrap each distribution in a list
  # to prevent untangling.
  wrapped_dsts <- lapply(dsts, function(x) {
    if (distionary::is_distribution(x)) {
      x <- list(x)
    }
    x
  })
  dsts <- unlist(wrapped_dsts, recursive = FALSE)
  not_all_dsts <- !all(vapply(
    dsts, is_distribution, FUN.VALUE = logical(1L)
  ))
  if (not_all_dsts) {
    stop("Ellipsis must contain distributions only.")
  }
  dsts
}
