#' Convert Distributions in Ellipsis to List
#'
#' Flattens distributions placed in an ellipsis argument into a list,
#' so that the ellipsis can include distributions themselves and lists
#' of distributions.
#'
#' @param ... Distribution objects, or lists of distributions.
#' @param na.rm Logical; remove NA entries? Note that NULL entries are
#' always removed.
#' @return A list of distributions contained in the `...`, with NULL
#' entries discarded. If no distributions are present, returns `list()`.
#' @details An error is thrown if, after discarding NULL entries,
#' `...` contains non-distributions. This function is essentially a
#' wrapper around `rlang::flatten()`.
#' @examples
#' d <- dst_norm(0, 1)
#' distplyr:::dots_to_dsts(d, list(d, d), NULL)
dots_to_dsts <- function(..., na.rm = FALSE) {
  dsts <- rlang::list2(...)
  # Flatten while preserving distribution structure
  # Note that `unlist` untangles a distribution object if it's at the top
  # level, because it itself is a list. Wrap each distribution in a list
  # to prevent untangling.
  wrapped_dsts <- lapply(dsts, function(x) {
    if (distionary::is_distribution(x)) {
      x <- list(x)
    }
    x
  })
  dsts <- unlist(wrapped_dsts, recursive = FALSE)
  nulls <- vapply(dsts, is.null, FUN.VALUE = logical(1L))
  is_na <- function(x) length(x) == 1L && is.na(x)
  if (na.rm) {
    nulls <- nulls | vapply(dsts, is_na, FUN.VALUE = logical(1L))
    acceptable_entry <- is_distribution
  } else {
    acceptable_entry <- function(x) is_distribution(x) || is_na(x)
  }
  dsts <- dsts[!nulls]
  not_all_dsts <- !all(vapply(
    dsts, acceptable_entry, FUN.VALUE = logical(1L)
  ))
  if (not_all_dsts) {
    stop("Ellipsis must contain distributions only.")
  }
  dsts
}
