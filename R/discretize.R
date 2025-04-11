#' Discretise a Distribution
#'
#' Bins a distribution by specified breakpoints. Left and right endpoints of
#' +/- infinity are implied.
#'
#' @param distribution Distribution to discretise.
#' @param breakpoints Vector of breakpoints separating each bin.
#' @param midpoints,values Specify the values associated with each new bin
#' by either specifying a computation of the `midpoint`, or as a vector
#' of `values` (of length one more than that of `breakpoints`) applied to the
#' bins from smallest to largest. If specified,
#' `values` will override `midpoint`.
#' @param closed Should the intervals be closed on the left or on the
#' right? One of `"right"` (default) or `"left"`.
#' @returns A finite distribution with the specified bins.
#' @note `NA` breakpoints and `values` (if specified) are silently removed.
#' Will throw an error if the lengths of `values` does not end up being
#' one more than that of `breakpoints`.
#' @examples
#' p1 <- dst_norm(0, 1)
#' p2 <- discretise(p1, breakpoints = -2:2)
#' p3 <- discretise(p1, breakpoints = -2:2, midpoints = "mean")
#' plot(p1, "cdf", from = -3, to = 3)
#' plot(p2, "cdf", from = -3, to = 3, add = TRUE, col = "blue", n = 1000)
#' plot(p3, "cdf", from = -3, to = 3, add = TRUE, col = "red", n = 1000)
#'
#' discretise(dst_exp(0.1), breakpoints = numeric())
#'
#' dst_norm(0, 1) |>
#'   slice_left(-2) |>
#'   slice_right(2) |>
#'   discretize(-2:2)
#' @rdname discretise
#' @export
discretise <- function(
    distribution,
    breakpoints,
    midpoints = c("median", "mean", "label"),
    values,
    closed = c("right", "left")
) {
  midpoints <- rlang::arg_match(midpoints)
  closed <- rlang::arg_match(closed)
  right_closed <- closed == "right"
  breakpoints <- sort(unique(breakpoints))
  n_break <- length(breakpoints)
  ## Probabilities
  cdf <- prob_left(distribution, breakpoints, inclusive = right_closed)
  cdf <- append(0, cdf)
  p <- diff(cdf)
  r <- prob_right(distribution, breakpoints[n_break], inclusive = !right_closed)
  p <- append(p, r)
  if (length(p) == 0) {
    p <- 1
  }

  ## Midpoints
  if (missing(values) && missing(midpoints)) {
    stop("Must specify either `values` or `midpoints` arguments.")
  } else if (!missing(values)) {
    values <- values[!is.na(values)]
    if (length(values) != length(breakpoints) + 1) {
      stop("`values` provided are of length ", length(values),
           ", but should have length ", length(breakpoints) + 1,
           " (one more than `breakpoints`).")
    }
  } else {
    if (length(breakpoints) == 0) {
      values <- rlang::exec(midpoints, distribution)
    } else {
      slices <- list(
        suppressWarnings(slice_right(
          distribution, breakpoints[1L], include = !right_closed
        ))
      )
      for (i in 1L + seq_len(n_break - 1L)) {
        d <- suppressWarnings(slice_right(
          distribution, breakpoints[i], include = !right_closed
        ))
        if (!is.null(d)) {
          d <- suppressWarnings(slice_left(
            d, breakpoints[i - 1L], include = right_closed
          ))
        }
        slices[[i]] <- d
      }
      slices[[n_break + 1L]] <- suppressWarnings(slice_left(
        distribution, breakpoints[n_break], include = right_closed
      ))
      values <- vapply(slices, function(dst) {
        if (is.null(dst)) {
          NA_real_
        } else {
          rlang::exec(midpoints, dst)
        }
      }, FUN.VALUE = numeric(1L))
    }
  }

  nonzero <- p > 0
  p <- p[nonzero]
  values <- values[nonzero]
  distionary::dst_empirical(values, weights = p)
}

#' @rdname discretise
#' @export
discretize <- discretise
