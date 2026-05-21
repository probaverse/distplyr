#' @export
range.slice_right <- function(distribution, ...) {
  # Not accurate when slicing between discrete points.
  d <- distribution$distribution
  breakpoint <- distribution$breakpoint

  r <- range(distribution$distribution)
  r[2L] <- distribution$breakpoint

  # slope_of_cdf <- eval_density(d, at = breakpoint, strict = FALSE)
  # if (slope_of_cdf == 0) {
  #   value_of_cdf <- eval_cdf(d, at = breakpoint)
  #   breakpoint_new <- eval_quantile(d, at = value_of_cdf)
  #   if (breakpoint_new != breakpoint) {
  #     breakpoint <- breakpoint_new
  #     include <- FALSE
  #   }
  # }

  r
}
