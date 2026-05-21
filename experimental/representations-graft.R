#' @export
eval_quantile.graft <- function(distribution, at) {
  p_cutoff <- distribution$components$probs[1L]
  res <- numeric(0L)
  for (i in seq_along(at)) {
    if (is.na(at[i])) {
      res[i] <- NA_real_
    } else if (at[i] <= p_cutoff) {
      new_at <- at[i] / p_cutoff
      this_d <- distribution$components$distributions[[1L]]
      res[i] <- distionary::eval_quantile(this_d, at = new_at)
    } else {
      new_at <- (at[i] - p_cutoff) / (1 - p_cutoff)
      this_d <- distribution$components$distributions[[2L]]
      res[i] <- distionary::eval_quantile(this_d, at = new_at)
    }
  }
  res
}
