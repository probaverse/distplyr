#' @export
Math.dst <- function(e1, ...) {
  op <- .Generic[[1]]
  switch(op,
    `log` = {
      log_distribution(e1, ...)
    },
    `log10` = {
      ellipsis::check_dots_empty()
      log_distribution(e1, base = 10)
    },
    `exp` = {
      ellipsis::check_dots_empty()
      exp_distribution(e1)
    },
    stop("Operation currently not supported.")
  )
}
