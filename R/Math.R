#' @export
Math.dst <- function(x, ...) {
  op <- .Generic[[1]]
  switch(op,
    `log` = {
      log_distribution(x, ...)
    },
    `log10` = {
      ellipsis::check_dots_empty()
      log_distribution(x, base = 10)
    },
    `exp` = {
      ellipsis::check_dots_empty()
      exp_distribution(x)
    },
    stop("Operation currently not supported.")
  )
}
