#' Condition a multivariate distribution on known values
#'
#' The distribution of some of a multivariate distribution's variables,
#' when the others are known to equal given values.
#'
#' @param distribution A distribution of several variables.
#' @param given The known values, named after their variables: a named
#' numeric vector, such as `c(x = 3)`, or a named list.
#' @details
#' The result is a distribution of the variables not in `given`, in their
#' original order: univariate if only one is left. It is what the `given`
#' argument of `distionary::eval_mv_cdf()` and the like evaluates, but as a
#' distribution in its own right, to be evaluated, drawn from, or
#' transformed further.
#'
#' This conditions on variables *equalling* values. To condition a single
#' variable on an event, landing above or below a value, see [trim_left()]
#' and [trim_right()].
#'
#' # Slices
#'
#' Conditioning on a variable that is a function of others slices the
#' distribution. To slice a distribution of \eqn{(X, Y)} along the line
#' \eqn{X + Y = s}, include \eqn{S = X + Y} as a third variable and
#' condition on it. The result is still a distribution of \eqn{X} and
#' \eqn{Y}, but it lives on a line, so its variable type is `"singular"`
#' and it has no density over the plane; take a
#' `distionary::marginal()` of one of them for one that does. For the
#' multivariate Normal and t this is exact, since the three variables are
#' again Normal (or t) with a singular covariance.
#'
#' # How it is found
#'
#' A distribution can state its own conditionals, as
#' `distionary::dst_mv_norm()` and `distionary::dst_mv_t()` do. Otherwise
#' distionary works one out: for a distribution on finitely many points, by
#' keeping the points that match `given`; for a continuous one, by dividing
#' densities.
#'
#' Conditioning on values that cannot occur, or on an `NA`, gives the Null
#' distribution.
#' @returns A distribution: univariate if one variable is left over.
#' @examples
#' d <- distionary::dst_bi_norm(mean = c(0, 1), sd = c(1, 2), cor = 0.6)
#' conditional(d, given = c(x = 1))
#'
#' # Slice two flows along their total.
#' a <- rbind(r1 = c(1, 0), r2 = c(0, 1), s = c(1, 1))
#' trio <- distionary::dst_mv_norm(
#'   mean = stats::setNames(as.numeric(a %*% c(50, 80)), rownames(a)),
#'   cov = a %*% matrix(c(100, 60, 60, 225), 2) %*% t(a)
#' )
#' slice <- conditional(trio, given = c(s = 200))
#' slice
#' distionary::marginal(slice, "r1")
#' @export
conditional <- function(distribution, given) {
  checkmate::assert_class(distribution, "dst")
  p <- distionary::dimension(distribution)
  if (is.na(p) || p == 1L) {
    stop("A distribution of one variable has nothing to condition on.")
  }
  known <- is.numeric(given) || is.list(given) ||
    (is.logical(given) && all(is.na(given)))
  if (!known || length(given) == 0) {
    stop(
      "`given` must be a named vector of known values,\n",
      "as in `c(x = 3)`."
    )
  }
  nms <- rlang::names2(given)
  if (any(nms == "")) {
    stop(
      "Name each value in `given` after its variable,\n",
      "as in `c(x = 3)`."
    )
  }
  if (is.list(given) && any(lengths(given) != 1L)) {
    stop("Each value in `given` must be a single number.")
  }
  vars <- distionary::variables(distribution)
  idx <- match(nms, vars)
  if (anyNA(idx)) {
    stop(
      "`given` names a variable `", nms[is.na(idx)][[1L]],
      "` that the\ndistribution does not have. Its variables are ",
      paste0("`", vars, "`", collapse = ", "), "."
    )
  }
  if (anyDuplicated(idx)) {
    stop("`given` names the same variable twice.")
  }
  if (length(idx) == p) {
    stop(
      "Every variable is `given`, which leaves nothing to describe.\n",
      "Leave at least one variable out of `given`."
    )
  }
  at <- as.numeric(unlist(given, use.names = FALSE))
  if (anyNA(at)) {
    return(distionary::dst_null())
  }
  distionary::eval_property(distribution, "conditional", idx, at)
}
