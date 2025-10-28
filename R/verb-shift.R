#' Linear and Reciprocal Transformations
#'
#' Transform distributions using location shifts, scaling, negation, and
#' reciprocals. If `X` is a random variable following a distribution, these
#' functions return the distribution of the transformed variable.
#'
#' @param distribution A probability distribution.
#' @param constant A numeric value for shifting or scaling.
#'
#' @section Functions vs Operators:
#' These transformations can be applied using named functions or arithmetic
#' operators:
#' - `shift(d, a)` or `d + a` - Returns distribution of `X + a`
#' - `multiply(d, a)` or `d * a` - Returns distribution of `X * a`
#' - `flip(d)` or `-d` - Returns distribution of `-X`
#' - `invert(d)` or `1 / d` - Returns distribution of `1 / X`
#'
#' For complete documentation of operator usage, see [Ops.dst()].
#'
#' @section Special Cases:
#' **Negation in multiplication**: When `multiply()` receives a negative
#' constant, it internally calls `flip()` on the result of multiplying by
#' the absolute value.
#'
#' **Inversion constraint**: `invert()` requires that the distribution has no
#' mass at zero (i.e., `P(X = 0) = 0`). An error is returned if this
#' condition is violated.
#'
#' @section Simplifications:
#' These functions apply automatic simplifications when possible. For example:
#' - Shifting a Normal distribution returns another Normal distribution
#' - Multiplying a Uniform distribution returns another Uniform distribution
#' - Flipping a symmetric distribution may preserve its form
#'
#' More simplifications will be added in future versions of distplyr.
#'
#' @return A transformed distribution.
#'
#' @examples
#' d_pois <- distionary::dst_pois(1.1)
#' d_norm <- distionary::dst_norm(4, 1)
#' d_unif <- distionary::dst_unif(0, 1)
#'
#' # Shifting
#' shift(d_pois, 1)
#' d_pois + 1           # Equivalent using operator
#'
#' # Scaling
#' multiply(d_unif, 2)
#' d_unif * 2           # Equivalent using operator
#'
#' # Negation
#' flip(d_norm)
#' -d_norm              # Equivalent using operator
#'
#' # Inversion
#' d_positive <- distionary::dst_unif(1, 2)
#' invert(d_positive)
#' 1 / d_positive       # Equivalent using operator
#'
#' # Combine multiple operations
#' 4 - 2 * d_pois
#' multiply(flip(multiply(d_pois, 2)), -1) + 4  # Equivalent
#'
#' @seealso [Ops.dst()] for arithmetic operators including `+`, `-`, `*`, `/`.
#' @rdname linear_transform
#' @export
shift <- function(distribution, constant) {
  checkmate::assert_number(constant, finite = TRUE, na.ok = TRUE)
  checkmate::assert_class(distribution, "dst")
  if (is.na(constant)) {
    return(distionary::dst_null())
  }
  if (constant == 0) {
    return(distribution)
  }
  nm <- distionary::pretty_name(distribution)
  if (nm == "Null") {
    return(distribution)
  }
  ## BEGIN special simplifications ---------------------------------------------
  if (nm == "Normal") {
    return(distionary::dst_norm(
      mean = mean(distribution) + constant,
      sd = distionary::stdev(distribution)
    ))
  } else if (nm == "Uniform") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_unif(
      min = p[["min"]] + constant,
      max = p[["max"]] + constant
    ))
  } else if (nm == "Cauchy") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_cauchy(
      location = p[["location"]] + constant,
      scale = p[["scale"]]
    ))
  } else if (nm == "Generalised Extreme Value") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_gev(
      location = p[["location"]] + constant,
      scale = p[["scale"]],
      shape = p[["shape"]]
    ))
  } else if (nm == "Finite") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_empirical(
      p[["outcomes"]] + constant, weights = p[["probs"]]
    ))
  } else if (nm == "Degenerate") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_degenerate(p[["location"]] + constant))
  } else if (nm == "Shifted") {
    p <- distionary::parameters(distribution)
    prev_const <- p[["shift"]]
    base_dist <- p[["distribution"]]
    return(shift(base_dist, constant + prev_const))
  }
  ## END special simplifications -----------------------------------------------
  d <- distionary::distribution(
    cdf = function(x) {
      distionary::eval_cdf(distribution, at = x - constant)
    },
    quantile = function(p) {
      distionary::eval_quantile(distribution, at = p) + constant
    },
    pmf = function(x) {
      distionary::eval_pmf(distribution, at = x - constant)
    },
    density = function(x) {
      distionary::eval_density(distribution, at = x - constant)
    },
    survival = function(x) {
      distionary::eval_survival(distribution, at = x - constant)
    },
    realize = function(n) {
      distionary::realize(distribution, n = n) + constant
    },
    .vtype = distionary::vtype(distribution),
    .name = "Shifted",
    .parameters = list(
      distribution = distribution,
      shift = constant
    )
  )
  if (distionary:::is_intrinsic(distribution, "mean")) {
    d[["mean"]] <- mean(distribution) + constant
  }
  if (distionary:::is_intrinsic(distribution, "median")) {
    d[["median"]] <- median(distribution) + constant
  }
  if (distionary:::is_intrinsic(distribution, "stdev")) {
    d[["stdev"]] <- distionary::stdev(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "range")) {
    d[["range"]] <- range(distribution) + constant
  }
  if (distionary:::is_intrinsic(distribution, "variance")) {
    d[["variance"]] <- distionary::variance(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "skewness")) {
    d[["skewness"]] <- distionary::skewness(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "kurtosis_exc")) {
    d[["kurtosis_exc"]] <- distionary::kurtosis_exc(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "kurtosis")) {
    d[["kurtosis"]] <- distionary::kurtosis(distribution)
  }
  distionary:::new_distribution(d, class = "shifted")
}


