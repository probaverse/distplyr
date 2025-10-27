#' Linear Transformations
#'
#' Change the location (`shift()`) and scale (`multiply()`) of a distribution.
#' The `flip()` verb is also available for negating a distribution, and is
#' called internally when `multiply()` receives a negative constant. These
#' transformations can also be accessed by the binary operators `+`, `-`,
#' and `*`.
#'
#' @param distribution A probability distribution of class `"dst"`.
#' @param constant A single numeric by which to shift or multiply the
#' distribution by.
#' @return A distribution, shifted or multiplied by the constant.
#' Specifically, a distribution with subclass "shift", "scale", or "flip".
#' @details If `X` is a random variable,
#' then the transformations correspond to the following:
#'
#' - `shift()` provides the distribution of `X + constant`.
#' - `multiply()` provides the distribution of `X * constant`.
#' - `flip()` provides the distribution of `-X`.
#'
#' Simplifications are made in some situations, and will improve with future
#' versions of distplyr.
#' @seealso [invert()] for division by a distribution.
#' @examples
#' d_pois <- distionary::dst_pois(1.1)
#' d_norm <- distionary::dst_norm(4, 1)
#' d_unif <- distionary::dst_unif(0, 1)
#'
#' # Shift a Poisson distribution by 1.
#' shift(d_pois, 1)
#' d_pois + 1
#'
#' # Multiply a Uniform distribution by 2: simplifies to a Uniform distribution.
#' multiply(d_unif, 2)
#' d_unif * 2
#'
#' # Flip a Normal distribution: simplifies to another Normal distribution.
#' flip(d_norm)
#' -d_norm
#'
#' # Combine multiple operations:
#' 4 - 2 * d_pois
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


