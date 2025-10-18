#' Linear Transformations
#'
#' `shift()` a distribution by adding a constant, or `multiply()` a
#' distribution by a constant. `flip()` is a specific case of multiplying
#' a distribution by `-1`, resulting in "flipping" the distribution about 0.
#'
#' @param distribution A probability distribution.
#' @param constant A single numeric by which to shift or multiply the
#' distribution by.
#' @return A distribution, shifted or multiplied by the constant.
#' Specifically, a distribution with subclass "shift", "scale", or "flip".
#' @note You can also use the binary operations `+`, `-`, `*`, and `/`
#' to access these transformations.
#' @details Specifically, if `X` is a random variable coming from a
#' distribution, then the resulting distributions are as follows:
#'
#' - For `shift()`, is the distribution of `X + constant`.
#' - For `multiply()`, is the distribution of `X * constant`.
#' - For `flip()`, is the distribution of `-X`.
#'
#' Although the `multiply()` function accepts negative constants,
#' the corresponding "scale" distribution class only holds positive
#' constants, delegating a potential negative sign to the "flip" class.
#' @seealso `invert()`
#' @examples
#' d_pois <- distionary::dst_pois(1.1)
#' d_norm <- distionary::dst_norm(4, 1)
#' d_unif <- distionary::dst_unif(0, 1)
#'
#' # Shift a Poisson distribution by 1.
#' shift(d_pois, 1)
#' d_pois + 1
#'
#' # Multiply a Uniform distribution by 2.
#' multiply(d_unif, 2)
#' d_unif * 2
#'
#' # Flip a Normal distribution.
#' flip(d_norm)
#' -d_norm
#'
#' # Combine multiple operations:
#' 4 - 2 * d_pois
#' @rdname linear_transform
#' @export
shift <- function(distribution, constant) {
  if (constant == 0) {
    return(distribution)
  }
  nm <- distionary::pretty_name(distribution)
  if (nm == "Null") {
    return(distribution)
  }
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
  }
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


