#' @rdname linear_transform
#' @export
multiply <- function(distribution, constant) {
  if (constant < 0) {
    return(flip(multiply(distribution, -constant)))
  } else if (constant == 0) {
  	return(distionary::dst_degenerate(0))
  } else if (constant == 1) {
    return(distribution)
  } else if (is.infinite(constant)) {
    stop("Cannot multiply a distribution by infinity.")
  }
  d <- distionary::distribution(
    cdf = function(x) {
      distionary::eval_cdf(distribution, at = x / constant)
    },
    survival = function(x) {
      distionary::eval_survival(distribution, at = x / constant)
    }
    quantile = function(p) {
      distionary::eval_quantile(distribution, at = p) * constant
    },
    pmf = function(x) {
      distionary::eval_pmf(distribution, at = x / constant)
    },
    density = function(x) {
      distionary::eval_density(distribution, at = x / constant) / constant
    },
    realize = function(n) {
      distionary::realise(distribution, n = n) * constant
    },
    .vtype = distionary::vtype(distribution),
    .name = "Scaled",
    .parameters = list(
      distribution = distribution,
      constant = constant
    )
  )
  if (distionary:::is_intrinsic(distribution, "mean")) {
    d[["mean"]] <- mean(distribution) * constant
  }
  if (distionary:::is_intrinsic(distribution, "median")) {
    d[["median"]] <- median(distribution) * constant
  }
  if (distionary:::is_intrinsic(distribution, "stdev")) {
    d[["stdev"]] <- distionary::stdev(distribution) * constant
  }
  if (distionary:::is_intrinsic(distribution, "variance")) {
    d[["variance"]] <- distionary::variance(distribution) * constant^2
  }
  if (distionary:::is_intrinsic(distribution, "skewness")) {
    d[["skewness"]] <- distionary::skewness(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "kurtosis_exc")) {
    d[["kurtosis_exc"]] <- distionary::kurtosis_exc(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "kurtosis_exc")) {
    d[["kurtosis"]] <- distionary::kurtosis(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "range")) {
    d[["range"]] <- range(distribution) * constant
  }
  distionary::new_distribution(d, class = "scaled")
}





















































































