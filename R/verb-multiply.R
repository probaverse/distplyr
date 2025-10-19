#' @rdname linear_transform
#' @export
multiply <- function(distribution, constant) {
  checkmate::assert_number(constant, finite = TRUE, na.ok = TRUE)
  checkmate::assert_class(distribution, "dst")
  if (is.na(constant)) {
    return(distionary::dst_null())
  }
  if (constant < 0) {
    return(flip(multiply(distribution, -constant)))
  } else if (constant == 0) {
  	return(distionary::dst_degenerate(0))
  } else if (constant == 1) {
    return(distribution)
  } else if (is.infinite(constant)) {
    stop("Cannot multiply a distribution by infinity.")
  }
  nm <- distionary::pretty_name(distribution)
  if (nm == "Null") {
    return(distribution)
  }
  ## BEGIN special simplifications ---------------------------------------------
  if (nm == "Normal") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_norm(
      mean = p[["mean"]] * constant,
      sd = p[["sd"]] * constant
    ))
  } else if (nm == "Uniform") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_unif(
      min = p[["min"]] * constant,
      max = p[["max"]] * constant
    ))
  } else if (nm == "Cauchy") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_cauchy(
      location = p[["location"]] * constant,
      scale = p[["scale"]] * abs(constant)
    ))
  } else if (nm == "Generalised Pareto") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_gp(
      scale = p[["scale"]] * constant,
      shape = p[["shape"]]
    ))
  } else if (nm == "Generalised Extreme Value") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_gev(
      location = p[["location"]] * constant,
      scale = p[["scale"]] * constant,
      shape = p[["shape"]]
    ))
  } else if (nm == "Finite") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_empirical(
      p[["outcomes"]] * constant, weights = p[["probs"]]
    ))
  } else if (nm == "Degenerate") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_degenerate(p[["location"]] * constant))
  }
  ## END special simplifications -----------------------------------------------
  d <- distionary::distribution(
    cdf = function(x) {
      distionary::eval_cdf(distribution, at = x / constant)
    },
    survival = function(x) {
      distionary::eval_survival(distribution, at = x / constant)
    },
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
  distionary:::new_distribution(d, class = "scaled")
}





















































































