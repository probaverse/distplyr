#' @rdname linear_transform
#' @export
flip <- function(distribution) {
  if (distionary::vtype(distribution) != "continuous") {
    warning(
      "A non-continuous distribution has been entered into a distplyr verb.\n",
      "At this stage of distplyr's development, some inaccuracies can be\n",
      "expected in these cases, particularly for quantile calculations."
    )
  }
  if (pretty_name(distribution) == "Normal") {
    return(distionary::dst_norm(
      mean = -mean(distribution),
      sd = stdev(distribution)
    ))
  } else if (pretty_name(distribution) == "Uniform") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_unif(
      min = -p[["max"]],
      max = -p[["min"]]
    ))
  } else if (distionary::pretty_name(distribution) == "Cauchy") {
    p <- distionary::parameters(distribution)
    return(distionary::dst_cauchy(
      location = -p[["location"]],
      scale = p[["scale"]]
    ))
  }
  d <- distionary::distribution(
    cdf = function(x) {
      distionary::eval_pmf(distribution, at = -x) +
        distionary::eval_survival(distribution, at = -x)
    },
    survival = function(x) {
      distionary::eval_cdf(distribution, at = -x) -
        distionary::eval_pmf(distribution, at = -x)
    },
    pmf = function(x) {
      distionary::eval_pmf(distribution, at = -x)
    },
    density = function(x) {
      distionary::eval_density(distribution, at = -x)
    },
    quantile = function(x) {
      -distionary::eval_quantile(distribution, at = 1 - x)
    },
    realize = function(n) {
      -distionary::realize(distribution, n = n)
    },
    .vtype = distionary::vtype(distribution),
    .name = "Negated",
    .parameters = list(
      distribution = distribution
    ),
  )
  if (distionary:::is_intrinsic(distribution, "range")) {
    r <- range(distribution)
    d[["range"]] <- -rev(r)
  }
  if (distionary:::is_intrinsic(distribution, "mean")) {
    d[["mean"]] <- -mean(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "median")) {
    d[["median"]] <- -median(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "variance")) {
    d[["variance"]] <- distionary::variance(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "stdev")) {
    d[["stdev"]] <- distionary::stdev(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "skewness")) {
    d[["skewness"]] <- -distionary::skewness(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "kurtosis_exc")) {
    d[["kurtosis_exc"]] <- distionary::kurtosis_exc(distribution)
  }
  if (distionary:::is_intrinsic(distribution, "kurtosis")) {
    d[["kurtosis"]] <- distionary::kurtosis(distribution)
  }
  distionary::new_distribution(d, class = "negated")
}

