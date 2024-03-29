#' @export
mean.negative <- function(x, ...) {
	with(x, {
		-mean(distribution)
	})
}

#' @export
median.negative <- function(x, ...) {
	with(x, {
		-median(distribution)
	})
}

#' @export
variance.negative <- function(distribution, ...) {
	with(distribution, {
		distionary::variance(distribution)
	})
}

#' @export
stdev.negative <- function(distribution, ...) {
	with(distribution, {
		distionary::stdev(distribution)
	})
}

#' @export
skewness.negative <- function(distribution, ...) {
	with(distribution, {
		-distionary::skewness(distribution)
	})
}

#' @export
kurtosis_exc.negative <- function(distribution, ...) {
	with(distribution, {
		distionary::kurtosis_exc(distribution)
	})
}

#' @export
range.negative <- function(distribution, ...) {
	with(distribution, {
		d <- range(distribution)
		-rev(d)
	})
}
