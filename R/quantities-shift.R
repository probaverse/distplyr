
#' @export
mean.shift <- function(x, ...) {
	with(x$components, {
		mean(distribution) + shift
	})
}

#' @export
median.shift <- function(x, ...) {
	with(x$components, {
		median(distribution) + shift
	})
}

#' @export
stdev.shift <- function(distribution) {
	with(distribution$components, {
		distionary::stdev(distribution)
	})
}

#' @export
range.shift <- function(distribution, ...) {
	with(distribution$components, {
		range(distribution) + shift
	})
}

#' @export
variance.shift <- function(distribution) {
	with(distribution$components, {
		distionary::variance(distribution)
	})
}

#' @export
skewness.shift <- function(distribution) {
	with(distribution$components, {
		distionary::skewness(distribution)
	})
}

#' @export
kurtosis_exc.shift <- function(distribution) {
	with(distribution$components, {
		distionary::kurtosis_exc(distribution)
	})
}
