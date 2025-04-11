#' @export
mean.scale <- function(x, ...) {
	ellipsis::check_dots_empty()
	with(x$components, {
		mean(distribution) * scale
	})
}

#' @export
median.scale <- function(x, ...) {
	with(x$components, {
		median(distribution) * scale
	})
}

#' @export
stdev.scale <- function(distribution) {
	with(distribution$components, {
		distionary::stdev(distribution) * scale
	})
}

#' @export
variance.scale <- function(distribution) {
	with(distribution$components, {
		distionary::variance(distribution) * scale^2
	})
}

#' @export
skewness.scale <- function(distribution) {
	with(distribution$components, {
		distionary::skewness(distribution)
	})
}

#' @export
kurtosis_exc.scale <- function(distribution) {
	with(distribution$components, {
		distionary::kurtosis_exc(distribution)
	})
}

#' @export
range.scale <- function(distribution, ...) {
	with(distribution$components, {
		range(distribution) * scale
	})
}
