#' @export
#' @inheritParams next_discrete
next_discrete.scale <- function(distribution, from, n = 1L,
								include_from = FALSE, ...) {
	with(distribution$components, {
		distionary::next_discrete(
			distribution, from = from, n = n, include_from = include_from
		) * scale
	})
}

#' @export
#' @inheritParams next_discrete
prev_discrete.scale <- function(distribution, from, n = 1L,
								include_from = FALSE, ...) {
	with(distribution$components, {
		distionary::prev_discrete(
			distribution, from = from, n = n, include_from = include_from
		) * scale
	})
}
