#' @export
num_discretes.mix <- function(distribution, from, to, include_from, include_to) {
	inf_discretes <- distionary::has_infinite_discretes(
		distribution, from = from, to = to
	)
	if (inf_discretes) {
		return(Inf)
	}
	with(distribution$components, {
		n <- vapply(
			distributions, distionary::num_discretes, FUN.VALUE = numeric(1L),
			from = from, to = to,include_from = include_from,
			include_to = include_to
		)
		discretes <- list()
		for (i in seq_along(distributions)) {
			discretes[[i]] <- distionary::next_discrete(
				distributions[[i]], from = from, n = n[[i]],
				include_from = include_from
			)
		}
		discretes <- unique(c(discretes, recursive = TRUE))
		length(discretes)
	})
}

#' @export
next_discrete.mix <- function(distribution, from, n = 1L,
                              include_from = FALSE) {
	if (n == 0) return(numeric(0L))
	inf_discretes <- is.infinite(n) &&
		has_infinite_discretes(distribution, from = from, to = Inf)
	if (inf_discretes) {
		stop("Your selection includes an infinite number of discrete points.")
	}
	distributions <- distribution$components$distributions
	next_discretes <- lapply(
		distributions, distionary::next_discrete,
		from = from, n = 1L, include_from = include_from
	)
	next_discretes <- c(next_discretes, recursive = TRUE)
	if (length(next_discretes) == 0) {
		return(numeric(0L))
	}
	is_nan <- is.nan(next_discretes)
	next_discretes <- next_discretes[!is_nan]
	if (length(next_discretes) == 0) {
		return(NaN)
	}
	candidate <- min(next_discretes)
	nan_result <- any(is_nan) &&
		distionary::has_infinite_discretes(
			distribution, from = from, to = candidate
		)
	if (nan_result) {
		return(NaN)
	}
	discretes <- candidate
	i <- 2L
	while (i <= n) {
		next_discretes <- lapply(
			distributions, distionary::next_discrete,
			from = discretes[i - 1L], n = 1L, include_from = FALSE
		)
		next_discretes <- c(next_discretes, recursive = TRUE)
		if (length(next_discretes) == 0) {
			return(discretes)
		}
		is_nan <- is.nan(next_discretes)
		next_discretes <- next_discretes[!is_nan]
		if (length(next_discretes) == 0) {
			discretes[i] <- NaN
			return(discretes)
		}
		candidate <- min(next_discretes)
		reached_nan <- any(is_nan) &&
			has_infinite_discretes(
				distribution, from = discretes[i - 1L], to = candidate
			)
		if (reached_nan) {
			discretes[i] <- NaN
			return(discretes)
		}
		discretes[i] <- candidate
		i <- i + 1L
	}
	discretes
}

#' @export
prev_discrete.mix <- function(distribution, from, n = 1L,
                              include_from = FALSE) {
	if (n == 0) {
		return(numeric(0L))
	}
	inf_discretes <- is.infinite(n) &&
		distionary::has_infinite_discretes(distribution, from = -Inf, to = from)
	if (inf_discretes) {
		stop("Your selection includes an infinite number of discrete points.")
	}
	distributions <- distribution$components$distributions
	prev_discretes <- lapply(
		distributions, distionary::prev_discrete,
		from = from, n = 1L, include_from = include_from
	)
	prev_discretes <- c(prev_discretes, recursive = TRUE)
	if (length(prev_discretes) == 0) {
		return(numeric(0L))
	}
	is_nan <- is.nan(prev_discretes)
	prev_discretes <- prev_discretes[!is_nan]
	if (length(prev_discretes) == 0) {
		return(NaN)
	}
	candidate <- min(prev_discretes)
	nan_result <- any(is_nan) &&
		distionary::has_infinite_discretes(
			distribution, from = discretes[i - 1L], to = candidate
		)
	if (nan_result) {
		return(NaN)
	}
	discretes <- candidate
	i <- 2L
	while (i <= n) {
		prev_discretes <- lapply(
			distributions, distionary::prev_discrete,
			from = discretes[i - 1L], n = 1L, include_from = FALSE
		)
		prev_discretes <- c(prev_discretes, recursive = TRUE)
		if (length(prev_discretes) == 0) {
			return(discretes)
		}
		is_nan <- is.nan(prev_discretes)
		prev_discretes <- prev_discretes[!is_nan]
		if (length(prev_discretes) == 0) {
			discretes[i] <- NaN
			return(discretes)
		}
		candidate <- min(prev_discretes)
		reached_nan <- any(is_nan) &&
			distionary::has_infinite_discretes(
				distribution, from = from, to = candidate
			)
		if (reached_nan) {
			discretes[i] <- NaN
			return(discretes)
		}
		discretes[i] <- candidate
		i <- i + 1L
	}
	discretes
}
