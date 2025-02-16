#' @export
mean.mix <- function(x, ...) {
	ellipsis::check_dots_empty()
	with(x[["components"]], {
		means <- vapply(
			distributions, mean, FUN.VALUE = numeric(1L)
		)
		sum(probs * means)
	})
}

#' @export
variance.mix <- function(distribution) {
	overall_mean <- mean(distribution)
	with(distribution[["components"]], {
		means <- vapply(
			distributions, mean, FUN.VALUE = numeric(1L)
		)
		variances <- vapply(
			distributions, distionary::variance, FUN.VALUE = numeric(1L)
		)
		sum(probs * (variances + means^2 - overall_mean^2))
	})
}

#' @export
skewness.mix <- function(distribution) {
	overall_mean <- mean(distribution)
	overall_sd <- distionary::stdev(distribution)
	with(distribution[["components"]], {
		means <- vapply(
			distributions, mean, FUN.VALUE = numeric(1L)
		)
		vars <- vapply(
			distributions, distionary::variance, FUN.VALUE = numeric(1L)
		)
		sds <- sqrt(vars)
		skews <- vapply(
			distributions, distionary::skewness, FUN.VALUE = numeric(1L)
		)
		cmoms <- list(
			zero = 1,
			first = 0,
			second = vars,
			third = skews * sds^3
		)
		terms <- lapply(0:3, function(k) {
			choose(3, k) * (means - overall_mean)^(3 - k) * cmoms[[k + 1L]]
		})
		sum(probs * Reduce(`+`, terms)) / overall_sd^3
	})
}

#' @export
kurtosis_exc.mix <- function(distribution) {
	overall_mean <- mean(distribution)
	overall_var <- distionary::variance(distribution)
	with(distribution[["components"]], {
		means <- vapply(
			distributions, mean, FUN.VALUE = numeric(1L)
		)
		vars <- vapply(
			distributions, distionary::variance, FUN.VALUE = numeric(1L)
		)
		sds <- sqrt(vars)
		skews <- vapply(
			distributions, distionary::skewness, FUN.VALUE = numeric(1L)
		)
		kurts <- vapply(
			distributions, distionary::kurtosis, FUN.VALUE = numeric(1L)
		)
		cmoms <- list(
			zero = 1,
			first = 0,
			second = vars,
			third = skews * sds^3,
			fourth = vars^2 * kurts
		)
		terms <- lapply(0:4, function(k) {
			choose(4, k) * (means - overall_mean)^(4 - k) * cmoms[[k + 1L]]
		})
		sum(probs * Reduce(`+`, terms)) / overall_var^2 - 3
	})
}

# #' @export
# evi.mix <- function(distribution, ...) {
# 	with(distribution[["components"]], {
# 		right_ends <- vapply(
# 			distributions, function(d) range(d)[2L],
# 			FUN.VALUE = numeric(1L)
# 		)
# 		max_end <- max(right_ends)
# 		has_max_ends <- right_ends == max_end
# 		evis <- vapply(distributions, distionary::evi, FUN.VALUE = numeric(1L))
# 		final_sign <- if (max_end < Inf) -1 else 1
# 		final_sign * max(abs(evis[has_max_ends]))
# 	})
# }


#' @export
range.mix <- function(distribution, ...) {
	r <- lapply(distribution$components$distributions, range)
	low <- Reduce(min, r)
	high <- Reduce(max, r)
	c(low, high)
}
