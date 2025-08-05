#' Create a tbl_dst
#'
#' Connect a distribution to variables in a data frame.
#' Experimental and not robust to downstream manipulation.
#' The handling of such objects probably
#' deserves its own package.
#'
#' @param .data Data frame or tibble
#' @param ... variables to connect a distribution to, some perhaps set
#' to a \code{dst} object. Tidyselect should work, too.
#' @return The original .data object, with a new "distributions"
#' attribute: a list whose names match the names of .data, and whose
#' entries are either a distribution (if that variable was specified),
#' or NA if not. Variables not set to a specific distribution will have
#' an empirical distribution.
#' @details Distributions are set in the order that they show up in \code{...}.
#' @note No precaution has yet to be taken as to how the \code{distributions}
#' attribute is handled downstream. \code{mutate()} etc. at your own risk.
#' @export
tbl_dst <- function(.data, ...) {
	# Inspiration from https://adv-r.hadley.nz/evaluation.html#application-transform
	if (!is.data.frame(.data)) {
		stop("Expecting '.data' to be a data frame. Instead, received ",
			 class(.data)[1], ".")
	}
	dots <- rlang::enquos(...)
	vars <- names(.data)
	nvars <- length(vars)
	distributions <- rep(list(NA), nvars)
	names(distributions) <- vars
	dot_names <- names(dots)
	is_named_dot <- dot_names  != ""
	for (i in seq_along(dots)) {
		dot_name <- dot_names[i]
		dot <- dots[[i]]
		# dot_label <- rlang::as_label(dot)
		if (dot_name != "") {
			.dst <- rlang::eval_tidy(dot, .data)
			if (is_distribution(.dst)) {
				distributions[[dot_name]] <- .dst
			} else {
				stop("Specification for variable '", dot_name,
					 "' is not a valid 'dst' object.")
			}
		} else {
			user_vars <- tidyselect::vars_select(names(distributions), !!dot)
			for (uvar in user_vars) {
				distributions[[uvar]] <- dst_emp(.data[[uvar]])
			}
		}
	}
	attr(.data, "distributions") <- distributions
	class(.data) <- c("tbl_dst", class(.data))
	.data
}
