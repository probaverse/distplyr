#' Mutate Parameters
#'
#' Modify distribution parameters. A lightweight function that only
#' checks that the parameter you're modifying already exists in the
#' distribution.
#' @param distribution Parametric distribution
#' @param ... Named expressions; quoted. Names should be parameters names of the
#' distribution. Expressions can involve computations with other parameters.
#' @return The input distribution, with the parameters modified as specified
#' in `...`.
#' @details
#' Parameters are not modified on-the-fly. That means parameters that are
#' modified earlier in `...` still retain their original values for use
#' downstream in `...`.
#' @examples
#' d <- distionary::dst_unif(1, 3)
#' distplyr:::mutate_parameters(d, min = max - min)
mutate_parameters <- function(distribution, ...) {
	ell <- rlang::enquos(...)
	params <- distionary::parameters(distribution)
	param_names <- names(params)
	for (i in seq_along(ell)) {
		param_name <- names(ell)[[i]]
		stopifnot(param_name %in% param_names)
		param_val <- ell[[i]]
		new_val <- rlang::eval_tidy(param_val, data = params)
		distribution$parameters[[param_name]] <- new_val
	}
	distribution
}
