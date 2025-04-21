#' Conditional Distributions
#'
#' `slice_left()` removes probability to the left of some breakpoint,
#' conditioning the random variable to be bigger than the breakpoint.
#' `slice_right()` does the opposite: removes probability to the right,
#' conditioning to be smaller than the breakpoint.
#'
#' @param distribution Distribution to slice.
#' @param breakpoint Point at which to slice (single numeric).
#' @param include Logical; should the breakpoint be removed as well?
#' This is only realistically relevant if the
#' breakpoint has a non-zero probability of occurrence.
#' @param ... Other arguments to pass to specific methods. Currently unused.
#' @return A conditional distribution.
#' @examples
#' distionary::dst_norm(0, 1) |>
#'   slice_left(-2) |>
#'   slice_right(2) |>
#'   distionary::enframe_cdf(at = -3:3)
#'
#' d |>
#'   slice_left(5) |>
#'   distionary::eval_pmf(at = 5)
#' d |>
#'   slice_left(5, include = TRUE) |>
#'   distionary::eval_pmf(at = 5)
#' @rdname slice
#' @export
slice_left <- function(distribution, breakpoint, include = TRUE, ...) {
  checkmate::assert_class(distribution, "dst")
	rng <- range(distribution)
	left <- rng[1L]
	right <- rng[2L]
	if (breakpoint < left) {
		return(distribution)
	}
	all_sliced <- FALSE
	if (breakpoint > right) {
		all_sliced <- TRUE
	}
	if (breakpoint == right) {
		if (include) {
			all_sliced <- TRUE
		} else {
			p <- distionary::eval_pmf(distribution, at = breakpoint)
			if (p == 0) {
				all_sliced <- TRUE
			} else {
				return(distionary::dst_degenerate(breakpoint))
			}
		}
	}
	if (all_sliced) {
	  warning("Sliced off entire distribution. Returning Null distribution")
	  return(distionary::dst_null())
	}
	v <- distionary::vtype(distribution)
	if (v != "continuous") {
	  warning(
	    "A non-continuous distribution has been entered into a distplyr verb.\n",
	    "At this stage of distplyr's development, some inaccuracies can be\n",
	    "expected in these cases, particularly for quantile calculations."
	  )
	}
	if (v == "mixed") {
	  v <- "unknown" # For now.
	}
	d <- distionary::distribution(
	  cdf = function(x) {
	    p_kept <- distionary::prob_right(
	      distribution, of = breakpoint, inclusive = !include
	    )
	    cdf <- 1 - distionary::eval_survival(distribution, at = x) / p_kept
	    pmax(cdf, 0)
	  },
	  survival = function(x) {
	    p_kept <- distionary::prob_right(
	      distribution, of = breakpoint, inclusive = !include
	    )
	    s <- distionary::eval_survival(distribution, at = x) / p_kept
	    pmin(s, 1)
	  },
	  density = function(x) {
	    p_kept <- distionary::prob_right(
	      distribution, of = breakpoint, inclusive = !include
	    )
	    pdf <- distionary::eval_density(distribution, at = x) / p_kept
	    if (include) {
	      pdf[x <= breakpoint] <- 0
	    } else {
	      pdf[x < breakpoint] <- 0
	    }
	    pdf
	  },
	  pmf = function(x) {
	    p_kept <- distionary::prob_right(
	      distribution, of = breakpoint, inclusive = !include
	    )
	    pmf <- distionary::eval_pmf(distribution, at = x) / p_kept
	    if (include) {
	      pmf[x <= breakpoint] <- 0
	    } else {
	      pmf[x < breakpoint] <- 0
	    }
	    pmf
	  },
	  quantile = function(p) {
	    p_kept <- distionary::prob_right(
	      distribution, of = breakpoint, inclusive = !include
	    )
	    distionary::eval_quantile(
	      distribution, at = (1 - p_kept) + p * p_kept
	    )
	  },
	  range = c(breakpoint, right),
	  .vtype = v,
	  .name = "Left-Sliced",
	  .parameters = list(
	    distribution = distribution,
	    breakpoint = breakpoint,
	    include = include
	  )
	)
	distionary::new_distribution(d, class = "slice_left")
}



