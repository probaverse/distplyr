#' @rdname slice
#' @export
slice_right <- function(distribution, breakpoint, include = TRUE, ...) {
	rng <- range(distribution)
	left <- rng[1L]
	right <- rng[2L]
	if (breakpoint > right) {
		return(distribution)
	}
	all_sliced <- FALSE
	if (breakpoint < left) {
		all_sliced <- TRUE
	}
	if (breakpoint == left) {
		if (include) {
			all_sliced <- TRUE
		} else {
			p <- distionary::eval_pmf(
			  distribution, at = breakpoint, strict = FALSE
			)
			if (p == 0) {
				all_sliced <- TRUE
			} else {
				return(distionary::dst_degenerate(breakpoint))
			}
		}
	}
	if (all_sliced) {
	  warning("Sliced off entire distribution. Returning Null distribution.")
	  return(distionary::dst_null())
	}
	v <- distionary::vtype(distribution)
	if (v == "mixed") {
		v <- "unknown" # For now. Need to evaluate cumulative discrete probs.
	}
	d <- distribution(
	  cdf = function(x) {
	    p_kept <- distionary::prob_left(
	      distribution, of = breakpoint, inclusive = !include
	    )
	    cdf <- distionary::eval_cdf(distribution, at = x) / p_kept
	    pmin(cdf, 1)
	  },
	  density = function(x) {
	    p_kept <- distionary::prob_left(
	      distribution, of = breakpoint, inclusive = !include
	    )
	    pdf <- distionary::eval_density(distribution, at = x) / p_kept
	    if (include) {
	      pdf[x >= breakpoint] <- 0
	    } else {
	      pdf[x > breakpoint] <- 0
	    }
	    pdf
	  },
	  pmf = function(x) {
	    p_kept <- distionary::prob_left(
	      distribution, of = breakpoint, inclusive = !include
	    )
	    pmf <- distionary::eval_pmf(distribution, at = x) / p_kept
	    if (include) {
	      pmf[x >= breakpoint] <- 0
	    } else {
	      pmf[x > breakpoint] <- 0
	    }
	    pmf
	  },
	  quantile = function(p) {
	    p_kept <- distionary::prob_left(
	      distribution, of = breakpoint, inclusive = !include
	    )
	    distionary::eval_quantile(distribution, at = p * p_kept)
	  },
	  range = c(left, breakpoint),
	  .vtype = v,
	  .name = "Right-Sliced",
	  .parameters = list(
	    distribution = distribution,
	    breakpoint = breakpoint,
	    include = include
	  )
	)
	distionary::new_distribution(d, class = "slice_right")
}
