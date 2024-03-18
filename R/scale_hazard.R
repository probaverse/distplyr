#' Scale a distribution's hazard function
#'
#' Makes a new distribution whose hazard function (ordinates) are a
#' multiple of a baseline distribution's hazard function. This is
#' particularly useful for proportional hazards models such as the Cox
#' model.
#'
#' @param distribution A probability distribution.
#' @param by Scaling factor; a single positive number.
#' @note Currently this function only applies to continuous distributions,
#' as it's unclear whether an equivalent would make sense otherwise.
#' @returns A probability distribution whose hazard function is the
#' original `distribution`'s hazard function with ordinates multiplied by `by`.
#' @export
scale_hazard <- function(distribution, by) {
  if (length(by) != 1) {
    stop("`by` must be a vector of length 1.")
  }
  if (by <= 0) {
    stop("Can only scale a hazard function by a positive number.")
  }
  v <- distionary::variable(distribution)
  if (is.na(v) || v != "continuous") {
    warning("Hazard can only be scaled for continuous distributions. ",
            "Returning NULL distribution.")
    return(distionary::dst_null())
  }
  if (by == 1) {
    return(distribution)
  }
  dist <- list(distribution = distribution, scale = by)
  distionary::new_distribution(
    dist, variable = v, class = "sc_haz"
  )
}


