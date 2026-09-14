#' Print a distribution that a verb built.
#'
#' A verb keeps the distributions it was built from among its parameters, so
#' the default method prints each of them in full, and a graft of a trim of
#' a shift arrives as pages of nested `--Parameters--` blocks. These methods
#' keep the header and name each distribution with `pretty_name()` instead
#' of unfolding it.
#' @noRd
print_verb <- function(x, ...) {
  cat(distionary::pretty_name(x), "distribution")
  vtype <- distionary::vtype(x)
  if (length(vtype) == 1L && !is.na(vtype)) {
    cat(" (", vtype, ")", sep = "")
  }
  cat("\n")
  params <- distionary::parameters(x)
  components <- params[["distributions"]]
  if (!is.null(components)) {
    print_components(components, params[["probs"]], ...)
    return(invisible(x))
  }
  if (length(params) > 0L) {
    cat("--Parameters--\n")
    print_parameters(params)
  }
  invisible(x)
}

#' The components of a mixture, one to a row, with their weights.
#'
#' A mixture can have as many components as it likes, and a page of them
#' says no more than the first few and a count.
#' @noRd
print_components <- function(components, probs, ..., max_rows = 10L) {
  cat("--Components--\n")
  labels <- vapply(components, verb_label, FUN.VALUE = character(1L))
  table <- data.frame(distribution = labels, stringsAsFactors = FALSE)
  if (length(probs) == length(labels)) {
    table[["weight"]] <- signif(probs, 4L)
  }
  rest <- nrow(table) - max_rows
  if (rest > 0L) {
    table <- table[seq_len(max_rows), , drop = FALSE]
  }
  print(table, row.names = FALSE, right = FALSE, ...)
  if (rest > 0L) {
    cat("...and", rest, "more\n")
  }
}

#' A verb's parameters, one to a line, name against value.
#' @noRd
print_parameters <- function(params) {
  labels <- vapply(params, param_label, FUN.VALUE = character(1L))
  names <- format(names(params))
  cat(paste0(names, "  ", labels, collapse = "\n"), "\n", sep = "")
}

#' A parameter on one line.
#'
#' A distribution becomes its name; a short vector its values; anything
#' longer or stranger says what it is, rather than spilling down the page.
#' @noRd
param_label <- function(param) {
  if (inherits(param, "dst")) {
    return(verb_label(param))
  }
  if (is.list(param)) {
    return(paste0("<list of ", length(param), ">"))
  }
  if (length(param) == 0L) {
    return("<none>")
  }
  values <- format(param, digits = 4L, trim = TRUE)
  if (length(values) > 4L) {
    values <- c(values[1:4], "...")
  }
  paste(values, collapse = ", ")
}

#' Name a distribution in one line, with its parameters where they fit.
#'
#' `pretty_name()` can take the parameters only when every one of them is a
#' single number: asked for them otherwise it tries to round a distribution
#' and fails. A verb's parameters hold the distribution it was built from,
#' so that one is named inside the brackets instead --- `Shifted(Normal(0,
#' 1))` --- until either the nesting or the width runs out.
#' @noRd
verb_label <- function(distribution, depth = 2L, width = 44L) {
  name <- distionary::pretty_name(distribution)
  params <- distionary::parameters(distribution)
  if (length(params) == 0L) {
    return(name)
  }
  numbers <- vapply(
    params,
    function(p) is.numeric(p) && length(p) == 1L,
    FUN.VALUE = logical(1L)
  )
  if (all(numbers)) {
    return(within_width(
      distionary::pretty_name(distribution, param_digits = 2L), name, width
    ))
  }
  inner <- params[vapply(params, inherits, logical(1L), "dst")]
  if (depth > 0L && length(inner) == 1L) {
    nested <- verb_label(inner[[1L]], depth = depth - 1L, width = width)
    return(within_width(paste0(name, "(", nested, ")"), name, width))
  }
  name
}

#' @noRd
within_width <- function(label, fallback, width) {
  if (nchar(label) <= width) label else fallback
}

#' @export
print.shifted <- print_verb

#' @export
print.scaled <- print_verb

#' @export
print.negated <- print_verb

#' @export
print.inverse <- print_verb

#' @export
print.logarithmic <- print_verb

#' @export
print.exponential <- print_verb

#' @export
print.trim_left <- print_verb

#' @export
print.trim_right <- print_verb

#' @export
print.graft <- print_verb

#' @export
print.mixture <- print_verb

#' @export
print.minimum <- print_verb

#' @export
print.maximum <- print_verb
