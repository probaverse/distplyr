# Standardize Dots and Numeric Arguments for minimize, maximize,
# and mix. Inputs are the distributions in ..., and num is either
# "draws" in max/min or "weights" in mix.
# - Checks inputs with NA handling.
# - Recycles inputs.
# - Aggregates `num` (by addition) and `...` (by `unique()`).
# - Returns dst_null().
# - Otherwise, returns a list of the recycled & aggregated distributions and
#   numbers.
pair_dots_num <- function(...,
                          num,
                          na_action_dst = c("null", "drop", "fail"),
                          na_action_num = c("null", "drop", "fail")) {
  checkmate::assert_numeric(num, finite = TRUE, lower = 0)
  na_action_dst <- rlang::arg_match(na_action_dst)
  na_action_num <- rlang::arg_match(na_action_num)
  dsts <- dots_to_dsts(...)
  ## Recycling
  l <- vctrs::vec_recycle_common(dsts, num)
  dsts <- l[[1L]]
  num <- l[[2L]]
  ## NA handling
  na_dsts <- vapply(
    dsts, \(d) distionary::pretty_name(d) == "Null", FUN.VALUE = logical(1L)
  )
  na_num <- is.na(num)
  has_na_dst <- any(na_dsts)
  has_na_num <- any(na_num)
  # --> Fail first
  if (has_na_dst && na_action_dst == "fail") {
    stop(
      "Null distributions found. You can either deal with these, or choose ",
      "an alternate option for `na_action_dst`."
    )
  }
  if (has_na_num && na_action_num == "fail") {
    stop(
      "Outcomes have NA values. You can either deal with these, or choose ",
      "an alternate option for `na_action_num`."
    )
  }
  # --> Null next
  if (has_na_dst && na_action_dst == "null") {
    return(distionary::dst_null())
  }
  if (has_na_num && na_action_num == "null") {
    return(distionary::dst_null())
  }
  # --> Drop last
  if (na_action_dst == "drop") {
    dsts <- dsts[!na_dsts]
    num <- num[!na_dsts]
  }
  if (na_action_num == "drop") {
    na_num <- is.na(num) # Because num has a new length.
    dsts <- dsts[!na_num]
    num <- num[!na_num]
  }
  ## Numerics corresponding to 0
  zero_num <- num == 0
  dsts <- dsts[!zero_num]
  num <- num[!zero_num]
  n_dsts <- length(dsts)
  if (n_dsts == 0) {
    return(distionary::dst_null())
  }
  ## Aggregate Weights
  grps <- match(dsts, dsts)
  num <- tapply(num, grps, FUN = sum, simplify = FALSE)
  dsts <- tapply(dsts, grps, FUN = unique, simplify = TRUE)
  num <- unlist(unname(num))
  dsts <- unname(dsts)
  list(
    dsts = dsts,
    num = num
  )
}
