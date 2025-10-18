# Test distribution simplifications
#
# For most verbs, distribution simplifications are helpful conceptually and
# for efficiency. For example, the logarithm of a Normal distribution is a
# Log Normal; a mixture of Finite distributions is also Finite. Parameter
# combinations for each verb that results in such simplifications are covered
# in the `special_distributions` internal object.
#
# To test that the correct simplified distribution was identified, one only
# needs to check that one of the representations of the simplified distribution
# matches that of the standard implementation. Not all representations need to
# be checked, because in all cases, a valid distribution object is returned,
# and the internal consistency of each distribution family has already been
# checked (either in distplyr for verb-based distributions, or in distionary
# for standard distribution families).
test_that("Simplifications: CDFs match.", {
  for (verb in names(special_distributions)) {
    cases <- special_distributions[[verb]]
    for (case in cases) {
      dst_simplified <- rlang::exec(verb, !!!case)
      i_dst <- which(vapply(case, distionary::is_distribution, logical(1L)))
      for (i in i_dst) {
        attr(case[[i]], "name") <- "Unknown"
      }
      dst_standard <- rlang::exec(verb, !!!case)
      r <- range(dst_simplified)
      x <- seq(r[1], r[2], length.out = 100)
      cdf_simplified <- distionary::eval_cdf(dst_simplified, x)
      cdf_standard <- distionary::eval_cdf(dst_standard, x)
      expect_true(length(unique(cdf_simplified)) > 1)
      expect_equal(cdf_simplified, cdf_standard)
    }
  }
})
