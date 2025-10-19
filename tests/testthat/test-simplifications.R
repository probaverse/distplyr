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
      case0 <- case
      dst_simplified <- rlang::exec(verb, !!!case)
      i_dst <- which(vapply(case, distionary::is_distribution, logical(1L)))
      for (i in i_dst) {
        attr(case[[i]], "name") <- "Unknown"
      }
      dst_standard <- rlang::exec(verb, !!!case)
      r <- eval_quantile(dst_simplified, c(0.001, 0.999))
      x <- seq(r[1], r[2], length.out = 100)
      cdf_simplified <- distionary::eval_cdf(dst_simplified, x)
      cdf_standard <- distionary::eval_cdf(dst_standard, x)
      expect_true(length(unique(cdf_simplified)) > 1)
      if (verb == "log" && distionary::vtype(case0[[1]]) == "discrete") {
        # Rounding issue: exp(log(19)) - 19 = -3.552714e-15
        cdf_standard_eps <- distionary::eval_cdf(dst_standard, x + 1e-15)
        expect_true(all(cdf_simplified >= cdf_standard))
        expect_true(all(cdf_simplified <= cdf_standard_eps))
      } else {
        expect_equal(cdf_simplified, cdf_standard)
      }
    }
  }
})
