# Test distribution simplifications
#
# For most verbs, distribution simplifications are helpful conceptually and
# for efficiency. For example, the logarithm of a Normal distribution is a
# Log Normal; a mixture of Finite distributions is also Finite. But are the
# simplifications coded up correctly?
#
# This is tested in two ways:
#
# 1. Light testing by comparing parameters. E.g., checking that the log of a
#    LogNormal distribution is a specific Normal distribution.
#    - These are conducted in a file for each verb.
#    - This does not check that the simplification was correct. This case is
#      obvious, but what about, for example, `maximize()`ing multiple finite
#      distributions resulting in another finite distribution?
# 2. More rigorous testing to verify that calculations (and derivations!)
#    are correct (or at least, provide good evidence that they are correct).
#    This is done here.
#
# Strategy of rigorous testing:
#
# - Define a `special_distributions` object that defines
#   parameter combinations for each verb that results in a simplification.
# - For each case, compute the simplified distribution using the verb. Also
#   compute the standard distribution (i.e., without simplifications). In most
#   cases, this can be done by the hack of setting the "name" attribute of each
#   input distribution to "Unknown", but some simplifications (maximize and
#   minimize) require more work (done in those verb files).
# - Compare the CDFs of the two resulting distributions over a fine grid.
#
# Only one representation is needed to be checked,
# because in all cases, a valid distribution object is returned,
# and the internal consistency of each distribution family has already been
# checked (either in distplyr under `test-internal_consistency` or in distionary
# for standard distribution families). CDF is used because it's currently
# required for all distributions.
test_that("Simplifications: CDFs match.", {
  ## Consider all families that are treated specially for each verb.
  special_distributions <- list(
    mix = list(
      list(
        dst_empirical(c(-2, 0, 6, 10), weights = 1:4),
        dst_empirical(c(-5, 4)),
        dst_empirical(c(23, 34, 14), weights = 1:3),
        weights = c(2, 3, 5)
      ),
      list(dst_empirical(-10:-5), dst_empirical(3:5))
    ),
    minimize = list(
      list(
        dst_empirical(c(-2, 0, 6, 10), weights = 1:4),
        dst_empirical(c(-5, 4)),
        dst_empirical(c(23, 34, 14), weights = 1:3),
        draws = c(2, 3, 5)
      ),
      list(dst_empirical(-10:-5), dst_empirical(3:5))
    ),
    maximize = list(
      list(
        dst_empirical(c(-2, 0, 6, 10), weights = 1:4),
        dst_empirical(c(-5, 4)),
        dst_empirical(c(23, 34, 14), weights = 1:3),
        draws = c(2, 3, 5)
      ),
      list(dst_empirical(-10:-5), dst_empirical(3:5))
    ),
    shift = list(
      list(dst_unif(0, 1), 5),
      list(dst_unif(0, 1), -5),
      list(dst_norm(0, 1), 3.5),
      list(dst_norm(0, 1), -3.5),
      list(dst_gev(5, 3, 0.6), 1.5),
      list(dst_gev(5, 3, 0.6), -1.5),
      list(dst_cauchy(4, 2), 4.4),
      list(dst_cauchy(4, 2), -4.4),
      list(dst_empirical(c(-2, 0, 6, 10), weights = 1:4), 3),
      list(dst_empirical(c(-2, 0, 6, 10), weights = 1:4), -3),
      list(dst_degenerate(3), 2),
      list(dst_degenerate(3), -5)
    ),
    multiply = list(
      list(dst_unif(0, 1), 5),
      list(dst_norm(0, 1), 3.5),
      list(dst_cauchy(4, 2), 4.4),
      list(dst_gp(3, 0.6), 1.5),
      list(dst_gp(3, -0.6), 1.5),
      list(dst_gp(3, 0), 1.5),
      list(dst_gev(5, 3, 0.6), 1.5),
      list(dst_gev(5, 3, -0.6), 1.5),
      list(dst_gev(5, 3, 0), 1.5),
      list(dst_empirical(c(-2, 0, 6, 10), weights = 1:4), 3),
      list(dst_degenerate(3), 2),
      list(dst_degenerate(-3), 2)
    ),
    flip = list(
      list(dst_norm(4, 2)),
      list(dst_norm(-4, 3)),
      list(dst_norm(0, 4)),
      list(dst_unif(2, 3)),
      list(dst_unif(-2, -1)),
      list(dst_unif(-2, 3)),
      list(dst_cauchy(3, 2)),
      list(dst_cauchy(-2, 4)),
      list(dst_cauchy(0, 2)),
      list(dst_t(2)),
      list(dst_t(3)),
      list(dst_empirical(-4:7)),
      list(dst_empirical(c(-2, 0, 6, 10), weights = 1:4)),
      list(dst_empirical(c(6, 10, 50), weights = 1:3)),
      list(dst_degenerate(3))
    ),
    exp = list(
      list(dst_norm(0, 1)),
      list(dst_norm(-5, 3)),
      list(dst_norm(1, 0.3)),
      list(dst_empirical(c(-2, 0, 6, 10), weights = 1:4)),
      list(dst_degenerate(3))
    ),
    log = list(
      list(dst_lnorm(0, 1)),
      list(dst_lnorm(-5, 3)),
      list(dst_lnorm(1, 0.3)),
      list(dst_lnorm(0, 1), base = 5),
      list(dst_lnorm(-5, 3), base = 5),
      list(dst_lnorm(1, 0.3), base = 5),
      list(dst_empirical(c(6, 10, 14, 19), weights = 1:4)),
      list(dst_empirical(c(6, 10, 14, 19), weights = 1:4), base = 3),
      list(dst_degenerate(3))
    )
  )

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
      x <- append(x, x[100] + 1)
      x <- append(x[1] - 1, x)
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
