# A list of distributions to be used for systematic testing **of special
# simplification cases**.
#
# This object defines a comprehensive set of distributions used for systematic
# testing of distribution representations, like with the `test_distributions`
# object, but has special treatment in the test suite: the simplification
# is compared against the standard implementation.
library(distionary)

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
    list(dst_empirical(-10:-5), dst_empirical(3:5)),
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

usethis::use_data(special_distributions, overwrite = TRUE, internal = TRUE)
