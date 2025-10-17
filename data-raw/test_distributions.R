# A list of distributions to be used for systematic testing
#
# This object defines a comprehensive set of distributions used for systematic
# testing of distribution representations. The testing strategy is the same as
# in distionary: compare the specified formulas against that recovered from
# other properties.
#
# Each distribution entry contains:
# - `distribution`: The constructor function name
# - `invalid`: Parameter sets that should fail validation
# - `valid`: Parameter sets for systematic testing
library(distionary)

test_distributions <- list(
  mix = list(
    list(dst_unif(0, 1), dst_unif(3, 5)),
    list(dst_geom(1), dst_pois(1)),
    list(dst_norm(0, 1), dst_norm(5, 2), dst_norm(-5, 0.5), weights = 1:3),
    list(dst_gamma(6, 2), dst_t(5), dst_exp(1.5), weights = c(2, 1, 3))
  ),
  minimize = list(
    list(dst_pois(1), draws = 5),
    list(dst_norm(0, 1), draws = 5.5),
    list(dst_unif(0, 1), dst_unif(3, 5)),
    list(dst_geom(1), dst_pois(1)),
    list(dst_norm(0, 1), dst_norm(5, 2), dst_norm(-5, 0.5), draws = 1:3)
  ),
  maximize = list(
    list(dst_pois(1), draws = 5),
    list(dst_norm(0, 1), draws = 5.5),
    list(dst_unif(0, 1), dst_unif(3, 5)),
    list(dst_geom(1), dst_pois(1)),
    list(dst_norm(0, 1), dst_norm(5, 2), dst_norm(-5, 0.5), draws = 1:3)
  ),
  shift = list(
    list(dst_unif(0, 1), 5),
    list(dst_pois(1), -0.5),
    list(dst_exp(1.5), -2),
    list(dst_norm(0, 1), 3.5)
  ),
  multiply = list(
    list(dst_exp(1.5), 2),
    list(dst_gamma(2, 4), 0.5),
    list(dst_unif(3, 5), 5),
    list(dst_beta(5, 2), -8)
  ),
  flip = list(
    list(dst_geom(0.3)),
    list(dst_binom(5, 0.4)),
    list(dst_unif(-1, 5)),
    list(dst_empirical(-4:7)),
    list(dst_unif(4, 5)),
    list(dst_unif(-10, -4))
  ),
  invert = list(
    list(dst_geom(0.3) + 1),
    list(dst_norm(1, 5)),
    list(dst_unif(4, 5)),
    list(dst_unif(-10, -4))
  ),
  exp = list(
    list(dst_exp(1.5) - 1),
    list(dst_beta(2, 4)),
    list(dst_unif(3, 5)),
    list(-dst_beta(5, 2)),
    list(dst_pois(1)),
    list(dst_nbinom(5, 0.1) - 10)
  ),
  log = list(
    list(dst_exp(1.5)),
    list(dst_gamma(5, 3), base = 10),
    list(dst_unif(3, 5), base = 5),
    list(dst_unif(0, 4)),
    list(dst_weibull(2, 1))
  )
)

usethis::use_data(test_distributions, overwrite = TRUE, internal = TRUE)
