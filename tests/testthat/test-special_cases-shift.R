library(distionary)

test_that("Shift - special cases - light testing", {
  # Special implementation for Normal distribution
  norm_dist <- dst_norm(2, 3)
  norm_shifted <- shift(norm_dist, 5)
  expect_equal(pretty_name(norm_shifted), "Normal")
  expect_equal(parameters(norm_shifted)[["mean"]], 7)
  expect_equal(parameters(norm_shifted)[["sd"]], 3)

  # Special implementation for Uniform distribution
  unif_dist <- dst_unif(1, 5)
  unif_shifted <- shift(unif_dist, 10)
  expect_equal(pretty_name(unif_shifted), "Uniform")
  expect_equal(parameters(unif_shifted)[["min"]], 11)
  expect_equal(parameters(unif_shifted)[["max"]], 15)

  # Special implementation for Cauchy distribution
  cauchy_dist <- dst_cauchy(location = 0, scale = 2)
  cauchy_shifted <- shift(cauchy_dist, 3)
  expect_equal(pretty_name(cauchy_shifted), "Cauchy")
  expect_equal(parameters(cauchy_shifted)[["location"]], 3)
  expect_equal(parameters(cauchy_shifted)[["scale"]], 2)

  # Special implementation for Generalised Extreme Value distribution
  gev_dist <- dst_gev(location = 1, scale = 2, shape = 0.5)
  gev_shifted <- shift(gev_dist, 4)
  expect_equal(pretty_name(gev_shifted), "Generalised Extreme Value")
  expect_equal(parameters(gev_shifted)[["location"]], 5)
  expect_equal(parameters(gev_shifted)[["scale"]], 2)
  expect_equal(parameters(gev_shifted)[["shape"]], 0.5)
})

test_that("Shift - NA handling", {
  expect_equal(shift(dst_null(), 5), dst_null())
  expect_equal(shift(dst_exp(1), NA), dst_null())
})

test_that("Shift - Edge cases", {
  d <- dst_exp(1.1)
  expect_equal(shift(d, 0), d)
})

test_that("Shift - bad parameters", {
  expect_error(shift(dst_exp(1), Inf))
  expect_error(shift(dst_exp(1), -Inf))
  expect_error(shift(dst_exp(1), "a"))
  expect_error(shift(5, 5))
})
