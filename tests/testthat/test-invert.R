library(distionary)

test_that("invert works with continuous distributions", {
  d <- dst_unif(1, 10)
  inv_d <- invert(d)
  expect_s3_class(inv_d, "inverse")
  expect_equal(eval_cdf(inv_d, at = 5), eval_survival(d, at = 1 / 5))
})

# test_that("invert handles edge cases", {
#   expect_error(invert(dst_pois(10))) # Contains zero
# })
