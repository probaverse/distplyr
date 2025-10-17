library(distionary)

test_that("multiply works with continuous distributions", {
  d <- dst_exp(1)
  scaled <- multiply(d, 2)
  expect_equal(pretty_name(scaled), "Scaled")
  expect_equal(mean(scaled), mean(d) * 2)
})

test_that("multiply works for a Normal distribution (special case).", {
  d <- dst_norm(0, 1)
  scaled_special <- multiply(d, 2)
  expect_equal(pretty_name(scaled_special), "Normal")
  attributes(d)$name <- "something else"
  scaled_regular <- multiply(d, 2)
  expect_equal(
    eval_cdf(scaled_special, at = seq(-5, 5, by = 0.1)),
    eval_cdf(scaled_regular, at = seq(-5, 5, by = 0.1))
  )
})

test_that("multiply handles edge cases", {
  d <- dst_unif(0, 10)
  expect_equal(pretty_name(multiply(d, 0)), "Degenerate")
  expect_error(multiply(d, Inf))
})
