test_that("different input arrangements work.", {
  d1 <- dst_norm(0, 1)
  d2 <- d1 + 1
  d3 <- d1 + 2
  ref <- list(d1, d2, d3)
  ld1 <- list(d1)
  expect_equal(ref, dots_to_dsts(ref))
  expect_equal(ref, dots_to_dsts(list(d1), list(d2, d3)))
  expect_equal(ld1, dots_to_dsts(d1))
  expect_equal(ld1, dots_to_dsts(list(d1)))
  expect_error(dots_to_dsts(list(list(d1))))
  expect_error(dots_to_dsts(list(d1, "character")))
})
