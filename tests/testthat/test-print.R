test_that("A verb names the distribution it was built from.", {
  d <- trim_left(distionary::dst_norm(0, 1), 1)
  out <- capture.output(print(d))
  expect_match(out[1], "Left-Trimmed distribution \\(continuous\\)")
  expect_true(any(grepl("distribution  Normal\\(0, 1\\)", out)))
  # The distribution inside is named, not unfolded: no second parameter
  # block, and none of the list printing that comes with one.
  expect_equal(sum(grepl("--Parameters--", out)), 1L)
  expect_false(any(grepl("^\\$", out)))
  expect_true(length(out) < 10L)
})

test_that("A mixture lists its components and their weights.", {
  d <- mix(
    distionary::dst_norm(0, 1),
    distionary::dst_pois(3),
    weights = c(1, 2)
  )
  out <- capture.output(print(d))
  expect_match(out[1], "Mixture distribution")
  expect_true(any(grepl("Normal\\(0, 1\\)", out)))
  expect_true(any(grepl("Poisson\\(3\\)", out)))
  expect_true(any(grepl("0.3333", out)))
  expect_true(any(grepl("0.6667", out)))
})

test_that("A graft names both of its pieces on one line each.", {
  body <- distionary::dst_norm(0, 1)
  d <- graft_right(body, of = 2, tail_excess = distionary::dst_gp(1, 0.3))
  out <- capture.output(print(d))
  expect_match(out[1], "Graft distribution")
  expect_true(any(grepl("Right-Trimmed\\(Normal\\(0, 1\\)\\)", out)))
  expect_true(any(grepl("Shifted\\(Generalised Pareto\\(1, 0.3\\)\\)", out)))
  expect_true(length(out) < 10L)
})

test_that("A long list of components is cut short and counted.", {
  parts <- lapply(1:13, function(i) distionary::dst_norm(i, 1))
  d <- do.call(mix, c(parts, list(weights = rep(1, 13))))
  out <- capture.output(print(d))
  expect_true(any(grepl("\\.\\.\\.and 3 more", out)))
  expect_false(any(grepl("Normal\\(11, 1\\)", out)))
})

test_that("A distribution whose parameters are not numbers gets its name.", {
  # A Finite distribution's parameters are vectors, so `pretty_name()`
  # cannot take them --- asked for them, it tries to round a vector of
  # outcomes. The name alone stands in for the component.
  d <- mix(
    distionary::dst_empirical(1:20),
    distionary::dst_norm(0, 1),
    weights = c(1, 1)
  )
  out <- capture.output(print(d))
  expect_true(any(grepl("Finite ", out)))
  expect_false(any(grepl("Finite\\(", out)))
  expect_true(any(grepl("Normal\\(0, 1\\)", out)))
  expect_true(length(out) < 10L)
})

test_that("Printing returns the distribution invisibly.", {
  trimmed <- trim_left(shift(distionary::dst_norm(0, 1), 2), 1)
  capture.output(printed <- withVisible(print(trimmed)))
  expect_false(printed$visible)
  expect_equal(printed$value, trimmed)
})
