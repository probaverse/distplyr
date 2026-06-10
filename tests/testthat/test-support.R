# Support propagation through distplyr's verbs.

vt <- function(d) distionary::vtype(d)
atoms_of <- function(d) as.double(distionary::atoms(distionary::support(d)))
cont_of <- function(d) unname(distionary::continuous_part(distionary::support(d)))

test_that("Monotonic transforms carry the support.", {
  # shift: atoms translate.
  d <- shift(distionary::dst_pois(3), 2.5)
  expect_equal(vt(d), "discrete")
  expect_equal(atoms_of(d)[1:3], c(2.5, 3.5, 4.5))
  # multiply by a positive constant: atoms scale.
  d <- multiply(distionary::dst_pois(3), 2)
  expect_equal(atoms_of(d)[1:3], c(0, 2, 4))
  # flip: continuous [0, Inf) -> (-Inf, 0].
  d <- flip(distionary::dst_gamma(2, 1))
  expect_equal(vt(d), "continuous")
  expect_equal(cont_of(d), matrix(c(-Inf, 0), nrow = 1))
  # log of a positive distribution -> the whole line.
  d <- log_distribution(distionary::dst_gamma(2, 1))
  expect_equal(cont_of(d), matrix(c(-Inf, Inf), nrow = 1))
})

test_that("invert carries the support only when it does not span zero.", {
  d <- invert(distionary::dst_gamma(2, 1))
  expect_equal(vt(d), "continuous")
  expect_false(is.null(distionary::support(d)))
  # Spanning zero: support is not set (falls back).
  d0 <- invert(distionary::dst_norm(0, 1))
  expect_null(distionary::support(d0))
})

test_that("mix unions the component supports.", {
  # Two continuous overlap -> single interval.
  d <- mix(distionary::dst_unif(0, 2), distionary::dst_unif(1, 3), weights = c(1, 1))
  expect_equal(cont_of(d), matrix(c(0, 3), nrow = 1))
  # discrete + continuous -> mixed.
  d <- mix(distionary::dst_pois(3), distionary::dst_unif(0, 10), weights = c(1, 1))
  expect_equal(vt(d), "mixed")
  expect_equal(atoms_of(d)[1:3], c(0, 1, 2))
  expect_equal(cont_of(d), matrix(c(0, 10), nrow = 1))
})

test_that("trim restricts the support.", {
  d <- trim_left(distionary::dst_norm(0, 1), of = 1)
  expect_equal(cont_of(d), matrix(c(1, Inf), nrow = 1))
  d <- trim_right(distionary::dst_gamma(2, 1), of = 3)
  expect_equal(cont_of(d), matrix(c(0, 3), nrow = 1))
})

test_that("maximize/minimize: continuous supports.", {
  d <- maximize(distionary::dst_unif(0, 2), distionary::dst_unif(1, 3))
  expect_equal(vt(d), "continuous")
  expect_equal(cont_of(d), matrix(c(1, 3), nrow = 1)) # [max(lo), max(hi)]
  d <- minimize(distionary::dst_unif(0, 2), distionary::dst_unif(1, 3))
  expect_equal(cont_of(d), matrix(c(0, 2), nrow = 1)) # [min(lo), min(hi)]
})

test_that("maximize keeps a boundary atom that has positive mass.", {
  # max(Unif(0,5), Finite{2,7}): new_lo = 2 is an atom of the max (mass 0.2).
  d <- maximize(
    distionary::dst_unif(0, 5),
    distionary::dst_finite(c(2, 7), probs = c(0.5, 0.5))
  )
  expect_equal(vt(d), "mixed")
  expect_equal(atoms_of(d), c(2, 7))
  expect_equal(cont_of(d), matrix(c(2, 5), nrow = 1))
})

test_that("maximize drops a boundary atom with zero mass.", {
  # max(Finite{0,5}, Unif(0,3)): new_lo = 0 is an atom of Finite, but
  # P(max = 0) = 0 because Unif(0,3) > 0 almost surely.
  d <- maximize(
    distionary::dst_finite(c(0, 5), probs = c(0.5, 0.5)),
    distionary::dst_unif(0, 3)
  )
  expect_equal(vt(d), "mixed")
  expect_equal(atoms_of(d), 5) # 0 removed
  expect_equal(cont_of(d), matrix(c(0, 3), nrow = 1))
})

test_that("A verb on a support-less (legacy) input falls back gracefully.", {
  legacy <- suppressWarnings(distionary::distribution(
    cdf = stats::pnorm, density = stats::dnorm, .vtype = "continuous"
  ))
  expect_null(distionary::support(legacy))
  d <- shift(legacy, 3)
  expect_null(distionary::support(d)) # no structured support to propagate
  expect_equal(vt(d), "continuous") # but still works
})
