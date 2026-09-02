# Support propagation through distplyr's verbs.

vt <- function(d) distionary::vtype(d)
atoms_of <- function(d, n = NULL) {
  a <- distionary::atoms(distionary::support(d))
  if (!is.null(n)) {
    a <- a[seq_len(n)]
  }
  as.double(a)
}
cont_of <- function(d) unname(distionary::regions(distionary::support(d)))

test_that("Monotonic transforms carry the support.", {
  # shift: atoms translate.
  d <- shift(distionary::dst_pois(3), 2.5)
  expect_equal(vt(d), "discrete")
  expect_equal(atoms_of(d, 3), c(2.5, 3.5, 4.5))
  # multiply by a positive constant: atoms scale.
  d <- multiply(distionary::dst_pois(3), 2)
  expect_equal(atoms_of(d, 3), c(0, 2, 4))
  # flip: continuous [0, Inf) -> (-Inf, 0].
  d <- flip(distionary::dst_gamma(2, 1))
  expect_equal(vt(d), "continuous")
  expect_equal(cont_of(d), matrix(c(-Inf, 0), nrow = 1))
  # log of a positive distribution -> the whole line.
  d <- log_distribution(distionary::dst_gamma(2, 1))
  expect_equal(cont_of(d), matrix(c(-Inf, Inf), nrow = 1))
})

test_that("invert carries the support, including across zero.", {
  d <- invert(distionary::dst_gamma(2, 1))
  expect_equal(vt(d), "continuous")
  expect_equal(cont_of(d), matrix(c(0, Inf), nrow = 1))
  # Spanning zero: each side is mapped separately and the results unioned;
  # the touching closed intervals merge to the whole line.
  d0 <- invert(distionary::dst_norm(0, 1))
  expect_equal(vt(d0), "continuous")
  expect_equal(cont_of(d0), matrix(c(-Inf, Inf), nrow = 1))
})

test_that("mix unions the component supports.", {
  # Two continuous overlap -> single interval.
  d <- mix(distionary::dst_unif(0, 2), distionary::dst_unif(1, 3), weights = c(1, 1))
  expect_equal(cont_of(d), matrix(c(0, 3), nrow = 1))
  # discrete + continuous -> mixed.
  d <- mix(distionary::dst_pois(3), distionary::dst_unif(0, 10), weights = c(1, 1))
  expect_equal(vt(d), "mixed")
  expect_equal(atoms_of(d, 3), c(0, 1, 2))
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

test_that("Every verb propagates a Null distribution.", {
  # The Null distribution is the missing value of the distribution world, and
  # it is the only one without a support. A verb handed one must hand one back
  # rather than trying to compute with it -- this is what makes it safe to
  # carry a failed fit through a pipeline.
  n <- distionary::dst_null()
  d <- distionary::dst_norm(0, 1)
  is_null_dst <- function(x) is.na(x)
  expect_true(is_null_dst(shift(n, 3)))
  expect_true(is_null_dst(multiply(n, 2)))
  expect_true(is_null_dst(flip(n)))
  expect_true(is_null_dst(exp(n)))
  expect_true(is_null_dst(invert(n)))
  expect_true(is_null_dst(mix(n, d)))
  expect_true(is_null_dst(maximise(n, d)))
  expect_true(is_null_dst(minimise(n, d)))
  expect_true(is_null_dst(trim_left(n, 0)))
  expect_true(is_null_dst(trim_right(n, 0)))
})

test_that("A distribution merely named Null is not treated as one.", {
  # distionary marks the Null distribution with a `null_dst` class, which is
  # what `is.na()` reads. Testing the pretty name instead would report TRUE
  # here and make every verb discard a perfectly good distribution.
  impostor <- distionary::distribution(
    cdf = function(x) stats::punif(x, 0, 1),
    density = function(x) stats::dunif(x, 0, 1),
    .name = "Null",
    .support = distionary::continuous(c(0, 1))
  )
  expect_false(is.na(impostor))
  expect_equal(distionary::eval_cdf(shift(impostor, 1), 1.5), 0.5)
  expect_equal(distionary::eval_cdf(multiply(impostor, 2), 1), 0.5)
  expect_equal(distionary::eval_cdf(flip(impostor), -0.5), 0.5)
  expect_false(is.na(trim_left(impostor, 0.25)))
})

test_that("A distribution cannot be built without a support.", {
  # distionary requires one, so a verb never meets an input lacking a support
  # (bar the Null distribution, handled above).
  expect_error(
    distionary::distribution(cdf = stats::pnorm, density = stats::dnorm),
    "needs a support"
  )
  # The old way of declaring a variable type is defunct rather than a
  # substitute for one.
  expect_error(
    distionary::distribution(
      cdf = stats::pnorm, density = stats::dnorm, .vtype = "continuous"
    ),
    class = "lifecycle_error_deprecated"
  )
})

test_that("Trimming on a flat region shifts the boundary to the support.", {
  # Support [1, 2] U [4, 5]: trimming left of 3 starts the result at 4.
  m <- mix(
    distionary::dst_unif(1, 2), distionary::dst_unif(4, 5),
    weights = c(1, 1)
  )
  tl <- trim_left(m, 3)
  expect_equal(range(tl), c(4, 5))
  expect_equal(distionary::eval_quantile(tl, c(0, 0.5)), c(4, 4.5))
  # Trimming exactly at an interval endpoint also lands on the next piece.
  expect_equal(distionary::eval_quantile(trim_left(m, 2), 0), 4)
  # Mirror image.
  tr <- trim_right(m, 3)
  expect_equal(range(tr), c(1, 2))
  expect_equal(distionary::eval_quantile(tr, 1), 2)
  # Discrete: a trim point between atoms shifts to the next atom.
  tp <- trim_left(distionary::dst_pois(5), 5.5)
  expect_equal(distionary::eval_quantile(tp, 0), 6)
  expect_equal(range(tp), c(6, Inf))
})

test_that("Trimming everything away gives a Null distribution.", {
  # Nothing survives the trim, so there is no distribution left to describe.
  expect_equal(
    distionary::pretty_name(trim_left(distionary::dst_unif(0, 1), 5)),
    "Null"
  )
  expect_equal(
    distionary::pretty_name(trim_right(distionary::dst_unif(0, 1), -5)),
    "Null"
  )
  # Discrete: `include = TRUE` removes `of` itself, taking the last atom.
  expect_equal(
    distionary::pretty_name(trim_right(distionary::dst_pois(3), 0)),
    "Null"
  )
  # Contrast: a trim that leaves something behind is not Null.
  expect_equal(
    distionary::pretty_name(trim_left(distionary::dst_pois(3), 0)),
    "Left-Trimmed"
  )
})

test_that("An empty restricted support also gives a Null distribution.", {
  # `trim_*()` checks the retained probability before it looks at the support,
  # so a trim that removes everything is normally caught there. The support
  # check behind it is reachable only when a distribution's `.support`
  # disagrees with its cdf, which `distribution()` does not verify.
  lo_liar <- distionary::distribution(
    cdf = function(x) stats::punif(x, 0, 10),
    density = function(x) stats::dunif(x, 0, 10),
    .support = distionary::continuous(c(0, 1))
  )
  # Half the cdf's mass sits above 5, so the probability check passes, but
  # the support restricted to [5, Inf) is empty.
  expect_equal(distionary::pretty_name(trim_left(lo_liar, 5)), "Null")
  hi_liar <- distionary::distribution(
    cdf = function(x) stats::punif(x, 0, 10),
    density = function(x) stats::dunif(x, 0, 10),
    .support = distionary::continuous(c(9, 10))
  )
  expect_equal(distionary::pretty_name(trim_right(hi_liar, 5)), "Null")
})

test_that("Reinstating a body across a threshold drops an empty side.", {
  core <- distionary::continuous(c(5, 10))
  body <- distionary::dst_unif(0, 1)
  # The body reaches below the threshold, so both pieces are kept.
  both <- reinstate_support(body, core, 5, "right")
  expect_equal(
    distionary::regions(both),
    distionary::regions(distionary::continuous(c(0, 1), c(5, 10)))
  )
  # The body lies entirely above the threshold, leaving nothing to reinstate.
  expect_equal(reinstate_support(body, core, -5, "right"), core)
})
