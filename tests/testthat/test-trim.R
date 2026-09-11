test_that("`knot_action` decides the fate of mass sitting on the knot.", {
  # A Poisson has an atom at 5, so the three actions are distinguishable.
  d <- distionary::dst_pois(3)
  m <- distionary::eval_pmf(d, at = 5)
  above <- 1 - stats::ppois(5, 3)
  for (action in c("discard", "keep", "split")) {
    retained <- switch(action, discard = 0, keep = m, split = m / 2)
    trimmed <- trim_left(d, 5, knot_action = action)
    expect_equal(
      distionary::eval_pmf(trimmed, at = 5),
      retained / (above + retained)
    )
    # Whatever is retained, the result is still a distribution.
    expect_equal(sum(distionary::eval_pmf(trimmed, at = 0:80)), 1)
  }
})

test_that("`split` keeps exactly half the knot, the mid-p convention.", {
  d <- distionary::dst_pois(3)
  kept <- distionary::eval_pmf(trim_left(d, 5, knot_action = "keep"), at = 5)
  split <- distionary::eval_pmf(trim_left(d, 5, knot_action = "split"), at = 5)
  # Not half the *reported* mass, since the two renormalise differently;
  # half of the knot before renormalising.
  m <- distionary::eval_pmf(d, at = 5)
  above <- 1 - stats::ppois(5, 3)
  expect_equal(split, (m / 2) / (above + m / 2))
  expect_equal(kept, m / (above + m))
  expect_true(split < kept)
})

test_that("The trimmed distribution is internally consistent.", {
  d <- distionary::dst_pois(3)
  at <- 0:20
  for (action in c("discard", "keep", "split")) {
    trimmed <- trim_left(d, 5, knot_action = action)
    cdf <- distionary::eval_cdf(trimmed, at = at)
    expect_equal(cdf, cumsum(distionary::eval_pmf(trimmed, at = at)))
    expect_equal(
      cdf + distionary::eval_survival(trimmed, at = at),
      rep(1, length(at))
    )
    # The quantile must reach at least as far as the probability asked for.
    p <- c(0.01, 0.25, 0.5, 0.9, 0.99)
    expect_true(all(
      distionary::eval_cdf(trimmed, at = distionary::eval_quantile(
        trimmed,
        at = p
      )) >= p - 1e-9
    ))
  }
})

test_that("The knot stays in the support when any of its mass is kept.", {
  d <- distionary::dst_pois(3)
  lower <- function(action) {
    range(distionary::support(trim_left(d, 5, knot_action = action)))[[1L]]
  }
  expect_equal(lower("discard"), 6)
  expect_equal(lower("keep"), 5)
  expect_equal(lower("split"), 5)
})

test_that("`knot_action` does nothing where the knot carries no mass.", {
  # Nothing to divide in a continuous distribution, so all three agree.
  d <- distionary::dst_norm(0, 1)
  answers <- vapply(
    c("discard", "keep", "split"),
    function(a) distionary::eval_cdf(trim_left(d, 0, knot_action = a), at = 1),
    FUN.VALUE = numeric(1L)
  )
  expect_equal(diff(range(answers)), 0)
})

test_that("`knot_action` reaches finite and mixture distributions.", {
  finite <- distionary::dst_empirical(c(1, 2, 3), weights = c(0.2, 0.5, 0.3))
  for (action in c("discard", "keep", "split")) {
    share <- switch(action, discard = 0, keep = 1, split = 0.5)
    trimmed <- trim_left(finite, 2, knot_action = action)
    expect_equal(
      distionary::eval_pmf(trimmed, at = 2),
      (0.5 * share) / (0.5 * share + 0.3)
    )
  }
  mixture <- mix(
    distionary::dst_pois(3),
    distionary::dst_pois(8),
    weights = c(0.5, 0.5)
  )
  knot <- 0.5 * stats::dpois(5, 3) + 0.5 * stats::dpois(5, 8)
  above <- 0.5 * (1 - stats::ppois(5, 3)) + 0.5 * (1 - stats::ppois(5, 8))
  trimmed <- trim_left(mixture, 5, knot_action = "split")
  expect_equal(
    distionary::eval_pmf(trimmed, at = 5),
    (knot / 2) / (above + knot / 2)
  )
  expect_equal(sum(distionary::eval_pmf(trimmed, at = 0:120)), 1)
})

test_that("`trim_right()` mirrors `trim_left()` at the knot.", {
  d <- distionary::dst_pois(3)
  m <- distionary::eval_pmf(d, at = 5)
  below <- stats::ppois(4, 3)
  for (action in c("discard", "keep", "split")) {
    retained <- switch(action, discard = 0, keep = m, split = m / 2)
    trimmed <- trim_right(d, 5, knot_action = action)
    expect_equal(
      distionary::eval_pmf(trimmed, at = 5),
      retained / (below + retained)
    )
    expect_equal(sum(distionary::eval_pmf(trimmed, at = 0:80)), 1)
  }
})

test_that("An unknown `knot_action` is refused.", {
  d <- distionary::dst_pois(3)
  expect_error(trim_left(d, 5, knot_action = "halve"))
  expect_error(trim_right(d, 5, knot_action = TRUE))
})
