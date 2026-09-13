test_that("A graft is its two pieces, weighted by the body.", {
  body <- distionary::dst_pois(3)
  tail <- distionary::dst_pois(8)
  graft <- graft_right(body, of = 5, tail_absolute = tail)
  # The body's own probability of exceeding the knot is the tail's share.
  share <- 1 - stats::ppois(5, 3)
  body_piece <- trim_right(body, 5, knot_action = "keep")
  tail_piece <- trim_left(tail, 5, knot_action = "discard")
  at <- 0:30
  expect_equal(
    distionary::eval_cdf(graft, at = at),
    (1 - share) * distionary::eval_cdf(body_piece, at = at) +
      share * distionary::eval_cdf(tail_piece, at = at)
  )
})

test_that("An excess tail is the same as one placed by hand.", {
  body <- distionary::dst_norm(0, 1)
  u <- distionary::eval_quantile(body, at = 0.9)
  excess <- distionary::dst_gp(1, 0.3)
  at <- u + c(-1, 0, 0.5, 2, 10)
  expect_equal(
    distionary::eval_cdf(
      graft_right(body, of = u, tail_excess = excess),
      at = at
    ),
    distionary::eval_cdf(
      graft_right(body, of = u, tail_absolute = shift(excess, u)),
      at = at
    )
  )
})

test_that("An excess tail keeps its distance from the knot.", {
  # Excesses that start above zero --- an empirical set of them, say ---
  # are read from zero all the same, so the gap between zero and the first
  # of them carries over to the knot.
  body <- distionary::dst_norm(0, 1)
  excess <- distionary::dst_empirical(c(0.3, 1.2, 4.5))
  graft <- graft_right(body, of = 2, tail_excess = excess)
  expect_equal(range(graft), c(-Inf, 6.5))
  expect_true(distionary::eval_pmf(graft, at = 2.3) > 0)
})

test_that("Each `knot_action` gives a distribution.", {
  body <- distionary::dst_pois(3)
  tail <- distionary::dst_pois(8)
  for (action in c("discard", "keep", "split")) {
    graft <- graft_right(
      body,
      of = 5,
      tail_absolute = tail,
      knot_action = action
    )
    # Mass the body declines passes to the tail's share rather than
    # vanishing, so the total is 1 however the action is set.
    expect_equal(sum(distionary::eval_pmf(graft, at = 0:150)), 1)
  }
})

test_that("The graft leaves the body alone up to and including the knot.", {
  # The knot belongs to the body: the tail models what exceeds it. So under
  # the defaults every probability at or below the knot is the body's own,
  # unscaled, and the tail's share is the probability of exceeding it.
  body <- distionary::dst_pois(3)
  tail <- distionary::dst_pois(8)
  graft <- graft_right(body, of = 5, tail_absolute = tail)
  expect_equal(distionary::eval_pmf(graft, at = 0:5), stats::dpois(0:5, 3))
  expect_equal(distionary::eval_cdf(graft, at = 5), stats::ppois(5, 3))
  expect_equal(
    distionary::eval_survival(graft, at = 5),
    stats::ppois(5, 3, lower.tail = FALSE)
  )
})

test_that("The tail takes the opposite knot action to the body's.", {
  body <- distionary::dst_pois(3)
  tail <- distionary::dst_pois(8)
  knot_of <- function(action) {
    distionary::eval_pmf(
      graft_right(body, of = 5, tail_absolute = tail, knot_action = action),
      at = 5
    )
  }
  # Keeping, the knot is the body's own mass and none of the tail's.
  expect_equal(knot_of("keep"), stats::dpois(5, 3))
  # Discarding, the body gives the knot up and the tail keeps it: the
  # knot is the tail's atom, scaled into the tail's share.
  share <- stats::ppois(4, 3, lower.tail = FALSE)
  kept_by_tail <- stats::ppois(4, 8, lower.tail = FALSE)
  expect_equal(knot_of("discard"), share * stats::dpois(5, 8) / kept_by_tail)
  # Splitting, each side keeps half of its own, so the knot lands between
  # the two --- counted once either way, never twice and never lost.
  expect_true(knot_of("discard") < knot_of("split"))
  expect_true(knot_of("split") < knot_of("keep"))
  for (action in c("keep", "discard", "split")) {
    graft <- graft_right(
      body,
      of = 5,
      tail_absolute = tail,
      knot_action = action
    )
    expect_equal(sum(distionary::eval_pmf(graft, at = 0:250)), 1)
  }
})

test_that("An excess atom at zero follows the same rule.", {
  # An excess of zero is the event `X = of`. The body keeps the knot by
  # default, so that atom is conditioned away --- the convention under which
  # excesses are strictly positive. Asked to discard, the body hands the
  # knot over and the atom stays.
  body <- distionary::dst_pois(3)
  excess <- distionary::dst_finite(c(0, 1, 2), c(0.5, 0.3, 0.2))
  kept <- graft_right(body, of = 5, tail_excess = excess)
  expect_equal(distionary::eval_pmf(kept, at = 5), stats::dpois(5, 3))
  handed_over <- graft_right(
    body,
    of = 5,
    tail_excess = excess,
    knot_action = "discard"
  )
  share <- stats::ppois(4, 3, lower.tail = FALSE)
  expect_equal(distionary::eval_pmf(handed_over, at = 5), share * 0.5)
})

test_that("`knot_action` does nothing for a continuous body.", {
  body <- distionary::dst_norm(0, 1)
  u <- distionary::eval_quantile(body, at = 0.9)
  answers <- vapply(
    c("discard", "keep", "split"),
    function(action) {
      distionary::eval_cdf(
        graft_right(
          body,
          of = u,
          tail_excess = distionary::dst_gp(1, 0.3),
          knot_action = action
        ),
        at = u + 1
      )
    },
    FUN.VALUE = numeric(1L)
  )
  expect_equal(diff(range(answers)), 0)
})

test_that("A graft says it is a graft.", {
  body <- distionary::dst_norm(0, 1)
  graft <- graft_right(body, of = 2, tail_excess = distionary::dst_gp(1, 0.3))
  expect_equal(distionary::pretty_name(graft), "Graft")
  expect_s3_class(graft, "graft")
})

test_that("A graft with nothing to attach is the distribution itself.", {
  # Nothing of the body reaches past the knot, so there is no share for
  # the tail and the body comes back untouched.
  body <- distionary::dst_unif(0, 1)
  expect_equal(
    distionary::eval_cdf(
      graft_right(body, of = 3, tail_absolute = distionary::dst_unif(5, 6)),
      at = 0.5
    ),
    distionary::eval_cdf(body, at = 0.5)
  )
})

test_that("`graft_left()` mirrors `graft_right()`.", {
  body <- distionary::dst_pois(8)
  tail <- distionary::dst_pois(3)
  graft <- graft_left(body, of = 5, tail_absolute = tail)
  expect_equal(sum(distionary::eval_pmf(graft, at = 0:150)), 1)
  # The body keeps its atom at the knot by default, so the tail's share is
  # the probability of falling short of it: P(body < 5), not P(body <= 5).
  share <- stats::ppois(4, 8)
  body_piece <- trim_left(body, 5, knot_action = "keep")
  tail_piece <- trim_right(tail, 5, knot_action = "discard")
  at <- 0:30
  expect_equal(
    distionary::eval_cdf(graft, at = at),
    share * distionary::eval_cdf(tail_piece, at = at) +
      (1 - share) * distionary::eval_cdf(body_piece, at = at)
  )
})

test_that("`graft_left()` takes its excesses below zero.", {
  body <- distionary::dst_norm(0, 1)
  excess <- flip(distionary::dst_gp(1, 0.3))
  graft <- graft_left(body, of = -2, tail_excess = excess)
  # The knot is where the body's own probability of reaching it says.
  expect_equal(distionary::eval_cdf(graft, at = -2), stats::pnorm(-2))
  # An excess reaching above zero is the wrong side for a left graft.
  expect_error(
    graft_left(body, of = -2, tail_excess = distionary::dst_gp(1, 0.3)),
    "above zero"
  )
})

test_that("The tail's scale has to be named, and only one of the two.", {
  body <- distionary::dst_norm(0, 1)
  excess <- distionary::dst_gp(1, 0.3)
  expect_error(graft_right(body, of = 2), "Name one of")
  expect_error(
    graft_right(body, of = 2, tail_excess = excess, tail_absolute = excess),
    "Name one of"
  )
  expect_error(graft_left(body, of = 2), "Name one of")
})

test_that("An excess tail reaching past the knot is refused.", {
  body <- distionary::dst_norm(0, 1)
  expect_error(
    graft_right(body, of = 2, tail_excess = distionary::dst_norm(0, 1)),
    "below zero"
  )
})

test_that("An excess tail already sitting at the knot is warned about.", {
  # The fingerprint of a fit whose location is the threshold rather than
  # zero: moving it to the knot would start the tail at twice the knot.
  body <- distionary::dst_norm(0, 1)
  placed <- shift(distionary::dst_gp(1, 0.3), 2)
  expect_warning(
    graft_right(body, of = 2, tail_excess = placed),
    "already starts at"
  )
  # A knot of zero is not that mistake: every excess model starts there.
  expect_silent(
    graft_right(body, of = 0, tail_excess = distionary::dst_gp(1, 0.3))
  )
})

test_that("An unknown knot action is refused.", {
  body <- distionary::dst_norm(0, 1)
  excess <- distionary::dst_gp(1, 0.3)
  expect_error(
    graft_right(body, of = 2, tail_excess = excess, knot_action = "halve")
  )
  expect_error(
    graft_right(body, of = 2, tail_excess = excess, knot_action = TRUE)
  )
})
