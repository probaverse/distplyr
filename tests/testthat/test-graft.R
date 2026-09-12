test_that("A graft is its two trimmed pieces, weighted by the body.", {
  body <- distionary::dst_pois(3)
  tail <- distionary::dst_pois(8)
  graft <- graft_right(body, tail, of = 5)
  # The body's own probability of reaching the knot is the tail's share.
  share <- 1 - stats::ppois(4, 3)
  body_piece <- trim_right(body, 5, knot_action = "discard")
  tail_piece <- trim_left(tail, 5, knot_action = "keep")
  at <- 0:30
  expect_equal(
    distionary::eval_cdf(graft, at = at),
    (1 - share) * distionary::eval_cdf(body_piece, at = at) +
      share * distionary::eval_cdf(tail_piece, at = at)
  )
})

test_that("Every pairing of the two knot actions gives a distribution.", {
  body <- distionary::dst_pois(3)
  tail <- distionary::dst_pois(8)
  for (b in c("discard", "keep", "split")) {
    for (t in c("keep", "discard", "split")) {
      graft <- graft_right(
        body, tail,
        of = 5,
        knot_body_action = b,
        knot_tail_action = t
      )
      # Mass the body declines passes to the tail's share rather than
      # vanishing, so the total is 1 however the two are set.
      expect_equal(sum(distionary::eval_pmf(graft, at = 0:150)), 1)
    }
  }
})

test_that("The knot's mass follows the two actions.", {
  body <- distionary::dst_pois(3)
  tail <- distionary::dst_pois(8)
  knot_of <- function(b, t) {
    distionary::eval_pmf(
      graft_right(
        body, tail,
        of = 5,
        knot_body_action = b,
        knot_tail_action = t
      ),
      at = 5
    )
  }
  # Neither side taking the knot leaves no mass on it at all.
  expect_equal(knot_of("discard", "discard"), 0)
  # Both taking it puts more there than either alone.
  expect_true(knot_of("keep", "keep") > knot_of("keep", "discard"))
  expect_true(knot_of("keep", "keep") > knot_of("discard", "keep"))
  # Splitting sits between discarding and keeping, on either side.
  expect_true(
    knot_of("discard", "discard") <
      knot_of("split", "discard") &&
      knot_of("split", "discard") < knot_of("keep", "discard")
  )
})

test_that("The knot actions do nothing for continuous distributions.", {
  body <- distionary::dst_norm(0, 1)
  q <- distionary::eval_quantile(body, at = 0.9)
  tail <- q + distionary::dst_gp(1, 0.3)
  answers <- c()
  for (b in c("discard", "keep", "split")) {
    for (t in c("keep", "discard", "split")) {
      answers <- c(answers, distionary::eval_cdf(
        graft_right(
          body, tail,
          of = q,
          knot_body_action = b,
          knot_tail_action = t
        ),
        at = q + 1
      ))
    }
  }
  expect_equal(diff(range(answers)), 0)
})

test_that("A graft says it is a graft.", {
  body <- distionary::dst_norm(0, 1)
  tail <- 2 + distionary::dst_gp(1, 0.3)
  graft <- graft_right(body, tail, of = 2)
  expect_equal(distionary::pretty_name(graft), "Graft")
  expect_s3_class(graft, "graft")
})

test_that("A graft with nothing to attach is the distribution itself.", {
  # Nothing of the body reaches past the knot, so there is no share for
  # the tail and the body comes back untouched.
  body <- distionary::dst_unif(0, 1)
  tail <- distionary::dst_unif(5, 6)
  expect_equal(
    distionary::eval_cdf(graft_right(body, tail, of = 3), at = 0.5),
    distionary::eval_cdf(body, at = 0.5)
  )
})

test_that("`graft_left()` mirrors `graft_right()`.", {
  body <- distionary::dst_pois(8)
  tail <- distionary::dst_pois(3)
  graft <- graft_left(body, tail, of = 5)
  expect_equal(sum(distionary::eval_pmf(graft, at = 0:150)), 1)
  # The body discards its atom at the knot by default, so that mass
  # goes to the tail's share: P(body <= 5), not P(body < 5).
  share <- stats::ppois(5, 8)
  body_piece <- trim_left(body, 5, knot_action = "discard")
  tail_piece <- trim_right(tail, 5, knot_action = "keep")
  at <- 0:30
  expect_equal(
    distionary::eval_cdf(graft, at = at),
    share * distionary::eval_cdf(tail_piece, at = at) +
      (1 - share) * distionary::eval_cdf(body_piece, at = at)
  )
})

test_that("An unknown knot action is refused.", {
  body <- distionary::dst_norm(0, 1)
  tail <- 2 + distionary::dst_gp(1, 0.3)
  expect_error(graft_right(body, tail, of = 2, knot_body_action = "halve"))
  expect_error(graft_right(body, tail, of = 2, knot_tail_action = TRUE))
})
