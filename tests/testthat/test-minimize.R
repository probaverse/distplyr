library(distionary)

test_that("Minimize - NA handling", {
  d1 <- dst_unif(0, 1)
  d2 <- dst_gamma(4, 2)
  d3 <- dst_pois(4)
  # Null distributions
  expect_equal(
    minimize(d1, d2, dst_null(), na_action_dst = "null"),
    dst_null()
  )
  expect_equal(
    minimize(d1, d2, dst_null(), na_action_dst = "drop"),
    minimize(d1, d2, na_action_dst = "drop")
  )
  expect_equal(
    minimize(d1, dst_null(), d2, draws = 1:3, na_action_dst = "drop"),
    minimize(d1, d2, draws = c(1, 3), na_action_dst = "drop")
  )
  expect_error(minimize(d1, d2, dst_null(), na_action_dst = "fail"))
  # NA draws
  expect_equal(
    minimize(d1, d2, d3, draws = c(1, 1, NA), na_action_draws = "null"),
    dst_null()
  )
  expect_equal(
    minimize(d1, d2, d3, draws = c(1, 2, NA), na_action_draws = "drop"),
    minimize(d1, d2, draws = 1:2, na_action_draws = "drop")
  )
  expect_equal(
    minimize(d1, d2, d3, draws = c(1, NA, 2), na_action_draws = "drop"),
    minimize(d1, d3, draws = 1:2, na_action_draws = "drop")
  )
  expect_error(minimize(d1, d2, draws = c(3, NA), na_action_draws = "fail"))
  # Both
  # --> null-null
  # --> null-drop
  # --> null-fail
  # --> drop-null
  # --> drop-drop
  # --> drop-fail
  # --> fail-null
  # --> fail-drop
  # --> fail-fail
  expect_equal(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "null",
      na_action_draws = "null"
    ),
    dst_null()
  )
  expect_equal(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "null",
      na_action_draws = "drop"
    ),
    dst_null()
  )
  expect_error(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "null",
      na_action_draws = "fail"
    )
  )
  expect_equal(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "drop",
      na_action_draws = "null"
    ),
    dst_null()
  )
  expect_equal(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "drop",
      na_action_draws = "drop"
    ),
    minimize(
      d1, d2,
      draws = c(1, 3),
      na_action_dst = "drop",
      na_action_draws = "drop"
    )
  )
  expect_error(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "drop",
      na_action_draws = "fail"
    )
  )
  expect_error(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "fail",
      na_action_draws = "null"
    )
  )
  expect_error(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "fail",
      na_action_draws = "drop"
    )
  )
  expect_error(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "fail",
      na_action_draws = "fail"
    )
  )
  ## Fail takes precedence
  expect_error(
    minimize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "fail",
      na_action_draws = "drop"
    )
  )
  expect_error(
    minimize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "fail",
      na_action_draws = "null"
    )
  )
  expect_error(
    minimize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "drop",
      na_action_draws = "fail"
    )
  )
  expect_error(
    minimize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "null",
      na_action_draws = "fail"
    )
  )
  # Null takes precedence over drop
  expect_equal(
    minimize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "null",
      na_action_draws = "drop"
    ),
    dst_null()
  )
  expect_equal(
    minimize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "drop",
      na_action_draws = "null"
    ),
    dst_null()
  )

















  expect_equal(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "null",
      na_action_draws = "null"
    ),
    dst_null()
  )
  expect_error(
    minimize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_draws = "fail",
      na_action_dst = "fail"
    )
  )
})

test_that("Minimize - Edge cases", {
  d1 <- dst_exp(1.1)
  d2 <- dst_weibull(1.1, 2.0)
  d3 <- dst_norm(4, 5)
  d4 <- dst_unif(0, 1)
  expect_equal(minimize(d1, draws = 1), d1)
  expect_equal(minimize(d2, draws = 1), d2)
  expect_equal(minimize(d3, draws = 1), d3)
  expect_equal(minimize(d4, draws = 1), d4)
  expect_equal(minimize(d1, d2, draws = 0:1), d2)
  expect_equal(minimize(d2, draws = 0), dst_null())
  expect_equal(minimize(d4, d2, draws = 0), dst_null())
})
