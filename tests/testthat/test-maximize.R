library(distionary)

## START test structure identical for mix, maximize, minimize -----

test_that("Maximize - NA handling", {
  d1 <- dst_unif(0, 1)
  d2 <- dst_gamma(4, 2)
  d3 <- dst_pois(4)
  # Null distributions
  expect_equal(
    maximize(d1, d2, dst_null(), na_action_dst = "null"),
    dst_null()
  )
  expect_equal(
    maximize(d1, d2, dst_null(), na_action_dst = "drop"),
    maximize(d1, d2, na_action_dst = "drop")
  )
  expect_equal(
    maximize(d1, dst_null(), d2, draws = 1:3, na_action_dst = "drop"),
    maximize(d1, d2, draws = c(1, 3), na_action_dst = "drop")
  )
  expect_error(maximize(d1, d2, dst_null(), na_action_dst = "fail"))
  # NA draws
  expect_equal(
    maximize(d1, d2, d3, draws = c(1, 1, NA), na_action_draws = "null"),
    dst_null()
  )
  expect_equal(
    maximize(d1, d2, d3, draws = c(1, 2, NA), na_action_draws = "drop"),
    maximize(d1, d2, draws = 1:2, na_action_draws = "drop")
  )
  expect_equal(
    maximize(d1, d2, d3, draws = c(1, NA, 2), na_action_draws = "drop"),
    maximize(d1, d3, draws = 1:2, na_action_draws = "drop")
  )
  expect_error(maximize(d1, d2, draws = c(3, NA), na_action_draws = "fail"))
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
    maximize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "null",
      na_action_draws = "null"
    ),
    dst_null()
  )
  expect_equal(
    maximize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "null",
      na_action_draws = "drop"
    ),
    dst_null()
  )
  expect_error(
    maximize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "null",
      na_action_draws = "fail"
    )
  )
  expect_equal(
    maximize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "drop",
      na_action_draws = "null"
    ),
    dst_null()
  )
  expect_equal(
    maximize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "drop",
      na_action_draws = "drop"
    ),
    maximize(
      d1, d2,
      draws = c(1, 3),
      na_action_dst = "drop",
      na_action_draws = "drop"
    )
  )
  expect_error(
    maximize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "drop",
      na_action_draws = "fail"
    )
  )
  expect_error(
    maximize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "fail",
      na_action_draws = "null"
    )
  )
  expect_error(
    maximize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "fail",
      na_action_draws = "drop"
    )
  )
  expect_error(
    maximize(
      d1, dst_null(), dst_null(), d2, d3,
      draws = c(1, 2, NA, 3, NA),
      na_action_dst = "fail",
      na_action_draws = "fail"
    )
  )
  ## Fail takes precedence
  expect_error(
    maximize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "fail",
      na_action_draws = "drop"
    )
  )
  expect_error(
    maximize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "fail",
      na_action_draws = "null"
    )
  )
  expect_error(
    maximize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "drop",
      na_action_draws = "fail"
    )
  )
  expect_error(
    maximize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "null",
      na_action_draws = "fail"
    )
  )
  # Null takes precedence over drop
  expect_equal(
    maximize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "null",
      na_action_draws = "drop"
    ),
    dst_null()
  )
  expect_equal(
    maximize(
      d1, dst_null(),
      draws = c(1, NA),
      na_action_dst = "drop",
      na_action_draws = "null"
    ),
    dst_null()
  )
})

test_that("Maximize - Edge cases", {
  d1 <- dst_exp(1.1)
  d2 <- dst_weibull(1.1, 2.0)
  d3 <- dst_norm(4, 5)
  d4 <- dst_unif(0, 1)
  expect_equal(maximize(d1, draws = 1), d1)
  expect_equal(maximize(d2, draws = 1), d2)
  expect_equal(maximize(d3, draws = 1), d3)
  expect_equal(maximize(d4, draws = 1), d4)
  expect_equal(maximize(d1, d2, draws = 0:1), d2)
  expect_equal(maximize(d2, draws = 0), dst_null())
  expect_equal(maximize(d4, d2, draws = 0), dst_null())
  expect_equal(maximize(), dst_null())
  expect_equal(maximize(d1, draws = NA, na_action_draws = "drop"), dst_null())
  expect_equal(maximize(dst_null(), na_action_dst = "drop"), dst_null())
})

test_that("Maximize - bad parameters", {
  expect_error(maximize(dst_exp(1), draws = "a"))
  expect_error(maximize(dst_exp(1), 1))
  expect_error(maximize("a"))
})

test_that("Maximize - Recycling", {
  # Picky recycling.
  expect_error(maximize(dst_exp(1), dst_norm(0, 1), draws = 1:4))
  expect_error(maximize(
    dst_exp(1), dst_norm(0, 1), dst_t(3), dst_norm(0, 3),
    draws = 1:2
  ))
  # Picky recycling takes precedence over collapsing inputs.
  expect_error(maximize(
    dst_t(3), dst_norm(0, 1),
    draws = c(1, 2, 0, 0)
  ))
  expect_error(maximize(
    dst_t(3), dst_t(3), dst_norm(0, 1), dst_norm(0, 1),
    draws = 1:2
  ))
  # Simple cases of recycling.
  expect_equal(
    maximize(dst_exp(1), draws = 1:3),
    maximize(dst_exp(1), dst_exp(1), dst_exp(1), draws = 1:3)
  )
  expect_equal(
    maximize(dst_t(3), dst_gamma(2, 3), dst_pois(4), draws = 2),
    maximize(dst_t(3), dst_gamma(2, 3), dst_pois(4), draws = c(2, 2, 2))
  )
})

test_that("Maximize - Aggregation", {
  expect_equal(
    maximize(dst_exp(1), dst_norm(0, 1), dst_exp(1)),
    maximize(dst_exp(1), dst_norm(0, 1), draws = 2:1)
  )
  expect_equal(
    maximize(dst_exp(1), dst_norm(0, 1), dst_exp(1), draws = 2),
    maximize(dst_exp(1), dst_norm(0, 1), draws = c(4, 2))
  )
  expect_equal(
    maximize(dst_exp(1), dst_norm(0, 1), dst_t(3), draws = c(2, 0, 3)),
    maximize(dst_exp(1), dst_t(3), draws = c(2, 3))
  )
  expect_equal(
    maximize(dst_exp(1), dst_norm(0, 1), dst_t(3), draws = 0),
    dst_null()
  )
  expect_equal(
    maximize(dst_exp(1), draws = 1:3),
    maximize(dst_exp(1), draws = sum(1:3))
  )
})

## END test structure identical for mix, maximize, minimize -----
