library(distionary)

## START test structure identical for mix, maximize, minimize -----

test_that("Mix - NA handling", {
  d1 <- dst_unif(0, 1)
  d2 <- dst_gamma(4, 2)
  d3 <- dst_pois(4)
  # Null distributions
  expect_equal(
    mix(d1, d2, dst_null(), na_action_dst = "null"),
    dst_null()
  )
  expect_equal(
    mix(d1, d2, dst_null(), na_action_dst = "drop"),
    mix(d1, d2, na_action_dst = "drop")
  )
  expect_equal(
    mix(d1, dst_null(), d2, weights = 1:3, na_action_dst = "drop"),
    mix(d1, d2, weights = c(1, 3), na_action_dst = "drop")
  )
  expect_error(mix(d1, d2, dst_null(), na_action_dst = "fail"))
  # NA weights
  expect_equal(
    mix(d1, d2, d3, weights = c(1, 1, NA), na_action_w = "null"),
    dst_null()
  )
  expect_equal(
    mix(d1, d2, d3, weights = c(1, 2, NA), na_action_w = "drop"),
    mix(d1, d2, weights = 1:2, na_action_w = "drop")
  )
  expect_equal(
    mix(d1, d2, d3, weights = c(1, NA, 2), na_action_w = "drop"),
    mix(d1, d3, weights = 1:2, na_action_w = "drop")
  )
  expect_error(mix(d1, d2, weights = c(3, NA), na_action_w = "fail"))
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
    mix(
      d1, dst_null(), dst_null(), d2, d3,
      weights = c(1, 2, NA, 3, NA),
      na_action_dst = "null",
      na_action_w = "null"
    ),
    dst_null()
  )
  expect_equal(
    mix(
      d1, dst_null(), dst_null(), d2, d3,
      weights = c(1, 2, NA, 3, NA),
      na_action_dst = "null",
      na_action_w = "drop"
    ),
    dst_null()
  )
  expect_error(
    mix(
      d1, dst_null(), dst_null(), d2, d3,
      weights = c(1, 2, NA, 3, NA),
      na_action_dst = "null",
      na_action_w = "fail"
    )
  )
  expect_equal(
    mix(
      d1, dst_null(), dst_null(), d2, d3,
      weights = c(1, 2, NA, 3, NA),
      na_action_dst = "drop",
      na_action_w = "null"
    ),
    dst_null()
  )
  expect_equal(
    mix(
      d1, dst_null(), dst_null(), d2, d3,
      weights = c(1, 2, NA, 3, NA),
      na_action_dst = "drop",
      na_action_w = "drop"
    ),
    mix(
      d1, d2,
      weights = c(1, 3),
      na_action_dst = "drop",
      na_action_w = "drop"
    )
  )
  expect_error(
    mix(
      d1, dst_null(), dst_null(), d2, d3,
      weights = c(1, 2, NA, 3, NA),
      na_action_dst = "drop",
      na_action_w = "fail"
    )
  )
  expect_error(
    mix(
      d1, dst_null(), dst_null(), d2, d3,
      weights = c(1, 2, NA, 3, NA),
      na_action_dst = "fail",
      na_action_w = "null"
    )
  )
  expect_error(
    mix(
      d1, dst_null(), dst_null(), d2, d3,
      weights = c(1, 2, NA, 3, NA),
      na_action_dst = "fail",
      na_action_w = "drop"
    )
  )
  expect_error(
    mix(
      d1, dst_null(), dst_null(), d2, d3,
      weights = c(1, 2, NA, 3, NA),
      na_action_dst = "fail",
      na_action_w = "fail"
    )
  )
  ## Fail takes precedence
  expect_error(
    mix(
      d1, dst_null(),
      weights = c(1, NA),
      na_action_dst = "fail",
      na_action_w = "drop"
    )
  )
  expect_error(
    mix(
      d1, dst_null(),
      weights = c(1, NA),
      na_action_dst = "fail",
      na_action_w = "null"
    )
  )
  expect_error(
    mix(
      d1, dst_null(),
      weights = c(1, NA),
      na_action_dst = "drop",
      na_action_w = "fail"
    )
  )
  expect_error(
    mix(
      d1, dst_null(),
      weights = c(1, NA),
      na_action_dst = "null",
      na_action_w = "fail"
    )
  )
  # Null takes precedence over drop
  expect_equal(
    mix(
      d1, dst_null(),
      weights = c(1, NA),
      na_action_dst = "null",
      na_action_w = "drop"
    ),
    dst_null()
  )
  expect_equal(
    mix(
      d1, dst_null(),
      weights = c(1, NA),
      na_action_dst = "drop",
      na_action_w = "null"
    ),
    dst_null()
  )
})

test_that("Mix - Edge cases", {
  d1 <- dst_exp(1.1)
  d2 <- dst_weibull(1.1, 2.0)
  d3 <- dst_norm(4, 5)
  d4 <- dst_unif(0, 1)
  expect_equal(mix(d1, weights = 1), d1)
  expect_equal(mix(d2, weights = 1), d2)
  expect_equal(mix(d3, weights = 1), d3)
  expect_equal(mix(d4, weights = 1), d4)
  expect_equal(mix(d1, d2, weights = 0:1), d2)
  expect_equal(mix(d2, weights = 0), dst_null())
  expect_equal(mix(d4, d2, weights = 0), dst_null())
  expect_equal(mix(), dst_null())
  expect_equal(mix(d1, weights = NA, na_action_w = "drop"), dst_null())
  expect_equal(mix(dst_null(), na_action_dst = "drop"), dst_null())
})

test_that("Mix - bad parameters", {
  expect_error(mix(dst_exp(1), weights = "a"))
  expect_error(mix(dst_exp(1), 1))
  expect_error(mix("a"))
})

test_that("Mix - Recycling", {
  # Picky recycling.
  expect_error(mix(dst_exp(1), dst_norm(0, 1), weights = 1:4))
  expect_error(mix(
    dst_exp(1), dst_norm(0, 1), dst_t(3), dst_norm(0, 3),
    weights = 1:2
  ))
  # Picky recycling takes precedence over collapsing inputs.
  expect_error(mix(
    dst_t(3), dst_norm(0, 1),
    weights = c(1, 2, 0, 0)
  ))
  expect_error(mix(
    dst_t(3), dst_t(3), dst_norm(0, 1), dst_norm(0, 1),
    weights = 1:2
  ))
  # Simple cases of recycling.
  expect_equal(
    mix(dst_exp(1), weights = 1:3),
    mix(dst_exp(1), dst_exp(1), dst_exp(1), weights = 1:3)
  )
  expect_equal(
    mix(dst_t(3), dst_gamma(2, 3), dst_pois(4), weights = 2.2),
    mix(dst_t(3), dst_gamma(2, 3), dst_pois(4), weights = c(2.2, 2.2, 2.2))
  )
})

test_that("Mix - Aggregation", {
  expect_equal(
    mix(dst_exp(1), dst_norm(0, 1), dst_exp(1)),
    mix(dst_exp(1), dst_norm(0, 1), weights = 2:1)
  )
  expect_equal(
    mix(dst_exp(1), dst_norm(0, 1), dst_exp(1), weights = 2),
    mix(dst_exp(1), dst_norm(0, 1), weights = c(4, 2))
  )
  expect_equal(
    mix(dst_exp(1), dst_norm(0, 1), dst_t(3), weights = c(2, 0, 3)),
    mix(dst_exp(1), dst_t(3), weights = c(2, 3))
  )
  expect_equal(
    mix(dst_exp(1), dst_norm(0, 1), dst_t(3), weights = 0),
    dst_null()
  )
  expect_equal(
    mix(dst_exp(1), weights = 1:3),
    mix(dst_exp(1), weights = sum(1:3))
  )
})

## END test structure identical for mix, maximize, minimize -----

test_that("Mix - Weights Normalization", {
  expect_equal(
    mix(dst_t(3), dst_gamma(2, 3), dst_pois(4), weights = 2),
    mix(dst_t(3), dst_gamma(2, 3), dst_pois(4))
  )
  expect_equal(mix(dst_t(3), weights = 2), dst_t(3))
})
