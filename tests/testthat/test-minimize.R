library(distionary)

## START test structure identical for mix, maximize, minimize -----

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
  expect_equal(minimize(), dst_null())
  expect_equal(minimize(d1, draws = NA, na_action_draws = "drop"), dst_null())
  expect_equal(minimize(dst_null(), na_action_dst = "drop"), dst_null())
})

test_that("Minimize - bad parameters", {
  expect_error(minimize(dst_exp(1), draws = "a"))
  expect_error(minimize(dst_exp(1), 1))
  expect_error(minimize("a"))
})

test_that("Minimize - Recycling", {
  # Picky recycling.
  expect_error(minimize(dst_exp(1), dst_norm(0, 1), draws = 1:4))
  expect_error(minimize(
    dst_exp(1), dst_norm(0, 1), dst_t(3), dst_norm(0, 3),
    draws = 1:2
  ))
  # Picky recycling takes precedence over collapsing inputs.
  expect_error(minimize(
    dst_t(3), dst_norm(0, 1),
    draws = c(1, 2, 0, 0)
  ))
  expect_error(minimize(
    dst_t(3), dst_t(3), dst_norm(0, 1), dst_norm(0, 1),
    draws = 1:2
  ))
  # Simple cases of recycling.
  expect_equal(
    minimize(dst_exp(1), draws = 1:3),
    minimize(dst_exp(1), dst_exp(1), dst_exp(1), draws = 1:3)
  )
  expect_equal(
    minimize(dst_t(3), dst_gamma(2, 3), dst_pois(4), draws = 2),
    minimize(dst_t(3), dst_gamma(2, 3), dst_pois(4), draws = c(2, 2, 2))
  )
})

test_that("Minimize - Aggregation", {
  expect_equal(
    minimize(dst_exp(1), dst_norm(0, 1), dst_exp(1)),
    minimize(dst_exp(1), dst_norm(0, 1), draws = 2:1)
  )
  expect_equal(
    minimize(dst_exp(1), dst_norm(0, 1), dst_exp(1), draws = 2),
    minimize(dst_exp(1), dst_norm(0, 1), draws = c(4, 2))
  )
  expect_equal(
    minimize(dst_exp(1), dst_norm(0, 1), dst_t(3), draws = c(2, 0, 3)),
    minimize(dst_exp(1), dst_t(3), draws = c(2, 3))
  )
  expect_equal(
    minimize(dst_exp(1), dst_norm(0, 1), dst_t(3), draws = 0),
    dst_null()
  )
  expect_equal(
    minimize(dst_exp(1), draws = 1:3),
    minimize(dst_exp(1), draws = sum(1:3))
  )
})

## END test structure identical for mix, maximize, minimize -----

test_that("Minimize - Edge cases", {
  # Draws = 1
  expect_equal(minimize(dst_gamma(1, 3)), dst_gamma(1, 3))
  # Distributions fully to the left of others are removed
  # --> Continuous distribution leading
  expect_equal(
    minimize(
      -dst_gamma(1, 3), dst_unif(0, 3), dst_unif(2, 4), dst_degenerate(0),
      draws = c(2, 3, 1, 2)
    ),
    minimize(
      -dst_gamma(1, 3),
      draws = 2
    )
  )
  # --> Discrete distribution leading
  expect_equal(
    minimize(
      -dst_pois(1), dst_unif(0, 3), dst_unif(2, 4), dst_degenerate(0),
      draws = c(2, 3, 1, 2)
    ),
    minimize(
      -dst_pois(1),
      draws = 2
    )
  )
  # --> Degenerate distribution leading
  expect_equal(
    minimize(
      dst_degenerate(0), dst_unif(0, 3), dst_unif(2, 4), dst_pois(3),
      draws = c(2, 3, 1, 2)
    ),
    dst_degenerate(0)
  )
  # --> Mixed mode distribution leading
  expect_equal(
    minimize(
      mix(dst_degenerate(0), -dst_exp(1)), dst_unif(0, 3), dst_pois(3)
    ),
    mix(dst_degenerate(0), -dst_exp(1))
  )
  # --> Two distributions leading
  expect_equal(
    minimize(
      -dst_gamma(1, 3), -dst_pois(1), dst_unif(0, 3), dst_degenerate(0),
      draws = c(2, 3, 1, 2)
    ),
    minimize(
      -dst_gamma(1, 3), -dst_pois(1),
      draws = c(2, 3)
    )
  )
  # Culling distributions outside of the range of the new distribution
  # happens first.
  expect_equal(
    pretty_name(
      minimize(dst_unif(20, 30), dst_empirical(0:5), dst_empirical(c(0, 3, 10)))
    ),
    "Finite"
  )
  # Just to be sure: these simplifications don't ruin the ability to accept
  # distributions all having left-endpoint of Inf.
  expect_equal(
    parameters(minimize(dst_norm(0, 1), dst_t(3), dst_exp(1)))$distributions,
    list(dst_norm(0, 1), dst_t(3), dst_exp(1))
  )
})

test_that("Minimize - vtype", {
  expect_equal(vtype(minimize(dst_gamma(2, 3), dst_exp(3))), "continuous")
  expect_equal(vtype(minimize(dst_pois(3), dst_nbinom(3, 0.4))), "discrete")
  expect_equal(vtype(minimize(dst_pois(3), -dst_exp(1))), "continuous")
  # Here's one that should be continuous but don't have the capability
  # currently to know this.
  m <- mix(-dst_binom(5, 0.5), -dst_gp(10, 1))
  d <- minimize(m, -dst_exp(1) - 6)
  expect_equal(vtype(d), "unknown")
})
