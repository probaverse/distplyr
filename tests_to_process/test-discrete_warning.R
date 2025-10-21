test_that("Entering a discrete distribution into each verb issues a warning.", {
  pois <- distionary::dst_pois(2)
  expo <- distionary::dst_exp(2)
  degen <- distionary::dst_degenerate(2)
  expect_warning(exp(pois))
  expect_warning(flip(pois))
  graft_right(pois, expo, breakpoint = 2) |>
    expect_warning() |>
    expect_warning() |>
    expect_warning()
  graft_right(expo, pois, breakpoint = 2) |>
    expect_warning() |>
    expect_warning() |>
    expect_warning()
  graft_left(pois, expo, breakpoint = 2) |>
    expect_warning() |>
    expect_warning() |>
    expect_warning()
  graft_left(expo, pois, breakpoint = 2) |>
    expect_warning() |>
    expect_warning() |>
    expect_warning()
  expect_warning(invert(degen))
  expect_warning(log(degen))
  expect_warning(maximize(pois, expo))
  expect_warning(minimize(pois, expo))
  expect_warning(mix(pois, expo))
  expect_warning(multiply(pois, 4))
  expect_warning(shift(pois, 2))
  expect_warning(slice_left(pois, breakpoint = 3))
  expect_warning(slice_right(pois, breakpoint = 3))
})
