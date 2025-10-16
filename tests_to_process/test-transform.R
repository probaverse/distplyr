test_that("Transform norm works", {
	expect_identical(
		distionary::dst_norm(2, 4) + 5,
		distionary::dst_norm(7, 4))
	expect_identical(
		distionary::dst_norm(0, 54) + 0,
		distionary::dst_norm(0, 54))
	expect_identical(
		distionary::dst_norm(2, 4) - 5,
		distionary::dst_norm(-3, 4))
	expect_identical(
		distionary::dst_norm(0, 1) - 0,
		distionary::dst_norm(0, 1))
	expect_identical(
		distionary::dst_norm(1, 2) * 2,
		distionary::dst_norm(1, 4))
	expect_identical(
		distionary::dst_norm(0, 1) * 0,
		distionary::dst_degenerate(0))
	expect_identical(
		distionary::dst_norm(1, 2) / 2,
		distionary::dst_norm(1, 1))
	expect_identical(
		distionary::dst_norm(1, 2) / 4,
		distionary::dst_norm(1, 0.5))
})

test_that("Transform unif works", {
	expect_identical(
		distionary::dst_unif(2, 4) + 5,
		distionary::dst_unif(7, 9))
	expect_identical(
		distionary::dst_unif(0, 54) + 0,
		distionary::dst_unif(0, 54))
	expect_identical(
		distionary::dst_unif(2, 4) - 5,
		distionary::dst_unif(-3, -1))
	expect_identical(
		distionary::dst_unif(2, 4) - 4,
		distionary::dst_unif(-2, 0))
	expect_identical(
		distionary::dst_unif(2, 4) * 5,
		distionary::dst_unif(2 * 5, 20))
	expect_identical(
		distionary::dst_unif(2, 4) * 0.75,
		distionary::dst_unif(2 * 0.75, 4 * 0.75))
	expect_identical(
		distionary::dst_unif(1, 4) / 2,
		distionary::dst_unif(1 / 2, 2))
	expect_identical(
		distionary::dst_unif(2, 80) / 5,
		distionary::dst_unif(2 / 5, 16))
})

test_that("Transform degenerate works", {
	expect_identical(
		distionary::dst_degenerate(2) + 5,
		distionary::dst_degenerate(7))
	expect_identical(
		distionary::dst_degenerate(0) + 0,
		distionary::dst_degenerate(0))
	expect_identical(
		distionary::dst_degenerate(-2) - 5,
		distionary::dst_degenerate(-7))
	expect_identical(
		distionary::dst_degenerate(2) - 2,
		distionary::dst_degenerate(0))
	expect_identical(
		distionary::dst_degenerate(-2) * 5,
		distionary::dst_degenerate(-2 * 5))
	expect_identical(
		distionary::dst_degenerate(-2) * 65,
		distionary::dst_degenerate(-2 * 65))
	expect_identical(
		distionary::dst_degenerate(4) * 5,
		distionary::dst_degenerate(20))
	expect_identical(
		distionary::dst_degenerate(-2) / 5,
		distionary::dst_degenerate(-2 / 5))
	expect_identical(
		distionary::dst_degenerate(4) / 5,
		distionary::dst_degenerate(4 / 5))
})


test_that("Transform finite works", {
  fin <- dst_finite(1:5, rep(0.2, 5))
  expect_equal(fin + 5, dst_finite(6:10, rep(0.2, 5)))
  expect_equal(fin + 0, fin)

  expect_equal(fin - 5, dst_finite(-4:0, rep(0.2, 5)))
  expect_equal(fin - 1, dst_finite(0:4, rep(0.2, 5)))

  expect_equal(fin * 5, dst_finite(1:5 * 5, rep(0.2, 5)))
  expect_equal(fin * 0.2, dst_finite(1:5 * 0.2, rep(0.2, 5)))

  expect_equal(fin / 5, dst_finite(1:5 / 5, rep(0.2, 5)))
  expect_equal(fin / 1, dst_finite(1:5, rep(0.2, 5)))
})

test_that("Transform GP works", {
  expect_identical(
		distionary::dst_gp(2, 4, 6) + 5,
		distionary::dst_gp(7, 4, 6))
	expect_identical(
		distionary::dst_gp(0, 54, 5) + 0,
		distionary::dst_gp(0, 54, 5))
	expect_identical(
		distionary::dst_gp(2, 4, 9) - 5,
		distionary::dst_gp(-3, 4, 9))
	expect_identical(
		distionary::dst_gp(0, 1, -54) - 0,
		distionary::dst_gp(0, 1, -54))
	expect_identical(
		distionary::dst_gp(1, 2, 54) * 2,
		distionary::dst_gp(1 * 2, 4, 54))
	expect_identical(
		distionary::dst_gp(0, 1, 12) * 8,
		distionary::dst_gp(0, 8, 12))
	expect_identical(
		distionary::dst_gp(1, 2, 4) / 2,
		distionary::dst_gp(1 / 2, 1, 4))
	expect_identical(
		distionary::dst_gp(1, 2, 9) / 4,
		distionary::dst_gp(1 / 4, 0.5, 9))
})
