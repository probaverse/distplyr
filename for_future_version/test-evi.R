a <- dst_unif(0, 1)
b <- dst_gpd(0, 1, 1)
c <- dst_gpd(1 / 2, 1, -2) # right endpoint = 1
d <- dst_gpd(0, 1, -3) # right endpoint = 1/3
e <- dst_gpd(0, 1, 0)
f <- dst_norm(0, 1)
g <- dst_empirical(-10:10)


test_that("mixture EVI works", {
  check_mix(a, b, evi = 1)
  check_mix(a, c, evi = -2)
  check_mix(a, c, d, evi = -2)
  check_mix(a, b, c, d, e, f, g, evi = 1)
  check_mix(e, f, a, evi = 0)
  check_mix(f, g, evi = 0)
  check_mix(a, c, d, g, evi = NaN)
  check_mix(b, g, evi = 1)
})
