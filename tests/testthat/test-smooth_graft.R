library(distionary)

# A light (Normal) body grafted to a heavier GPD right tail, with a logistic
# handover that is essentially zero at the GPD's location (so the density is
# smooth there).
make_right <- function(scale = 0.4, shape = 0.3) {
  body <- dst_norm(0, 1)
  q <- eval_quantile(body, at = 0.9)
  tail <- q + dst_gp(scale = 1, shape = shape)
  w <- function(x) stats::plogis(x, location = q + 1, scale = scale)
  smooth_graft_right(body, tail, weight = w)
}

# Integrate a density that may step at `break_at` (the tail's location) and has
# a heavy upper tail, splitting there so adaptive quadrature stays well-behaved.
total_density <- function(g, break_at) {
  lower <- integrate(function(x) eval_density(g, x), -Inf, break_at,
    rel.tol = 1e-6
  )$value
  upper <- integrate(function(x) eval_density(g, x), break_at, Inf,
    rel.tol = 1e-6
  )$value
  lower + upper
}

test_that("smooth_graft_right is a proper distribution", {
  g <- make_right()
  expect_s3_class(g, "smooth_graft")
  expect_equal(pretty_name(g), "Smooth Graft")
  # Survival decreases from 1 to 0; cdf and survival are complementary.
  xs <- seq(-4, 8, by = 0.5)
  expect_equal(eval_cdf(g, xs) + eval_survival(g, xs), rep(1, length(xs)))
  expect_true(!is.unsorted(eval_cdf(g, xs)))
  expect_equal(eval_cdf(g, -50), 0, tolerance = 1e-6)
  expect_equal(eval_cdf(g, 1e6), 1, tolerance = 1e-4)
  # Density integrates to one (split at the tail's location, q).
  q <- eval_quantile(dst_norm(0, 1), at = 0.9)
  expect_equal(total_density(g, q), 1, tolerance = 1e-5)
})

test_that("density equals the numerical derivative of the CDF", {
  g <- make_right()
  # Away from the GPD's own density jump at its location, f = F'.
  xs <- c(-1, 0, 0.5, 1, 3, 4, 5)
  h <- 1e-5
  num_d <- (eval_cdf(g, xs + h) - eval_cdf(g, xs - h)) / (2 * h)
  expect_equal(num_d, eval_density(g, xs), tolerance = 1e-4)
})

test_that("closed-form survival M*C matches exp(-integral of hazard)", {
  g <- make_right()
  hz <- function(x) eval_hazard(g, x) # m / M, independent of the correction
  xs <- c(0, 1, 2, 3, 5)
  s_haz <- vapply(xs, function(xi) {
    exp(-integrate(hz, -Inf, xi, rel.tol = 1e-8)$value)
  }, numeric(1L))
  expect_equal(eval_survival(g, xs), s_haz, tolerance = 1e-6)
})

test_that("quantiles invert the CDF", {
  g <- make_right()
  ps <- c(0.05, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
  expect_equal(eval_cdf(g, eval_quantile(g, ps)), ps, tolerance = 1e-6)
})

test_that("a constant weight reduces to the static mixture", {
  body <- dst_norm(0, 1)
  q <- eval_quantile(body, at = 0.9)
  tail <- q + dst_gp(scale = 1, shape = 0.3)
  wc <- function(x) rep(0.3, length(x))
  wp <- function(x) rep(0, length(x))
  g <- smooth_graft_right(body, tail, weight = wc, weight_deriv = wp)
  xs <- seq(-3, 6, by = 0.5)
  static <- 0.7 * eval_survival(body, xs) + 0.3 * eval_survival(tail, xs)
  expect_equal(eval_survival(g, xs), static, tolerance = 1e-9)
})

test_that("an empirical (Historical Simulation) body is admissible", {
  set.seed(1)
  dat <- rnorm(200)
  hs <- dst_empirical(dat)
  u <- quantile(dat, 0.9, names = FALSE)
  tail <- u + dst_gp(scale = 0.5, shape = 0.2)
  w <- function(x) stats::plogis(x, location = u, scale = 0.3)
  g <- smooth_graft_right(hs, tail, weight = w)
  expect_equal(vtype(g), "mixed")
  # Atom masses follow wbar(a) * C(a) * B({a}), equivalently the CDF jump.
  a <- sort(unique(dat))[c(50, 120, 195)]
  jump <- eval_cdf(g, a) - eval_cdf(g, a - 1e-9)
  expect_equal(eval_pmf(g, a), jump, tolerance = 1e-8)
  # Total mass (atoms + continuous tail) is one.
  patoms <- sum(eval_pmf(g, sort(unique(dat))))
  pcont <- integrate(function(x) eval_density(g, x), u, Inf,
    rel.tol = 1e-6
  )$value
  expect_equal(patoms + pcont, 1, tolerance = 1e-4)
})

test_that("smooth_graft_left is the mirror image and proper", {
  body <- dst_norm(0, 1)
  ltail <- -2 - dst_gp(scale = 1, shape = 0.2)
  w <- function(x) stats::plogis(x, location = -3.5, scale = 0.4)
  g <- smooth_graft_left(body, ltail, weight = w)
  expect_s3_class(g, "smooth_graft")
  xs <- seq(-8, 4, by = 0.5)
  expect_equal(eval_cdf(g, xs) + eval_survival(g, xs), rep(1, length(xs)))
  expect_equal(eval_cdf(g, -1e6), 0, tolerance = 1e-6)
  expect_equal(eval_cdf(g, 1e6), 1, tolerance = 1e-6)
  # Density integrates to one (split at the left tail's location, -2).
  expect_equal(total_density(g, -2), 1, tolerance = 1e-5)
})

test_that("a finite threshold reproduces the body below it and stays proper", {
  body <- dst_norm(0, 1)
  q <- eval_quantile(body, at = 0.9)
  tail <- q + dst_gp(scale = 1, shape = 0.3)
  w <- function(x) stats::plogis(x, location = q + 0.5, scale = 0.4)
  g <- smooth_graft_right(body, tail, weight = w, threshold = q)
  # Exactly the body below the threshold.
  below <- c(-2, -1, 0, 1, q - 0.05)
  expect_equal(eval_cdf(g, below), eval_cdf(body, below))
  expect_equal(eval_density(g, below), eval_density(body, below))
  # Survival is continuous across the threshold.
  expect_equal(
    eval_survival(g, q - 1e-6), eval_survival(g, q + 1e-6),
    tolerance = 1e-5
  )
  # Proper distribution.
  expect_equal(eval_cdf(g, xs <- seq(-4, 8, 0.5)) + eval_survival(g, xs),
    rep(1, length(xs))
  )
  expect_equal(eval_cdf(g, 1e6) - eval_cdf(g, -1e6), 1, tolerance = 1e-4)
  # Hazard below the threshold is the body's; overall it is f / S.
  expect_equal(eval_hazard(g, below), eval_hazard(body, below))
  expect_equal(
    eval_hazard(g, c(q, 2, 4)),
    eval_density(g, c(q, 2, 4)) / eval_survival(g, c(q, 2, 4))
  )
})

test_that("a finite threshold keeps an empirical body exactly below it", {
  set.seed(1)
  dat <- rnorm(200)
  hs <- dst_empirical(dat)
  u <- quantile(dat, 0.9, names = FALSE)
  tail <- u + dst_gp(scale = 0.5, shape = 0.2)
  w <- function(x) stats::plogis(x, location = u, scale = 0.3)
  g <- smooth_graft_right(hs, tail, weight = w, threshold = u)
  expect_equal(vtype(g), "mixed")
  below <- sort(unique(dat))[sort(unique(dat)) < u]
  expect_equal(eval_pmf(g, below), eval_pmf(hs, below))
  expect_equal(eval_cdf(g, below), eval_cdf(hs, below))
  # Atoms above the threshold are reweighted; pmf equals the CDF jump. The jump
  # is a finite difference, so compare on an absolute scale (its noise swamps a
  # relative tolerance for the tiniest atoms).
  above <- sort(unique(dat))[sort(unique(dat)) > u]
  jump <- eval_cdf(g, above) - eval_cdf(g, above - 1e-9)
  expect_lt(max(abs(eval_pmf(g, above) - jump)), 1e-7)
  expect_equal(eval_cdf(g, 1e4) - eval_cdf(g, -1e4), 1, tolerance = 1e-6)
})

test_that("a finite threshold reproduces the body above it for a left graft", {
  body <- dst_norm(0, 1)
  ltail <- -2 - dst_gp(scale = 1, shape = 0.2)
  w <- function(x) stats::plogis(x, location = -2, scale = 0.4)
  g <- smooth_graft_left(body, ltail, weight = w, threshold = -2)
  above <- c(-1, 0, 1, 2)
  expect_equal(eval_cdf(g, above), eval_cdf(body, above))
  expect_equal(eval_cdf(g, 1e4) - eval_cdf(g, -1e4), 1, tolerance = 1e-6)
  expect_equal(eval_cdf(g, -1e5), 0, tolerance = 1e-6)
})

test_that("NA inputs propagate as NA in both modes", {
  body <- dst_norm(0, 1)
  q <- eval_quantile(body, at = 0.9)
  tail <- q + dst_gp(scale = 1, shape = 0.3)
  w <- function(x) stats::plogis(x, location = q + 1, scale = 0.4)
  for (g in list(
    smooth_graft_right(body, tail, weight = w),
    smooth_graft_right(body, tail, weight = w, threshold = q)
  )) {
    at <- c(0, NA, 2)
    expect_identical(is.na(eval_cdf(g, at)), c(FALSE, TRUE, FALSE))
    expect_identical(is.na(eval_survival(g, at)), c(FALSE, TRUE, FALSE))
    expect_identical(is.na(eval_density(g, at)), c(FALSE, TRUE, FALSE))
  }
})

test_that("an atom exactly at the threshold stays with the body", {
  set.seed(3)
  dat <- round(rnorm(100), 1) # rounding creates repeated atoms
  hs <- dst_empirical(dat)
  uu <- sort(unique(dat))
  u <- uu[floor(0.85 * length(uu))] # an actual atom in the upper range
  expect_gt(eval_pmf(hs, u), 0)
  tail <- u + dst_gp(scale = 0.5, shape = 0.2)
  w <- function(x) stats::plogis(x, location = u, scale = 0.3)
  g <- smooth_graft_right(hs, tail, weight = w, threshold = u)
  # The threshold atom keeps its full body mass; survival at u is P(X > u).
  expect_equal(eval_pmf(g, u), eval_pmf(hs, u))
  expect_equal(eval_survival(g, u), eval_survival(hs, u))
  expect_equal(eval_cdf(g, u - 1e-9), eval_cdf(hs, u - 1e-9))
  expect_equal(eval_cdf(g, 1e4) - eval_cdf(g, -1e4), 1, tolerance = 1e-6)
})

test_that("whole-range left graft handles an empirical body", {
  set.seed(4)
  dat <- round(rnorm(80), 1)
  hs <- dst_empirical(dat)
  ltail <- min(dat) - dst_gp(scale = 0.5, shape = 0.2)
  q10 <- quantile(dat, 0.1, names = FALSE)
  w <- function(x) stats::plogis(-(x - q10), location = 0, scale = 0.3)
  g <- smooth_graft_left(hs, ltail, weight = w)
  a <- sort(unique(dat))
  jump <- eval_cdf(g, a) - eval_cdf(g, a - 1e-9)
  expect_lt(max(abs(eval_pmf(g, a) - jump)), 1e-7)
  expect_equal(eval_cdf(g, 1e4) - eval_cdf(g, -1e4), 1, tolerance = 1e-6)
  expect_equal(eval_cdf(g, -1e5), 0, tolerance = 1e-6)
})

test_that("a body with infinitely many atoms (Poisson) is admissible", {
  # The body's support accumulates at +Inf, so its atoms cannot be listed as a
  # vector; the correction integral must enumerate them per query interval.
  body <- dst_pois(5)
  tail <- 12 + dst_gp(scale = 3, shape = 0.25)
  w <- function(x) stats::plogis(x, location = 12, scale = 1)
  g <- smooth_graft_right(body, tail, weight = w)
  expect_s3_class(g, "smooth_graft")
  expect_equal(vtype(g), "mixed")
  xs <- c(0, 2, 5, 8, 12, 20, 50)
  expect_equal(eval_cdf(g, xs) + eval_survival(g, xs), rep(1, length(xs)))
  expect_true(!is.unsorted(eval_cdf(g, xs)))
  expect_equal(eval_cdf(g, 1e5), 1, tolerance = 1e-6)
  # Atom masses equal the CDF jumps. The offset is 1e-6 rather than 1e-9: the
  # body's atoms are integers, and a smaller offset is snapped back to the atom.
  a <- 0:15
  jump <- eval_cdf(g, a) - eval_cdf(g, a - 1e-6)
  expect_equal(eval_pmf(g, a), jump, tolerance = 1e-6)
  # Total mass (atoms + the continuous part above the tail's location) is one.
  patoms <- sum(eval_pmf(g, 0:300))
  pcont <- integrate(function(x) eval_density(g, x), 12, Inf,
    rel.tol = 1e-6
  )$value
  expect_equal(patoms + pcont, 1, tolerance = 1e-6)
})

test_that("a finite threshold keeps a Poisson body exactly below it", {
  body <- dst_pois(5)
  u <- 10
  tail <- u + dst_gp(scale = 3, shape = 0.25)
  w <- function(x) stats::plogis(x, location = u, scale = 1)
  g <- smooth_graft_right(body, tail, weight = w, threshold = u)
  expect_equal(vtype(g), "mixed")
  below <- 0:9
  expect_equal(eval_pmf(g, below), eval_pmf(body, below))
  expect_equal(eval_cdf(g, below), eval_cdf(body, below))
  patoms <- sum(eval_pmf(g, 0:300))
  pcont <- integrate(function(x) eval_density(g, x), u, Inf,
    rel.tol = 1e-6
  )$value
  expect_equal(patoms + pcont, 1, tolerance = 1e-6)
})

test_that("supplying the analytic weight derivative matches the numeric one", {
  body <- dst_norm(0, 1)
  q <- eval_quantile(body, at = 0.9)
  tail <- q + dst_gp(scale = 1, shape = 0.3)
  w <- function(x) stats::plogis(x, location = q + 1, scale = 0.4)
  wp <- function(x) stats::dlogis(x, location = q + 1, scale = 0.4)
  g_num <- smooth_graft_right(body, tail, weight = w)
  g_ana <- smooth_graft_right(body, tail, weight = w, weight_deriv = wp)
  xs <- seq(-2, 6, by = 0.5)
  expect_equal(eval_cdf(g_ana, xs), eval_cdf(g_num, xs), tolerance = 1e-6)
})

test_that("dots must be empty", {
  body <- dst_norm(0, 1)
  tail <- 1 + dst_gp(scale = 1, shape = 0.3)
  w <- function(x) stats::plogis(x, 2, 0.5)
  expect_error(smooth_graft_right(body, tail, w, include = TRUE))
})
