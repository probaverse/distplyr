library(rlang)
library(testthat)




eval_tidy_if_possible <- function(expr, data = NULL, env = caller_env()) {
	eval <- try(eval_tidy(expr, data = data, env = env), silent = TRUE)
	if (class(eval) == "try-error") {
		rlang::get_expr(expr)  # Can't evaluate? No need for an environment.
	} else {
		eval
	}
}

resolve_all_possible <- function(exprs) {
	lapply(exprs, eval_tidy_if_possible)
}

foo_norm <- function(a, b) {
	a <- enquo(a)
	b <- enquo(b)
	resolve_all_possible(quos(mean = !!a, sd = sqrt(!!b)))
}

set_foo <- function(foo, mean) {
	if (!is_expression(foo$mean)) {
		stop("mean is already resolved.",
			 "Try making a new distribution if you want a different mean.")
	}

	if (quo_is_foo$mean
}

eval_dfoo <- function(foo, at) {
	answer <- expr(dnorm(at, !!!foo))
	eval_tidy(answer)
}
get_dfoo <- function(foo) {
	expr(dnorm(at, !!!foo))
}


mu <- 0
ss <- 4
(f1 <- foo_norm(mu, ss))
(f2 <- tibble(m = 0, v = 4) |>
		mutate(d = list(foo_norm(m, v))) |>
		pull(d) |>
		.[[1]])
(f3 <- foo_norm(my_mean, my_var))
(f4 <- foo_norm(my_mean, ss))

expect_equal(eval_dfoo(f1, -3:3), dnorm(-3:3, sd = 2))
expect_equal(eval_dfoo(f2, -3:3), dnorm(-3:3, sd = 2))
expect_error(eval_dfoo(f3, -3:3))
expect_error(eval_dfoo(f4, -3:3))
expect_equal(get_dfoo(f1), expr(dnorm(at, mean = 0, sd = 2)))
expect_equal(get_dfoo(f2), expr(dnorm(at, mean = 0, sd = 2)))
expect_equal(get_dfoo(f3), expr(dnorm(at, mean = my_mean, sd = sqrt(my_var))))
expect_equal(get_dfoo(f4), expr(dnorm(at, mean = my_mean, sd = 2)))

