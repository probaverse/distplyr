# Extremum of Several Distributions

For a collection of distributions, obtain the distributions of the
maximum (`maximize()`) and minimum (`minimize()`) from independent draws
of each component distribution.

Aliases `maximise()` and `minimise()` are also provided.

## Usage

``` r
maximize(
  ...,
  draws = 1,
  na_action_dst = c("null", "drop", "fail"),
  na_action_draws = c("null", "drop", "fail")
)

minimize(
  ...,
  draws = 1,
  na_action_dst = c("null", "drop", "fail"),
  na_action_draws = c("null", "drop", "fail")
)
```

## Arguments

- ...:

  Distribution objects, or list of distributions.

- draws:

  Number of draws from each distribution considered in the maximum
  (possibly not integer, but never negative). Either a single numeric
  applying to all distributions in `...`, or a vector matching the
  number of distributions in `...`.

- na_action_dst, na_action_draws:

  What should be done with Null distributions in `...` and `NA` in
  `draws`? Character vector of length 1: one of "fail", "null"
  (default), or "drop". See details.

## Value

A distribution of class `"max"`.

## Details

To give an example of what distribution is returned, if X1 and X2 are
two random variables with distributions D1 and D2 respectively, then
`maximize(D1, D2, draws = c(2, 3))` returns the distribution of
`max(X1, X1, X2, X2, X2)`.

Distributions in `...` and the `draws` vector are recycled to have the
same length, but only if one of them has length 1 (via
[`vctrs::vec_recycle_common()`](https://vctrs.r-lib.org/reference/vec_recycle.html)).

`na_action_dst` and `na_action_draws` specify the NA action for
distributions and `draws`. "NA" here means either `NA` in the `draws`
vector, or a Null distribution
([`distionary::dst_null()`](https://distionary.probaverse.com/reference/dst_null.html))
in the distributions. Options are, in order of precedence:

- `"fail"`: Throw an error in the presence of NAs.

- `"null"`: Return a Null distribution in the presence of NAs.

- `"drop"`: Remove distribution-weight pairs having an NA value

Simplifications made in these functions include the following:

- If any distributions are entirely to the left (right) of others, then
  they are removed from consideration in `maximize()` (`minimize()`).

- If all Finite distributions are input, the result is also a Finite
  distribution.

- If the same distribution is input multiple times, their corresponding
  draws are summed.

## Examples

``` r
# One is always more extreme than the other in this case.
d1 <- dst_unif(-1, 2)
#> Error in dst_unif(-1, 2): could not find function "dst_unif"
d2 <- dst_unif(5, 6)
#> Error in dst_unif(5, 6): could not find function "dst_unif"
maximize(d1, d2) # d2
#> Error: object 'd1' not found
minimize(d1, d2) # d1
#> Error: object 'd1' not found

# Visualizing the maximum and minimum
d3 <- dst_norm(4, 1)
#> Error in dst_norm(4, 1): could not find function "dst_norm"
d4 <- dst_exp(0.3)
#> Error in dst_exp(0.3): could not find function "dst_exp"

dmax <- maximize(d3, d4, draws = 1:2)
#> Error: object 'd3' not found
dmin <- minimize(d3, d4, draws = 1:2)
#> Error: object 'd3' not found

# Maximum
plot(d3, col = "blue", lty = 2, from = 0, to = 14)
#> Error: object 'd3' not found
plot(d4, col = "red", lty = 2, add = TRUE)
#> Error: object 'd4' not found
plot(dmax, add = TRUE, n = 1000)
#> Error: object 'dmax' not found
legend(
 "topright",
 legend = c("Maximum", "N(4,1)", "Exp(0.3)"),
 col = c("black", "blue", "red"),
 lty = c(1, 2, 2)
)
#> Error in (function (s, units = "user", cex = NULL, font = NULL, vfont = NULL,     ...) {    if (!is.null(vfont))         vfont <- c(typeface = pmatch(vfont[1L], Hershey$typeface),             fontindex = pmatch(vfont[2L], Hershey$fontindex))    .External.graphics(C_strWidth, as.graphicsAnnot(s), pmatch(units,         c("user", "figure", "inches")), cex, font, vfont, ...)})(dots[[1L]][[1L]], cex = dots[[2L]][[1L]], font = dots[[3L]][[1L]],     units = "user"): plot.new has not been called yet

# Minimum
plot(d3, col = "blue", lty = 2, from = 0, to = 10)
#> Error: object 'd3' not found
plot(d4, col = "red", lty = 2, add = TRUE)
#> Error: object 'd4' not found
plot(dmin, add = TRUE, n = 1000)
#> Error: object 'dmin' not found
legend(
  "topright",
  legend = c("Minimum", "N(4,1)", "Exp(0.3)"),
  col = c("black", "blue", "red"),
  lty = c(1, 2, 2)
)
#> Error in (function (s, units = "user", cex = NULL, font = NULL, vfont = NULL,     ...) {    if (!is.null(vfont))         vfont <- c(typeface = pmatch(vfont[1L], Hershey$typeface),             fontindex = pmatch(vfont[2L], Hershey$fontindex))    .External.graphics(C_strWidth, as.graphicsAnnot(s), pmatch(units,         c("user", "figure", "inches")), cex, font, vfont, ...)})(dots[[1L]][[1L]], cex = dots[[2L]][[1L]], font = dots[[3L]][[1L]],     units = "user"): plot.new has not been called yet
```
