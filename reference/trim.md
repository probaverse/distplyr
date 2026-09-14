# Trim (condition) a distribution

Discard the probability lying to one side of a value, and scale up what
remains so that it sums to 1 again. `trim_left()` discards the
probability below `of`, giving the distribution of the variable
conditioned on landing at or above it; `trim_right()` discards the
probability above `of`, conditioning on landing at or below.

## Usage

``` r
trim_left(distribution, of, ..., knot = c("keep", "discard", "split"))

trim_right(distribution, of, ..., knot = c("keep", "discard", "split"))
```

## Arguments

- distribution:

  Distribution to trim.

- of:

  Value on the real line defining where to trim (single numeric).

- ...:

  Currently unused; must be empty.

- knot:

  What to do with the probability sitting exactly on `of`: `"keep"` it
  (the default), `"discard"` it with the trimmed side, or `"split"` it
  evenly between the two sides. Only has an effect where `of` carries
  probability. See Details.

## Value

The conditional distribution, renormalised to total probability 1; or
the Null distribution, if the trim leaves nothing behind.

## What `knot` does

`of` is the knot: the point the trim cuts at. `knot` says what becomes
of the probability sitting exactly on it, and matters only when the knot
carries mass of its own, as an atom does. Where there is no mass exactly
at `of` — anywhere in a continuous distribution — all three actions give
the same answer.

- `"keep"` (default) retains it, so `trim_left(d, of)` keeps outcomes
  greater than *or equal to* `of`. The knot is not on the side being
  trimmed away: `of` is not to the left of itself.

- `"discard"` throws the knot away with that side, so `trim_left(d, of)`
  keeps outcomes strictly greater than `of`. Under this setting the
  probability kept is exactly
  `distionary::prob_right(d, of, inclusive = FALSE)`, and a left and a
  right trim at the same knot share nothing.

- `"split"` retains half of it. This is the mid-p convention used in
  discrete inference, where a boundary atom is shared evenly between the
  two sides rather than assigned wholly to one.

Whatever is retained is renormalised along with the rest, so the result
is always a distribution in its own right.

## Values of `of` with nothing beside them

If `of` falls in a gap in the support, the trim takes effect where the
support resumes. Trimming a distribution living on `[1, 2]` and `[4, 5]`
to the left of 3 gives one living on `[4, 5]`.

If the trim leaves no probability at all — trimming a Uniform(0, 1) to
the left of 2, say — the result is the Null distribution,
[`distionary::dst_null()`](https://distionary.probaverse.com/reference/dst_null.html).

## See also

[`graft_left()`](https://distplyr.probaverse.com/reference/graft.md) and
[`graft_right()`](https://distplyr.probaverse.com/reference/graft.md),
which replace a tail rather than discarding it.

## Examples

``` r
d <- distionary::dst_norm(0, 1)
d <- trim_left(d, -2)
d <- trim_right(d, 2)
distionary::enframe_cdf(d, at = -3:3)
#> # A tibble: 7 × 2
#>    .arg   cdf
#>   <int> <dbl>
#> 1    -3 0    
#> 2    -2 0    
#> 3    -1 0.142
#> 4     0 0.5  
#> 5     1 0.858
#> 6     2 1    
#> 7     3 1    

# A Poisson has an atom at 5, so the knot is visible there. By default
# the trim keeps it.
d <- distionary::dst_pois(3)
distionary::eval_pmf(trim_left(d, 5), at = 5)
#> Loading required namespace: testthat
#> [1] 0.5457431
distionary::eval_pmf(trim_left(d, 5, knot = "discard"), at = 5)
#> [1] 0
distionary::eval_pmf(trim_left(d, 5, knot = "split"), at = 5)
#> [1] 0.3752728
```
