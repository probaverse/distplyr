# Graft a tail onto a distribution

Replace one end of a distribution with a different model of that end.
`graft_right()` keeps `body` below `of` — the knot — and hands
everything above it to the tail; `graft_left()` does the same at the
lower end. The tail's share of the total is the body's own probability
of reaching past the knot.

## Usage

``` r
graft_right(
  body,
  of,
  ...,
  tail_excess,
  tail_absolute,
  knot = c("body", "tail", "split")
)

graft_left(
  body,
  of,
  ...,
  tail_excess,
  tail_absolute,
  knot = c("body", "tail", "split")
)
```

## Arguments

- body:

  Distribution supplying the part of the range that is kept.

- of:

  Value on the real line where the tail is attached: the knot.

- ...:

  Currently unused; must be empty.

- tail_excess:

  Distribution of the tail measured from the knot, moved from zero to
  `of`. Name either this or `tail_absolute`, not both.

- tail_absolute:

  Distribution of the tail on the body's own scale, left where it is and
  conditioned beyond the knot. Name either this or `tail_excess`, not
  both.

- knot:

  Which side probability sitting exactly on the knot belongs to: the
  `"body"` (the default), the `"tail"`, or `"split"` between them.

## Value

A graft: the body on one side of the knot and the tail on the other,
which is a special type of mixture distribution.

## Two ways to hand over the tail

Name exactly one of `tail_excess` and `tail_absolute`. Neither has a
default, because nothing in a distribution says which scale it is on.

- `tail_excess` is measured from the knot: the distribution of `X - of`,
  whose zero is the knot. It is **moved from zero to the knot**, by
  adding `of` to it, so a generalised Pareto living on `[0, Inf)`
  becomes a tail living on `[of, Inf)`. All of its probability must lie
  on one side of zero: at or above for `graft_right()`, at or below for
  `graft_left()`.

- `tail_absolute` is on the body's scale already and stays where it is,
  conditioned on falling beyond the knot. Anything placed by hand goes
  here — `multiply(ratio, of)`, for a model of `X / of`, say.

A tail that is already in place but happens to sit above zero cannot be
told apart from a model of excesses, so `tail_excess` accepts it. If it
starts exactly at `of`, the likeliest case, you get a warning.

## Where the knot goes

`knot` names the side that probability sitting exactly on the knot
belongs to: `"body"` (the default), `"tail"`, or `"split"` for half
each, the mid-p convention. Naming one side names the other, so the knot
is counted once; what a side does not take passes into the other's
share. None of this has any effect unless there is mass exactly at `of`,
as there never is in a continuous distribution.

The default leaves the body alone up to and including the knot, and
gives the tail `prob_right(body, of, inclusive = FALSE)`, the
probability of exceeding it. That is the convention peaks-over-threshold
is written in, where the excess `X - of` is conditioned on `X > of`
strictly and an excess of exactly zero does not arise.

## See also

[`trim_left()`](https://distplyr.probaverse.com/reference/trim.md) and
[`trim_right()`](https://distplyr.probaverse.com/reference/trim.md),
which discard an end rather than replacing it.

## Examples

``` r
body <- distionary::dst_norm(0, 1)
u <- distionary::eval_quantile(body, at = 0.9)

# Excesses over `u`, living on [0, Inf): moved to start at `u`.
graft_right(body, of = u, tail_excess = distionary::dst_gp(1, 0.3))
#> Graft distribution (continuous)
#> --Components--
#>  distribution                        weight
#>  Right-Trimmed(Normal(0, 1))         0.9   
#>  Shifted(Generalised Pareto(1, 0.3)) 0.1   

# The same graft, placed by hand instead.
moved <- shift(distionary::dst_gp(1, 0.3), u)
graft_right(body, of = u, tail_absolute = moved)
#> Graft distribution (continuous)
#> --Components--
#>  distribution                        weight
#>  Right-Trimmed(Normal(0, 1))         0.9   
#>  Shifted(Generalised Pareto(1, 0.3)) 0.1   

# A model on the body's scale, conditioned above `u`.
graft_right(body, of = u, tail_absolute = distionary::dst_norm(1, 3))
#> Graft distribution (continuous)
#> --Components--
#>  distribution                weight
#>  Right-Trimmed(Normal(0, 1)) 0.9   
#>  Left-Trimmed(Normal(1, 3))  0.1   
```
