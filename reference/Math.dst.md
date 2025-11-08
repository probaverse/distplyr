# Mathematical Transformations for Distributions

Apply mathematical functions like
[`log()`](https://rdrr.io/r/base/Log.html) and
[`exp()`](https://rdrr.io/r/base/Log.html) to probability distributions.
If `X` is a random variable following a distribution, these functions
return the distribution of the transformed variable.

## Usage

``` r
# S3 method for class 'dst'
Math(x, ...)
```

## Arguments

- x:

  A probability distribution object.

- ...:

  Additional arguments passed to specific methods. For
  [`log()`](https://rdrr.io/r/base/Log.html), you can specify `base`
  (defaults to `exp(1)` for natural log).

## Value

A transformed distribution object.

## Details

These S3 methods extend base R functions to work with distributions.

## Supported Functions

- `log(x, base = exp(1))`:

  Returns the distribution of `log(X)`. The base can be specified
  (defaults to natural log). An error is returned if the distribution
  has non-positive values as possible outcomes.

- `log10(x)`:

  Returns the distribution of `log10(X)`. Equivalent to
  `log(x, base = 10)`.

- `exp(x)`:

  Returns the distribution of `exp(X)`.

- `sqrt(x)`:

  Returns the distribution of `sqrt(X)`. Equivalent to `x^0.5`. Requires
  all values to be non-negative.

## Power Operator

The power operator `^` also works with distributions (see
[`Ops.dst()`](https://distplyr.probaverse.com/reference/Ops.dst.md)).
When raising a distribution to a numeric power (e.g., `dst^2`), it uses
the relationship `X^a = exp(a * log(X))`, combining both exponential and
logarithmic transformations.

## See also

- [`Ops.dst()`](https://distplyr.probaverse.com/reference/Ops.dst.md)
  for the `^` operator and other arithmetic operations

- [`shift()`](https://distplyr.probaverse.com/reference/linear_transform.md),
  [`multiply()`](https://distplyr.probaverse.com/reference/linear_transform.md),
  [`flip()`](https://distplyr.probaverse.com/reference/linear_transform.md),
  [`invert()`](https://distplyr.probaverse.com/reference/linear_transform.md)
  for linear transformations

## Examples

``` r
# Logarithmic transformations
d <- distionary::dst_unif(1, 10)
log(d)              # Natural log
#> Logarithmic distribution (continuous) 
#> --Parameters--
#> $distribution
#> Uniform distribution (continuous) 
#> --Parameters--
#> min max 
#>   1  10 
#> 
log(d, base = 10)   # Log base 10
#> Scaled distribution (continuous) 
#> --Parameters--
#> $distribution
#> Logarithmic distribution (continuous) 
#> --Parameters--
#> $distribution
#> Uniform distribution (continuous) 
#> --Parameters--
#> min max 
#>   1  10 
#> 
#> 
#> $constant
#> [1] 0.4342945
#> 
log10(d)            # Also log base 10
#> Scaled distribution (continuous) 
#> --Parameters--
#> $distribution
#> Logarithmic distribution (continuous) 
#> --Parameters--
#> $distribution
#> Uniform distribution (continuous) 
#> --Parameters--
#> min max 
#>   1  10 
#> 
#> 
#> $constant
#> [1] 0.4342945
#> 
sqrt(d)             # Square root of uniform
#> Exponentiated distribution (continuous) 
#> --Parameters--
#> $distribution
#> Scaled distribution (continuous) 
#> --Parameters--
#> $distribution
#> Logarithmic distribution (continuous) 
#> --Parameters--
#> $distribution
#> Uniform distribution (continuous) 
#> --Parameters--
#> min max 
#>   1  10 
#> 
#> 
#> $constant
#> [1] 0.5
#> 
#> 

# Exponential transformation
d2 <- distionary::dst_norm(0, 1)
d3 <- distionary::dst_beta(5, 4)
exp(d2)             # Log-normal distribution
#> Log Normal distribution (continuous) 
#> --Parameters--
#> meanlog   sdlog 
#>       0       1 
exp(d3)             # No simplification
#> Exponentiated distribution (continuous) 
#> --Parameters--
#> $distribution
#> Beta distribution (continuous) 
#> --Parameters--
#> shape1 shape2 
#>      5      4 
#> 

# These can be combined
log(exp(d2))        # Returns back to normal distribution
#> Normal distribution (continuous) 
#> --Parameters--
#> mean   sd 
#>    0    1 
log(exp(d3))        # Still returns d3.
#> Beta distribution (continuous) 
#> --Parameters--
#> shape1 shape2 
#>      5      4 
5^(log(d3, base = 5)) # Still returns d3.
#> Beta distribution (continuous) 
#> --Parameters--
#> shape1 shape2 
#>      5      4 
```
