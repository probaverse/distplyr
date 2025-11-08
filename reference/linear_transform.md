# Linear and Reciprocal Transformations

Transform distributions using location shifts, scaling, negation, and
reciprocals. If `X` is a random variable following a distribution, these
functions return the distribution of the transformed variable.

## Usage

``` r
flip(distribution)

invert(distribution)

multiply(distribution, constant)

shift(distribution, constant)
```

## Arguments

- distribution:

  A probability distribution.

- constant:

  A numeric value for shifting or scaling.

## Value

A transformed distribution.

## Functions vs Operators

These transformations can be applied using named functions or arithmetic
operators:

- `shift(d, a)` or `d + a` - Returns distribution of `X + a`

- `multiply(d, a)` or `d * a` - Returns distribution of `X * a`

- `flip(d)` or `-d` - Returns distribution of `-X`

- `invert(d)` or `1 / d` - Returns distribution of `1 / X`

For complete documentation of operator usage, see
[`Ops.dst()`](https://distplyr.probaverse.com/reference/Ops.dst.md).

## Special Cases

**Negation in multiplication**: When `multiply()` receives a negative
constant, it internally calls `flip()` on the result of multiplying by
the absolute value.

**Inversion constraint**: `invert()` requires that the distribution has
no mass at zero (i.e., `P(X = 0) = 0`). An error is returned if this
condition is violated.

## Simplifications

These functions apply automatic simplifications when possible. For
example:

- Shifting a Normal distribution returns another Normal distribution

- Multiplying a Uniform distribution returns another Uniform
  distribution

- Flipping a symmetric distribution may preserve its form

More simplifications will be added in future versions of distplyr.

## See also

[`Ops.dst()`](https://distplyr.probaverse.com/reference/Ops.dst.md) for
arithmetic operators including `+`, `-`, `*`, `/`.

## Examples

``` r
d_pois <- distionary::dst_pois(1.1)
d_norm <- distionary::dst_norm(4, 1)
d_unif <- distionary::dst_unif(0, 1)

# Shifting
shift(d_pois, 1)
#> Shifted distribution (discrete) 
#> --Parameters--
#> $distribution
#> Poisson distribution (discrete) 
#> --Parameters--
#> lambda 
#>    1.1 
#> 
#> $shift
#> [1] 1
#> 
d_pois + 1           # Equivalent using operator
#> Shifted distribution (discrete) 
#> --Parameters--
#> $distribution
#> Poisson distribution (discrete) 
#> --Parameters--
#> lambda 
#>    1.1 
#> 
#> $shift
#> [1] 1
#> 

# Scaling
multiply(d_unif, 2)
#> Uniform distribution (continuous) 
#> --Parameters--
#> min max 
#>   0   2 
d_unif * 2           # Equivalent using operator
#> Uniform distribution (continuous) 
#> --Parameters--
#> min max 
#>   0   2 

# Negation
flip(d_norm)
#> Normal distribution (continuous) 
#> --Parameters--
#> mean   sd 
#>   -4    1 
-d_norm              # Equivalent using operator
#> Normal distribution (continuous) 
#> --Parameters--
#> mean   sd 
#>   -4    1 

# Inversion
d_positive <- distionary::dst_unif(1, 2)
invert(d_positive)
#> Inverse distribution (continuous) 
#> --Parameters--
#> $distribution
#> Uniform distribution (continuous) 
#> --Parameters--
#> min max 
#>   1   2 
#> 
1 / d_positive       # Equivalent using operator
#> Inverse distribution (continuous) 
#> --Parameters--
#> $distribution
#> Uniform distribution (continuous) 
#> --Parameters--
#> min max 
#>   1   2 
#> 

# Combine multiple operations
4 - 2 * d_pois
#> Shifted distribution (discrete) 
#> --Parameters--
#> $distribution
#> Negated distribution (discrete) 
#> --Parameters--
#> $distribution
#> Scaled distribution (discrete) 
#> --Parameters--
#> $distribution
#> Poisson distribution (discrete) 
#> --Parameters--
#> lambda 
#>    1.1 
#> 
#> $constant
#> [1] 2
#> 
#> 
#> 
#> $shift
#> [1] 4
#> 
multiply(flip(multiply(d_pois, 2)), -1) + 4  # Equivalent
#> Shifted distribution (discrete) 
#> --Parameters--
#> $distribution
#> Scaled distribution (discrete) 
#> --Parameters--
#> $distribution
#> Poisson distribution (discrete) 
#> --Parameters--
#> lambda 
#>    1.1 
#> 
#> $constant
#> [1] 2
#> 
#> 
#> $shift
#> [1] 4
#> 
```
