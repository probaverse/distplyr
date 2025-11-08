# Arithmetic Operations for Distributions

Apply arithmetic operators to probability distributions. These
operations transform distributions in intuitive ways, treating them
similarly to numeric values.

## Usage

``` r
# S3 method for class 'dst'
Ops(e1, e2)
```

## Arguments

- e1:

  A probability distribution or numeric value.

- e2:

  A probability distribution or numeric value.

## Value

A transformed distribution object.

## Details

These S3 methods extend arithmetic operators to work with distributions.

## Supported Operators

- `d + a` or `a + d`:

  Shifts the distribution by adding constant `a`. Equivalent to
  [`shift()`](https://distplyr.probaverse.com/reference/linear_transform.md).

- `d - a`:

  Shifts the distribution by subtracting constant `a`. Equivalent to
  `shift(d, -a)`.

- `-d`:

  Flips the distribution (negation). Equivalent to
  [`flip()`](https://distplyr.probaverse.com/reference/linear_transform.md).

- `d * a` or `a * d`:

  Scales the distribution by multiplying by constant `a`. Equivalent to
  [`multiply()`](https://distplyr.probaverse.com/reference/linear_transform.md).

- `d / a`:

  Scales the distribution by dividing by constant `a`. Equivalent to
  `multiply(d, 1/a)`.

- `a / d`:

  Returns the distribution of `a / X` (reciprocal scaling). For `a = 1`,
  equivalent to
  [`invert()`](https://distplyr.probaverse.com/reference/linear_transform.md).

- `d ^ a`:

  Raises the distribution to power `a`. For positive distributions only,
  computed as `exp(a * log(d))`.

- `a ^ d`:

  Returns the distribution of `a^X`. Requires positive base `a`.

## Power Operator Details

The power operator `^` deserves special attention:

- When the **base is a distribution** (e.g., `dst_beta(1, 1)^2`), it
  computes the distribution of `X^a` using the transformation
  `exp(a * log(X))`. This requires all values in the distribution to be
  positive.

- When the **exponent is a distribution** (e.g., `2^dst_norm(0, 1)`), it
  computes the distribution of `a^X` using `exp(X * log(a))`. The base
  `a` must be positive.

These implementations internally use both
[`Math.dst()`](https://distplyr.probaverse.com/reference/Math.dst.md)
methods [`log()`](https://rdrr.io/r/base/Log.html) and
[`exp()`](https://rdrr.io/r/base/Log.html).

## See also

- [`shift()`](https://distplyr.probaverse.com/reference/linear_transform.md),
  [`multiply()`](https://distplyr.probaverse.com/reference/linear_transform.md),
  [`flip()`](https://distplyr.probaverse.com/reference/linear_transform.md),
  [`invert()`](https://distplyr.probaverse.com/reference/linear_transform.md)
  for the underlying transformation functions

- [`Math.dst()`](https://distplyr.probaverse.com/reference/Math.dst.md)
  for [`log()`](https://rdrr.io/r/base/Log.html),
  [`exp()`](https://rdrr.io/r/base/Log.html), and
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html) functions

## Examples

``` r
d <- distionary::dst_beta(3, 2)

# Shifting and scaling
d + 10          # Shift right by 10
#> Shifted distribution (continuous) 
#> --Parameters--
#> $distribution
#> Beta distribution (continuous) 
#> --Parameters--
#> shape1 shape2 
#>      3      2 
#> 
#> $shift
#> [1] 10
#> 
d * 2           # Scale by 2
#> Scaled distribution (continuous) 
#> --Parameters--
#> $distribution
#> Beta distribution (continuous) 
#> --Parameters--
#> shape1 shape2 
#>      3      2 
#> 
#> $constant
#> [1] 2
#> 
3 * d - 5       # Scale then shift
#> Shifted distribution (continuous) 
#> --Parameters--
#> $distribution
#> Scaled distribution (continuous) 
#> --Parameters--
#> $distribution
#> Beta distribution (continuous) 
#> --Parameters--
#> shape1 shape2 
#>      3      2 
#> 
#> $constant
#> [1] 3
#> 
#> 
#> $shift
#> [1] -5
#> 

# Power operations
exp(d)          # e^X: exponential of Beta
#> Exponentiated distribution (continuous) 
#> --Parameters--
#> $distribution
#> Beta distribution (continuous) 
#> --Parameters--
#> shape1 shape2 
#>      3      2 
#> 
2^d             # 2^X: base 2 raised to Beta
#> Exponentiated distribution (continuous) 
#> --Parameters--
#> $distribution
#> Scaled distribution (continuous) 
#> --Parameters--
#> $distribution
#> Beta distribution (continuous) 
#> --Parameters--
#> shape1 shape2 
#>      3      2 
#> 
#> $constant
#> [1] 0.6931472
#> 
#> 

# With positive distributions
d_pos <- distionary::dst_unif(1, 2)
d_pos^2         # X^2: uniform squared
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
#>   1   2 
#> 
#> 
#> $constant
#> [1] 2
#> 
#> 
d_pos^0.5       # sqrt(X): square root
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
#>   1   2 
#> 
#> 
#> $constant
#> [1] 0.5
#> 
#> 
sqrt(d_pos)     # Equivalent to d_pos^0.5
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
#>   1   2 
#> 
#> 
#> $constant
#> [1] 0.5
#> 
#> 
```
