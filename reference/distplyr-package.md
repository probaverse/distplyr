# distplyr: Manipulate and Combine Probability Distributions

Go beyond standard probability distributions such as the Normal or
Exponential by combining, shifting, maximizing, and otherwise
transforming distributions with simple, verb-based functions. Provides
easy access to a broader space of distributions more representative of
real-world systems such as river flows or insurance claims. Part of the
probaverse framework of packages to support advanced statistical
modeling and simulations with an intuitive workflow.

## Details

The distplyr package provides tools for manipulating probability
distributions using intuitive syntax. Key features include:

**Arithmetic operators**: Use `+`, `-`, `*`, `/`, and `^` with
distributions. See
[`Ops.dst()`](https://distplyr.probaverse.com/reference/Ops.dst.md) for
details.

**Mathematical functions**: Use
[`log()`](https://rdrr.io/r/base/Log.html),
[`exp()`](https://rdrr.io/r/base/Log.html),
[`log10()`](https://rdrr.io/r/base/Log.html), and
[`sqrt()`](https://rdrr.io/r/base/MathFun.html) with distributions. See
[`Math.dst()`](https://distplyr.probaverse.com/reference/Math.dst.md)
for details.

**Transformation functions**: Use
[`shift()`](https://distplyr.probaverse.com/reference/linear_transform.md),
[`multiply()`](https://distplyr.probaverse.com/reference/linear_transform.md),
[`flip()`](https://distplyr.probaverse.com/reference/linear_transform.md),
and
[`invert()`](https://distplyr.probaverse.com/reference/linear_transform.md)
to transform distributions. See `?linear_transform` for details.

**Mixing distributions**: Use
[`mix()`](https://distplyr.probaverse.com/reference/mix.md) to create
mixture distributions.

## See also

Useful links:

- <https://distplyr.probaverse.com/>

## Author

**Maintainer**: Vincenzo Coia <vincenzo.coia@gmail.com> \[copyright
holder\]

Other contributors:

- Amogh Joshi \[contributor\]

- Shuyi Tan \[contributor\]

- Zhipeng Zhu \[contributor\]
