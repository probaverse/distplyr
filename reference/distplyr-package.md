# distplyr: Manipulation of Probability Distributions

Manipulating probability distributions, using verbs such as \`mix\`,
\`shift\`, \`maximize\`, \`invert\`, and others. This package builds on
the distionary package and belongs in the wider probaverse suite of R
packages to support statistical modeling and simulations.

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
