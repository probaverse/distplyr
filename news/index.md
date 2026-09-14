# Changelog

## distplyr 0.3.0

Every verb is now built on distionary’s support objects, so a
transformed distribution says where its probability lives:
[`support()`](https://distionary.probaverse.com/reference/support.html),
its atoms, and discrete or mixed moments all work on the result. Verbs
no longer state a `range` or `.vtype`; both are read from the support.
[`invert()`](https://distplyr.probaverse.com/reference/linear_transform.md),
for one, derives the support of `1 / X` for a distribution spanning zero
by mapping each side of it separately. Requires distionary 0.2.0.

### New verbs

- [`trim_left()`](https://distplyr.probaverse.com/reference/trim.md) and
  [`trim_right()`](https://distplyr.probaverse.com/reference/trim.md)
  condition a distribution on one side of a point, with `knot` saying
  what becomes of the mass sitting exactly on it: `"keep"` it (the
  default, since a point is not to one side of itself), `"discard"` it
  with the trimmed side, or `"split"` it. See `?trim`.

- [`graft_left()`](https://distplyr.probaverse.com/reference/graft.md)
  and
  [`graft_right()`](https://distplyr.probaverse.com/reference/graft.md)
  replace one end of a `body` with another model of that end. Name the
  tail either as `tail_excess`, measured from the knot and moved there,
  or as `tail_absolute`, on the body’s own scale and conditioned beyond
  the knot. `knot` names the side that mass sitting exactly on the knot
  belongs to — the `"body"` (the default), the `"tail"`, or `"split"`
  between them. See `?graft`.

### Other changes

- Distributions built by a verb print as a summary rather than unfolding
  every distribution they were built from: each is named with
  [`pretty_name()`](https://distionary.probaverse.com/reference/pretty_name.html),
  and a mixture’s components are listed with their weights.

### Bug fixes

- [`mix()`](https://distplyr.probaverse.com/reference/mix.md),
  [`maximize()`](https://distplyr.probaverse.com/reference/extremum.md)
  and
  [`minimize()`](https://distplyr.probaverse.com/reference/extremum.md)
  no longer treat different components as the same one. Results may
  change for transformed distributions.

- Verbs no longer discard a distribution that merely carries the name
  `"Null"`. Null-ness is read with
  [`is.na()`](https://rdrr.io/r/base/NA.html).

## distplyr 0.2.0

CRAN release: 2025-12-08

- Initial CRAN release.

## distplyr 0.1.5

- Updates to package infrastructure in the process of migrating to a new
  GitHub Organization.

## distplyr 0.1.4

- Fix graft distributions so that they can evaluate on `NA`.

## distplyr 0.1.3

- Default evaluation methods, and base distributional forms, have been
  moved to a new package, distionary. distplyr focusses on manipulation
  verbs only.
- `Math` method now applies to finite distributions.
- `Ops` methods are now available for arithmetic operations (`+`, `-`,
  `*`, and `/`) on a single distribution, along with the verbs
  [`shift()`](https://distplyr.probaverse.com/reference/linear_transform.md),
  [`multiply()`](https://distplyr.probaverse.com/reference/linear_transform.md),
  [`invert()`](https://distplyr.probaverse.com/reference/linear_transform.md),
  and
  [`flip()`](https://distplyr.probaverse.com/reference/linear_transform.md).
- [`graft_left()`](https://distplyr.probaverse.com/reference/graft.md)
  and
  [`graft_right()`](https://distplyr.probaverse.com/reference/graft.md)
  are fully functional, and `slice_left()` and `slice_right()` are now
  also available.

## distplyr 0.1.2

- If you have the tibble package installed, distplyr will now output
  tibbles wherever data frames were previously output.

### Breaking changes

- The `get_` prefix has been removed from distributional quantities.
  `get_mean()` is now [`mean()`](https://rdrr.io/r/base/mean.html), etc.
  - For now, the `get_` prefix still holds for distributional
    representations, like `get_cdf()`.
- Make your own distribution object with
  [`distribution()`](https://distionary.probaverse.com/reference/distribution.html)
  instead of `dst()`, and checked with
  [`is_distribution()`](https://distionary.probaverse.com/reference/distribution.html).

## distplyr 0.1.1

This patch both fixes some problems in the previous release, as well as
offering a step towards a bigger expansion.

- Some change in the functional representations:
  - Changed random number generation from `randfn`, a functional
    representation, to the
    [`realise()`](https://distionary.probaverse.com/reference/realise.html)
    and
    [`realize()`](https://distionary.probaverse.com/reference/realise.html)
    functions.
  - Changed `probfn` representation to be more specific: `pmf` or
    `density`
- Added the `enframe` suite of functions.
- Implement the beginnings of being able to specify your own
  distribution, with the `set_` suite of functions, after making an
  empty distribution with `dst()`.

Additionally, there’s some internal rearrangement, where the `get`
functions call the `eval` functions, not vice versa.

## distplyr 0.1.0

The first version of `distplyr` is now available! Its functionality is
rather limited at the moment, but is still useful, especially for its
capability to handle a discrete component of a distribution. Here are
the main features:

- Base distributions include step distributions, Gaussian, Uniform, and
  generalized Pareto.
- Operations include grafting (right) and mixing
- Distribution properties included are moment-related quantities, and
  extreme value index.
- Distribution representations are mostly comprehensive, perhaps only
  missing mean excess function and moment generating function.

Take a look at the “Vision” vignette to get a sense of where this
package is headed.
