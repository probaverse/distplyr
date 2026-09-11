# distplyr 0.3.0

Major updates:

- Requires distionary 0.2.0 or later, the release that introduces support
  objects. Every verb below is built on them, so an older distionary
  cannot satisfy this package.

- Verbs now propagate a structured support (the `.support` feature from
  distionary) following the new distionary implementation, so `support()`, 
  the atoms, and discrete/mixed moments work on transformed distributions.

- Verbs no longer specify `range` or `.vtype`; the range and variable type
  are derived from the support. `.vtype` is defunct in distionary, and a
  distribution can no longer be built without a support, so a verb never
  meets an input lacking one --- the fallbacks that guessed at a variable
  type are gone.

- `trim_left()` and `trim_right()` now shift a trim point that lands on a
  flat region (a gap in the support) to where the support resumes, instead
  of erroring. For example, trimming a distribution supported on
  [1, 2] U [4, 5] to the left of 3 yields a distribution on [4, 5]. The
  general trimming method now requires the distribution to carry a
  structured support.

- `invert()` now derives the support of `1 / X` for distributions spanning
  zero, by mapping the negative and positive parts separately.

Bug fixes:

- Fix `mix()`, `maximize()`, and `minimize()` sometimes treating different
  distributions as the same component. You may see different results if you
  combine transformed distributions (for example after `flip()`).

- Fix `trim_left()` and `trim_right()` on a mixture returning the Null
  distribution whenever the trim removed an entire component; dead
  components are now dropped from the mixture instead.

- Fix every verb discarding a distribution that merely carries the name
  `"Null"`. Null-ness is now read from the distribution's class, via
  `is.na()`, rather than by comparing its pretty name, so a distribution
  built with `.name = "Null"` is transformed like any other.

- Fix `trim_left()` and `trim_right()` on the Null distribution raising
  "missing value where TRUE/FALSE needed" instead of returning Null. Both
  now short-circuit on a Null input, as the other verbs already did.

# distplyr 0.2.0

- Initial CRAN release.

# distplyr 0.1.5

- Updates to package infrastructure in the process of migrating to a new GitHub Organization.

# distplyr 0.1.4

- Fix graft distributions so that they can evaluate on `NA`. 

# distplyr 0.1.3

- Default evaluation methods, and base distributional forms, have been moved to a new package, distionary. distplyr focusses on manipulation verbs only.
- `Math` method now applies to finite distributions.
- `Ops` methods are now available for arithmetic operations (`+`, `-`, `*`, and `/`) on a single distribution, along with the verbs `shift()`, `multiply()`, `invert()`, and `flip()`.
- `graft_left()` and `graft_right()` are fully functional, and `slice_left()` and `slice_right()` are now also available. 

# distplyr 0.1.2

- If you have the tibble package installed, distplyr will now output tibbles wherever data frames were previously output.  

## Breaking changes

- The `get_` prefix has been removed from distributional quantities. `get_mean()` is now `mean()`, etc.
	- For now, the `get_` prefix still holds for distributional representations, like `get_cdf()`. 
- Make your own distribution object with `distribution()` instead of `dst()`, and checked with `is_distribution()`. 


# distplyr 0.1.1

This patch both fixes some problems in the previous release, as well as offering a step towards a bigger expansion.

- Some change in the functional representations: 
	- Changed random number generation from `randfn`, a functional representation, to the `realise()` and `realize()` functions. 
	- Changed `probfn` representation to be more specific: `pmf` or `density`
- Added the `enframe` suite of functions.
- Implement the beginnings of being able to specify your own distribution, with the `set_` suite of functions, after making an empty distribution with `dst()`. 

Additionally, there's some internal rearrangement, where the `get` functions call the `eval` functions, not vice versa.

# distplyr 0.1.0

The first version of `distplyr` is now available! Its functionality is rather limited at the moment, but is still useful, especially for its capability to handle a discrete component of a distribution. Here are the main features:

- Base distributions include step distributions, Gaussian, Uniform, and generalized Pareto.
- Operations include grafting (right) and mixing
- Distribution properties included are moment-related quantities, and extreme value index.
- Distribution representations are mostly comprehensive, perhaps only missing mean excess function and moment generating function.

Take a look at the "Vision" vignette to get a sense of where this package is headed.
