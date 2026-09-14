## R CMD check results

0 errors | 0 warnings | 0 notes

* This release moves distplyr onto the support objects introduced in distionary
  0.2.0. Distributions built by distplyr's verbs now carry a `.support` instead
  of the defunct `.vtype`. See NEWS.md.

* distplyr 0.2.0, the version currently on CRAN, fails against distionary 0.2.0
  for that reason. This release is the fix, and it is submitted as soon as
  distionary 0.2.0 is available on CRAN, which the `Imports` entry
  `distionary (>= 0.2.0)` requires.

Checked with `R CMD check --as-cran` locally (macOS, R 4.6.0) against
distionary 0.2.0.

## Reverse dependencies

distplyr has one reverse dependency, probaverse, which attaches the suite and
is unaffected by this release.
