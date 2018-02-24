# sppt 0.1.5

## New features

* New function `sppt_diff()` added. This function calculates the difference
  in proportions between Base and Test points using standard 'proportion
  difference' tests. Having zero points in an areal unit will not lead to
  a confidence interval of 0% to 0% (as in `sppt()` and `sppt_boot()`),
  making is a more conservative test. See Wheeler, A., Steenbeek, W., &
  Andresen, M.A. (2018). Testing for Similarity in Area-Based Spatial Patterns:
  Alternative Methods to Andresen's Spatial Point Pattern Test.
  (SSRN: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3111822)

* New function `sppt_boot()` added. This is a full bootstrapping procedure
  to measure the difference in proportions between Base and Test points. In
  contrast to the original `sppt()`, the results of the test are not dependent
  on which dataset is selected as Base and which is selected as Test.

# sppt 0.1.4

First published version of the `sppt` package. Includes function `sppt()`.
