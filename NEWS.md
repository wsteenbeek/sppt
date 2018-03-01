# sppt 0.1.6

* Adjusted function `sppt_diff()` to use `exact2x2::exact2x2()` instead
  of `stats::fisher.test` as the former matches the different two-sided 
  conditional exact tests for 2x2 tables with the appropriate confidence 
  intervals. See https://cran.r-project.org/package=exact2x2 for details.

* Added information on the `exact2x2::exact2x2()` to vignette
  'Proportion difference tests'.

* README updated

* Moved helper functions such as `summary.sppt()` to separate R file.

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

## Misc

* Changed README.md and vignettes to facilitate easier building of vignettes on 
  local machine.
  

# sppt 0.1.4

First published version of the `sppt` package. Includes function `sppt()`.
