# sppt 0.2.0

## Important changes

* Function `sppt_diff()` changed to have "Chi2_Nmin1" as the default test. This
  means that a function call to `sppt_diff()` will give different results than
  the same call up to version 0.2.0, as the latter had "Yates" as default.

* Function `sppt_diff()` updated to have a variety of tests.

* Function `sppt_diff()` now uses the p-values throughout (instead of confidence 
  intervals) to decide on statistical significance. This makes it easy to adjust
  the p-values (and decisions to reject the null) for multiple comparisons,
  the default setting.

* Function `sppt_diff()` no longer uses exact2x2 package as fisher.test reports
  the same p-value (only the confidence intervals were different, which are no
  longer used.)
  
* Arguments of `sppt_boot()` changed. Instead of 'replacement' now 'bootstrap'
  is used. 'percpoints' default is set to 85, but is ignored if bootstrap = TRUE.
  This means that in the default behavior, a bootstrap sample (i.e. 100% of
  points are samples with replacement) is drawn for both Base and Test. If
  bootstrap = FALSE, the percpoints (default 85) is used in a subsampling method
  for both Base and Test (i.e. as in Andresen's original test, but also for Base).

* Added argument 'bootstrap' to `sppt()`. In the default behavior, bootstrap =
  FALSE and `sppt()` will be Andresen's original test (i.e. subsampling Test
  only with percpoints sampling without replacement). When bootstrap = TRUE,
  'percpoints' is ignored and a bootstrapped sample (i.e. 100% of Test points
  sampled WITH replacement) is used for Test.

## Misc

* Vignette sppt renamed into sppt_intro

* Vignette sppt_diff updated with new information.

* LICENSE file added

* README updated

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

* First published version of the `sppt` package. Includes function `sppt()`.
