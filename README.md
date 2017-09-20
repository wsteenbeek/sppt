<!-- README.md is generated from README.Rmd. Please edit that file -->
Overview
--------

sppt is an [R](https://www.r-project.org) package that implements Martin Andresen's Spatial Point Pattern Test. It has been published [here](http://jrc.sagepub.com/content/48/1/58) (and elsewhere). The test measures the degree of similarity at the local level between two spatial point patterns and is an area-based test.

This spatial point pattern test is not for the purpose of testing point patterns with the null hypotheses of random, uniform, or clustered distributions, but may be used to compare a particular point pattern with these distributions. One advantage of the test is that it can be performed for a number of different area boundaries using the same original point datasets.

To cite package `sppt` in publications use:

> Wouter Steenbeek, Christophe Vandeviver, Martin Andresen, Nicolas Malleson (2017). *sppt: Spatial Point Pattern Test*. R package version 0.1.4. URL: <https://github.com/wsteenbeek/sppt>

Installation
------------

You can install the package from this [GitHub repository](https://github.com/wsteenbeek/sppt). You first need to install the [devtools](https://CRAN.R-project.org/package=devtools) package.

``` r
install.packages("devtools")
```

Then install sppt using the `install_github` function in the [devtools](https://CRAN.R-project.org/package=devtools) package.

``` r
devtools::install_github("wsteenbeek/sppt")
```

Example
-------

Spatial objects areas.sp, points1.sp, and points2.sp, are included in the package. For example, you can think of these as neighborhoods and the locations where crimes occur in two different years.

``` r
library(sppt)
#> Loading required package: sp
plot(areas.sp)
points(points1.sp, col="blue", pch = 19)
points(points2.sp, col="red", pch = 15)
```

![](man/figures/README-example-1.png)

The main function within the sppt package is also called `sppt`:

``` r
set.seed(93255) # set seed for reproducibility
output <- sppt(points1, points2, areas.sp)
```

The defaults of the `sppt` function are to randomly sampled 85 percent of the points, using 200 repetitions, and a 95 percent confidence interval. So the command above is the same as:

``` r
set.seed(93255) # set seed for reproducibility
output <- sppt(points1, points2, areas.sp,
               nsamples=200, percpoints=85, conf_level=95)
```

The package includes vignettes explaining the sppt procedure in more detail; a worked example of a toy dataset and actual crime data; and a comparison between this R package and an existing Java application that has been written by [Nick Malleson](http://nickmalleson.co.uk/).

You can view which vignettes are available using:

``` r
vignette(package = "sppt")
```

You can read the vignettes using:

``` r
vignette("sppt", package = "sppt")
vignette("sppt_comparison", package = "sppt")
```

License
-------

This package is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License, version 3, as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose. See the GNU General Public License for more details.

A copy of the GNU General Public License, version 3, is available at <https://www.r-project.org/Licenses/GPL-3>
