<!-- badges: start -->

[![R-CMD-check](https://github.com/hannahlowens/voluModel/workflows/R-CMD-check/badge.svg)](https://github.com/hannahlowens/voluModel/actions)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/hannahlowens/voluModel/branch/main/graph/badge.svg)](https://codecov.io/gh/hannahlowens/voluModel?branch=main)
[![cran
version](https://www.r-pkg.org/badges/version/voluModel)](https://cran.r-project.org/package=voluModel)
[![DOI](https://zenodo.org/badge/409153439.svg)](https://zenodo.org/badge/latestdoi/409153439)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/voluModel)](https://github.com/r-hub/cranlogs.app)
<!-- badges: end -->

# voluModel <img src='man/figures/logo.png' align="right" height="138" />

## Summary

This package is designed to model species distributions in three
dimensions. This is useful in an aquatic context when the organisms one
wishes to model can be found across a wide range of depths in the water
column.

Please cite `voluModel`. Run the following to get the appropriate
citation for the version you’re using:

``` r
citation(package = "voluModel")
```

    ## Warning in citation(package = "voluModel"): no date field in DESCRIPTION file of
    ## package 'voluModel'

    ## Warning in citation(package = "voluModel"): could not determine year for
    ## 'voluModel' from package DESCRIPTION file

    ## 
    ## To cite package 'voluModel' in publications use:
    ## 
    ##   Hannah Owens (NA). voluModel: Modeling Species Distributions in Three
    ##   Dimensions. R package version 0.0.0.9000.
    ##   https://hannahlowens.github.io/voluModel/
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {voluModel: Modeling Species Distributions in Three Dimensions},
    ##     author = {Hannah Owens},
    ##     note = {R package version 0.0.0.9000},
    ##     url = {https://hannahlowens.github.io/voluModel/},
    ##   }

## Installation

Currently, there is only a development version.

``` r
devtools::install_github("hannahlowens/voluModel")
```

``` r
library("voluModel")
```

## Getting Started

-   voluModel introduction vignette
    (<https://hannahlowens.github.io/voluModel/articles/Simple.html>)

## Meta

-   Please [report any issues or
    bugs](https://github.com/hannahlowens/voluModel/issues).
-   License: GPL-3
-   Get citation information for `voluModel` in R using
    `citation(package = 'voluModel')`
