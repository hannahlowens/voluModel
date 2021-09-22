<!-- badges: start -->

[![R-CMD-check](https://github.com/hannahlowens/voluModel/workflows/R-CMD-check/badge.svg)](https://github.com/hannahlowens/voluModel/actions)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Codecov test
coverage](https://codecov.io/gh/hannahlowens/voluModel/branch/main/graph/badge.svg)](https://codecov.io/gh/hannahlowens/voluModel?branch=main)
<!-- badges: end -->

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

-   Vignettes coming soon.

## Meta

-   Please [report any issues or
    bugs](https://github.com/hannahlowens/voluModel/issues).
-   License: GPL-3
-   Get citation information for `voluModel` in R using
    `citation(package = 'voluModel')`
