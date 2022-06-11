# voluModel <img src="man/figures/logo.png" alt="voluModel hex logo" align="right" height="138" />

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

## Summary

This package is designed to facilitate modeling species' ecological niches
and geographic distributions based on occurrences and environments that 
have a vertical as well as horizontal component, and projecting models into 
three-dimensional geographic space. Working in three dimensions is useful in 
an aquatic context when the organisms one wishes to model can be found across 
a wide range of depths in the water column.

Please cite `voluModel`. Run the following to get the appropriate
citation for the version you’re using:

```r 
citation(package = "voluModel")
```
    ## Owens H, Rahbek C (2022). _voluModel: Modeling Species Distributions in Three Dimensions_. doi:
    ## 10.5281/zenodo.6367505 (URL: https://doi.org/10.5281/zenodo.6389839), R package version 0.1.5, <URL:
    ## http://CRAN.R-project.org/package=voluModel>.
    ##
    ## A BibTeX entry for LaTeX users is
    ##
    ##  @Manual{,
    ##    title = {{voluModel}: Modeling Species Distributions in Three Dimensions},
    ##    author = {Hannah L. Owens and Carsten Rahbek},
    ##    year = {2022}
    ##    note = {R package version 0.1.5},
    ##    url = {http://CRAN.R-project.org/package=voluModel},
    ##    doi = {10.5281/zenodo.6389839},
    ##  }


## Installation


``` r
install.packages("voluModel")
```

Or, install GitHub development version:

``` r
devtools::install_github("hannahlowens/voluModel", build_vignettes = TRUE)
```

## Getting Started

1. [Introduction](https://hannahlowens.github.io/voluModel/articles/a_Introduction.html)
2. [Processing Rasters](https://hannahlowens.github.io/voluModel/articles/b_RasterProcessing.html)
3. [Sampling Environtmental Data in 3D](https://hannahlowens.github.io/voluModel/articles/c_DataSampling.html)
4. [Visualization Tools](https://hannahlowens.github.io/voluModel/articles/d_Visualization.html)
5. [Generalized Linear Model Workflow](https://hannahlowens.github.io/voluModel/articles/e_GLMWorkflow.html)

## Meta

-   Please [report any issues or
    bugs](https://github.com/hannahlowens/voluModel/issues).
-   License: ACM
-   Get citation information for `voluModel` in R using
    `citation(package = 'voluModel')`
    
## Acknowledgements

<img src="http://publications.europa.eu/code/images/scan/5000100-flag.jpg" alt="EU flag" align="right" height="100" />

- This project has received funding from the European Union’s Horizon 2020 research and innovation programme under the Marie Skłodowska-Curie grant agreement No 891702. 
