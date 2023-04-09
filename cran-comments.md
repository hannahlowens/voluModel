## This is a package update. In this version, I have:

* scaleRange argument added to oneRasterPlot().
* MESS3D now calculated using modEvA::MESS() instead of dismo::mess().
* Removed dependency on `raster`, `sp` and `rgdal`. Raster visualizations now done with `terra` and include optional graticule plotting.
* Added function to generate `SpatRaster` template with input `SpatVector` points centered on template cells.

## Test environments
* local OS X 10.15.7, R 4.1.2
* mac-latest (release), R 4.2.3
* win-builder (devel and release), R 4.2.3
* ubuntu 20.04 (devel, release, and oldrel-1; on GitHub Actions), R 4.2.3
* windows-latest (on GitHub Actions), R 4.2.3
* macOS-latest (on GitHub Actions), R 4.2.3
* rhub

## R CMD check results

0 errors | 0 warnings | 0 notes
    
## Downstream dependencies
* There are no downstream dependencies at this time.
