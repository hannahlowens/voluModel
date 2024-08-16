## This is a package update. In this version, I have:

* Removed use of sf package in `e_GLMWorkflow` vignette.
* Added possibility to specify method in `interpolateRaster()` and `smoothRaster()` using `method` argument as in `fields::Tps()`.
* Added `verticalTransect()` for visualizing vertical slices through multi-layered `spatRaster` 
objects.

## Test environments
* local OS X 14.6.1, R 4.4.1
* mac-latest (release), R 4.4.1
* win-builder (devel and release), R 4.4.1
* ubuntu 22.04.3 (devel, release, and oldrel-1; on GitHub Actions), R 4.4.1
* windows-latest (on GitHub Actions), R 4.4.1
* macOS-latest (on GitHub Actions), R 4.4.1

## R CMD check results

0 errors | 0 warnings | 0 notes
    
## Downstream dependencies
* There are no downstream dependencies at this time.
