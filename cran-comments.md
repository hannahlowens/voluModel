## Test environments
* local OS X 10.15.7, R 4.1.2
* win-builder (devel and release)
* ubuntu 20.04 (devel and release; on GitHub Actions), R 4.1.2
* windows-latest (on GitHub Actions), R 4.1.2
* macOS-latest (on GitHub Actions), R 4.1.2
* rhub

## R CMD check results

0 errors | 0 warnings | 3 notes

* One NOTE occurred in all test environments:

    NOTE: installed size is 6.9Mb

    This is the smallest file size possible, balancing file size with speed of vignette build and extent of vignette-based documentation. Every test environment had some version of this note, although the installed file size varied +/- 1Mb among the environments.

* One NOTE occurred in all rhub environments and local OS X:

   Examples with CPU (user + system) or elapsed time > 5s
                       user system elapsed
   marineBackground 143.882  1.084 150.746
   
    We chose as small a realistic dataset for this example as possible to ensure it would run as quickly as possible. Time elapsed varied +/- 50s depending on environment.

* One NOTE occurred in rhub, win-devel, and win-release:

    NOTE: Non-FOSS package license (ACM)

    This is necessary, as a package on which this package depends has an ACM license.

## Downstream dependencies
* There are no downstream dependencies at this time.
