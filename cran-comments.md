## Test environments
* local OS X 10.15.7, R 4.0.2
* win-builder (devel and release)
* ubuntu 20.04 (devel and release; on GitHub Actions), R 4.1.2
* windows-latest (on GitHub Actions), R 4.1.2
* macOS-latest (on GitHub Actions), R 4.1.2
* rhub

## R CMD check results

0 errors | 0 warnings | 3 notes

* This is a new release.
* One NOTE occurred in all test environments:

    NOTE: installed size is 110.8Mb

    This is the smallest file size possible, balancing file size with speed of vignette build. Every test environment had some version of this note, although the installed file size varied +/- 5Mb among the environments.

* One NOTE occurred in all test environments except local OS X:

    NOTE: Examples with CPU (user + system) or elapsed time > 5s

                         user system elapsed
      marineBackground 152.34   0.74  153.14
   
    We chose as small a realistic dataset for this example as possible to ensure it would run as quickly as possible. Every test environmental had some version of this note, although the elapsed time varied +/- 10s.

* One NOTE occurred in rhub, win-devel, and win-release:

    NOTE: Non-FOSS package license (ACM)

    This is necessary, as a package on which this package depends has an ACM license.

## Downstream dependencies
* There are no downstream dependencies at this time.
