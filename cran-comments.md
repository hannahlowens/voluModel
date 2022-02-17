## Test environments
* local OS X 10.15.7, R 4.1.2
* win-builder (devel and release)
* ubuntu 20.04 (devel, release, and oldrel-1; on GitHub Actions), R 4.1.2
* windows-latest (on GitHub Actions), R 4.1.2
* macOS-latest (on GitHub Actions), R 4.1.2
* rhub

## R CMD check results

0 errors | 0 warnings | 2 notes

* One NOTE occurred in rhub, win-devel, and win-release:

    NOTE: Non-FOSS package license (ACM)

    This is necessary, unfortunately. The problem is that a function in voluModel relies on generating alpha hulls. The only available package to do this, to my knowledge, is alphahull, which relies on a FORTRAN algorithm that has an ACM license. I have searched exhaustively for replacements, but there are none that I could find.
    
* One NOTE occurred in rhub Windows Server 2022, R-devel, 64 bit:

  NOTE: Examples with CPU (user + system) or elapsed time > 5s
                   user system elapsed
  marineBackground 4.95   0.27    5.21

  

## Downstream dependencies
* There are no downstream dependencies at this time.
