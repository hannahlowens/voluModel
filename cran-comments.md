## Test environments
* local OS X 10.15.7, R 4.1.2
* win-builder (devel and release)
* ubuntu 20.04 (devel, release, and oldrel-1; on GitHub Actions), R 4.1.2
* windows-latest (on GitHub Actions), R 4.1.2
* macOS-latest (on GitHub Actions), R 4.1.2
* rhub

## R CMD check results

0 errors | 0 warnings | 1 note

* One NOTE occurred in all rhub environments, win-devel, and win-release:

    NOTE: Non-FOSS package license (ACM)

    This is necessary, unfortunately. The problem is that a function in voluModel relies on generating alpha hulls. The only available package to do this, to my knowledge, is alphahull, which relies on a FORTRAN algorithm that has an ACM license. I have searched exhaustively for replacements, but there are none that I could find.
    
## Downstream dependencies
* There are no downstream dependencies at this time.
