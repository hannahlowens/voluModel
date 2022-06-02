## This is a package update. In this version, I have:

* Addressed an error on some platforms when "buff" in marineBackground() was not specified.

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

Maintainer: ‘Hannah L. Owens <hannah.owens@gmail.com>’

New submission

Package was archived on CRAN

Non-FOSS package license (ACM)

Possibly misspelled words in DESCRIPTION:
  Casal (26:30)
  DL (24:37)
  Feldman (24:62)
  Furrer (25:13)
  JA (24:81)
  Nychka (25:3)
  Pateiro (26:3)
  Rabosky (24:9, 24:29)
  rasters (23:34)
  Sain (25:32)

    The package was archived before the source of the error was identified. The ACM license is necessary, unfortunately. The problem is that a function in voluModel relies on generating alpha hulls. The only available package to do this, to my knowledge, is alphahull, which relies on a FORTRAN algorithm that has an ACM license. I have searched exhaustively for replacements, but there are none that I could find. The "possibly misspelled words" are all correct.
    
## Downstream dependencies
* There are no downstream dependencies at this time.
