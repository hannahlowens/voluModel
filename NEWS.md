# voluModel 0.1.8

- Addressed ERRORs on CRAN resulting from package updates on which voluModel depends.
- marineBackground() updated to use terra and sf packages, now returns a SpatVector object instead of a SpatialPolygons object.
- Vignettes updated to reflect the changes.

# voluModel 0.1.7

- Default rasterComp() plotting colors have been adjusted to increase contrast
- oneRasterPlot() adjusted to accept "n" argument for viridis() color scales
- MOre explicit code in "Introduction"" and "Visualization"" vignettes 
- Added verbose switch to mute messages about which columns are being interpreted as x, y, and/or z
coordinates to all relevant functions (default verbose = TRUE)

# voluModel 0.1.6

- Updated license from ACM to GPL-3
- Fixed a bug in error handling for marineBackground()

# voluModel 0.1.5

- Addressed an error on some platforms when "buff" in marineBackground() was not specified.

# voluModel 0.1.4

- Updated metadata on funding source

# voluModel 0.1.3

- Adjust testing to comply with CRAN

# voluModel 0.1.2

- Overhauled vignettes--more detailed, special topics
- Reduced space footprint, increased speed

# voluModel 0.1.0

- First release for CRAN
- Added Zenodo-generated DOI

# voluModel 0.0.0.9000

- The package was created.
