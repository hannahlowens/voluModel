#' @title Interpolate patchily sampled rasters
#'
#' @description Uses thin plate spline regression from
#' `fields` package to interpolate missing raster values.
#' Note that this is only done in two dimensions, x and y.
#'
#' @param inputRaster An object of class `raster`
#'
#' @return An object of class raster
#'
#' @importFrom fields Tps
#' @importFrom raster raster
#'
#' @examples
#' # Create sample raster
#' r <- raster(ncol=10, nrow=10)
#' values(r) <- 1:100
#'
#' # Introduce a "hole"
#' values(r)[67:70] <- NA
#' interpolatedRaster <- interpolateRaster(r)
#'
#' @keywords dataPrep
#' @export

# Interpolates patchily sampled rasters (2d)
interpolateRaster <- function(inputRaster){
  # Prepare input data
  r <- inputRaster
  ra <- aggregate(r, 4)
  xy <- data.frame(xyFromCell(ra, 1:ncell(ra)))
  v <- getValues(ra)
  # remove NAs
  i <- !is.na(v)
  xy <- xy[i,]
  v <- v[i]

  #### Thin plate spline model
  tps <- Tps(xy, v, lon.lat = T, verbose = F)
  p <- raster(r)

  # use model to predict values at all locations
  p <- interpolate(p, tps, verbose = F)

  # Fill in holes in original raster
  complete <- cover(r, p)

  return(complete)
}
