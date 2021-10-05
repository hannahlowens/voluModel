#' @title Interpolate patchily sampled rasters
#'
#' @description Uses thin plate spline regression from
#' `fields` package to interpolate missing two-dimensional
#' raster values.
#'
#' @param inputRaster An object of class `raster`
#' @param fast A logical operator. Setting to `T` triggers use
#' of `fastTps` instead of `Tps`.
#' @param ... For any additional arguments passed to `Tps` or `fastTps`
#'
#' @details Missing data values from original raster
#' are replaced with interpolated values. User has the
#' option of choosing `fastTps` to speed calculation,
#' but be advised that this is only an approximation
#' of a true thin plate spline.
#'
#' @return An object of class raster
#'
#' @examples
#' library(raster)
#' library(fields)
#' # Create sample raster
#' r <- raster(ncol=100, nrow=100)
#' values(r) <- 1:10000
#'
#' # Introduce a "hole"
#' values(r)[67:70] <- NA
#'
#' # Patch hole with interpolateRaster
#' interpolatedRaster <- interpolateRaster(r)
#' fastInterp <- interpolateRaster(r, fast = TRUE, aRange = 3.0)
#'
#' @import raster
#' @import fields
#'
#' @seealso \code{\link[fields]{Tps}}, \code{\link[fields]{fastTps}}
#'
#' @keywords dataPrep
#' @export

# Interpolates patchily sampled rasters (2d)
interpolateRaster <- function(inputRaster, fast = F, ...){
  args <- list(...)

  if("lon.lat" %in% names(args)){
    lon.lat <- args$lon.lat
  } else{
    lon.lat <- FALSE
  }

  if("aRange" %in% names(args)){
    aRange <- args$aRange
  } else{
    aRange <- NA
  }

  if("theta" %in% names(args)){
    aRange <- args$theta # Theta and aRange are same, per fields docs
  }

  if("REML" %in% names(args)){
    REML <- args$REML
  } else{
    REML <- FALSE
  }

  # Input error check
  if (!class(inputRaster) == "RasterLayer") {
    warning(paste0("inputRaster is not of class 'RasterLayer'.\n"))
    return(NULL)
  }
  if (!is.logical(fast)) {
    warning(paste0("Argument 'fast' is not of type 'logical'.\n"))
    return(NULL)
  }
  if (!is.logical(lon.lat)) {
    warning(paste0("Argument 'lon.lat' is not of type 'logical'.\n"))
    return(NULL)
  }
  if(fast && is.na(aRange)){
    warning(paste0("'aRange' or 'theta' must be specified when using `fastTps()`.\n"))
    return(NULL)
  }
  if(fast && !is.numeric(aRange)){
    warning(paste0("'aRange' must be numeric.\n"))
    return(NULL)
  }
  if (!is.logical(REML)) {
    warning(paste0("Argument 'REML' is not of type 'logical'.\n"))
    return(NULL)
  }

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
  if(fast){
    tps <- fastTps(xy, v, lon.lat = lon.lat, aRange = aRange, REML = REML, verbose = F)
  } else {
    tps <- Tps(xy, v, lon.lat = lon.lat, verbose = F)
  }

  p <- raster(r)

  # use model to predict values at all locations
  p <- interpolate(p, tps, verbose = F)

  # Fill in holes in original raster
  complete <- cover(r, p)

  return(complete)
}

#' @title Smooth rasters
#'
#' @description Uses thin plate spline regression from
#' `fields` package to smooth raster values.
#'
#' @param inputRaster An object of class `raster`
#' @param fast A logical operator. Setting to `T` triggers use
#' of `fastTps` instead of `Tps`.
#' @param ... For any additional arguments passed to `Tps` or `fastTps`
#'
#' @details Original raster
#' are replaced with interpolated values. User has the
#' option of choosing `fastTps` to speed calculation,
#' but be advised that this is only an approximation
#' of a true thin plate spline.
#'
#' @return An object of class `RasterLayer`
#'
#' @examples
#' library(raster)
#' library(fields)
#' # Create sample raster
#' r <- raster(ncol=100, nrow=100)
#' values(r) <- 1:10000
#'
#' # Introduce a "bubble"
#' values(r)[520:525] <- 9999
#'
#' # Smooth bubble with smoothRaster
#' interpolatedRaster <- smoothRaster(r)
#' fastInterp <- smoothRaster(r, fast = TRUE, aRange = 3.0)
#'
#' @import raster
#' @import fields
#'
#' @seealso \code{\link[fields]{Tps}}, \code{\link[fields]{fastTps}}
#'
#' @keywords dataPrep
#' @export

smoothRaster <- function(inputRaster, fast = F, ...){
  args <- list(...)

  if("lon.lat" %in% names(args)){
    lon.lat <- args$lon.lat
  } else{
    lon.lat <- FALSE
  }

  if("aRange" %in% names(args)){
    aRange <- args$aRange
  } else{
    aRange <- NA
  }

  if("theta" %in% names(args)){
    aRange <- args$theta # Theta and aRange are same, per fields docs
  }

  if("REML" %in% names(args)){
    REML <- args$REML
  } else{
    REML <- FALSE
  }

  # Input error check
  if (!class(inputRaster) == "RasterLayer") {
    warning(paste0("inputRaster is not of class 'RasterLayer'.\n"))
    return(NULL)
  }
  if (!is.logical(fast)) {
    warning(paste0("Argument 'fast' is not of type 'logical'.\n"))
    return(NULL)
  }
  if (!is.logical(lon.lat)) {
    warning(paste0("Argument 'lon.lat' is not of type 'logical'.\n"))
    return(NULL)
  }
  if(fast && is.na(aRange)){
    warning(paste0("'aRange' or 'theta' must be specified when using `fastTps()`.\n"))
    return(NULL)
  }
  if(fast && !is.numeric(aRange)){
    warning(paste0("'aRange' must be numeric.\n"))
    return(NULL)
  }
  if (!is.logical(REML)) {
    warning(paste0("Argument 'REML' is not of type 'logical'.\n"))
    return(NULL)
  }

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
  if(fast){
    tps <- fastTps(xy, v, lon.lat = lon.lat, aRange = aRange, REML = REML, verbose = F)
  } else {
    tps <- Tps(xy, v, lon.lat = lon.lat, verbose = F)
  }

  p <- raster(r)

  # use model to predict values at all locations
  p <- interpolate(p, tps, verbose = F)

  return(p)
}
