#' @title Interpolate patchily sampled rasters
#'
#' @description Uses thin plate spline regression from
#' `fields` package to interpolate missing two-dimensional
#' raster values.
#'
#' @param inputRaster An object of class `SpatRaster`
#' @param fast A logical operator. Setting to `TRUE` triggers use
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
#' \donttest{
#' library(terra)
#' library(fields)
#' # Create sample raster
#' r <- rast(ncol=50, nrow=50)
#' values(r) <- 1:2500
#'
#' # Introduce a "hole"
#' values(r)[c(117:127, 167:177, 500:550)] <- NA
#' plot(r)
#'
#' # Patch hole with interpolateRaster
#' interpolatedRaster <- interpolateRaster(r)
#' plot(interpolatedRaster)
#' fastInterp <- interpolateRaster(r, fast = TRUE, aRange = 3.0)
#' plot(fastInterp)
#' }
#'
#' @import terra
#' @importFrom fields Tps fastTps
#'
#' @seealso \code{\link[fields]{Tps}}, \code{\link[fields]{fastTps}}
#'
#' @keywords dataPrep
#' @export

# Interpolates patchily sampled rasters (2d)
interpolateRaster <- function(inputRaster, fast = FALSE, ...){
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

  if("method" %in% names(args)){
    method <- args$method
  } else{
    method <- "GCV"
  }

  # Input error check
  if (!is(inputRaster, "SpatRaster")) {
    warning(paste0("inputRaster is not of class 'SpatRaster'.\n"))
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
  if (!(method %in% c("GCV", "GCV.model", "GCV.one", "RMSE", "pure error", "REML"))) {
    warning(paste0("Argument 'method' is not a recognized value.\n"))
    return(NULL)
  }

  # Prepare input data
  r <- inputRaster
  ra <- terra::aggregate(r, 4, na.rm = TRUE)
  xy <- data.frame(crds(ra, na.rm = FALSE))
  v <- values(ra)
  # remove NAs
  i <- -c(which(is.na(v)))
  if(length(i) == 0){
    i <- 1:nrow(xy)
  }
  xy <- xy[i,]
  v <- v[i]

  #### Thin plate spline model
  if(fast){
    tps <- fastTps(xy, v, lon.lat = lon.lat, aRange = aRange, REML = REML,
                   verbose = F)
  } else {
    tps <- Tps(xy, v, lon.lat = lon.lat, method = method, verbose = FALSE)
  }

  p <- rast(r)

  # use model to predict values at all locations
  p <- interpolate(p, tps, verbose = FALSE)

  # Fill in holes in original raster
  complete <- cover(r, p)

  return(complete)
}

#' @title Smooth rasters
#'
#' @description Uses thin plate spline regression from
#' `fields` package to smooth raster values.
#'
#' @param inputRaster An object of class `SpatRaster`
#' @param fast A logical operator. Setting to `TRUE` triggers use
#' of `fastTps` instead of `Tps`.
#' @param ... For any additional arguments passed to `Tps` or `fastTps`
#'
#' @details Original raster is smoothed using a thin
#' plate spline. This may be desirable in cases where
#' the user has a reasonable expectation of spatial autocorrelation,
#' but observes putative measurement errors in a raster. The user has
#' the option of choosing `fastTps` to speed calculation,
#' but be advised that this is only an approximation
#' of a true thin plate spline.
#'
#' @return An object of class `SpatRaster`
#'
#' @examples
#' library(terra)
#' library(fields)
#' # Create sample raster
#' r <- rast(ncol=100, nrow=100)
#' values(r) <- 1:10000
#'
#' # Introduce a "bubble"
#' values(r)[720:725] <- 9999
#' plot(r)
#'
#' # Smooth bubble with smoothRaster
#' fastSmooth <- smoothRaster(r, fast = TRUE, aRange = 10.0)
#' plot(fastSmooth)
#'
#' @import terra
#' @importFrom fields Tps fastTps
#'
#' @seealso \code{\link[fields]{Tps}}, \code{\link[fields]{fastTps}}
#'
#' @keywords dataPrep
#' @export

smoothRaster <- function(inputRaster, fast = FALSE, ...){
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

  if("method" %in% names(args)){
    method <- args$method
  } else{
    method <- "GCV"
  }

  # Input error check
  if (!is(inputRaster, "SpatRaster")) {
    warning(paste0("inputRaster is not of class 'SpatRaster'.\n"))
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
  if (!(method %in% c("GCV", "GCV.model", "GCV.one", "RMSE", "pure error", "REML"))) {
    warning(paste0("Argument 'method' is not a recognized value.\n"))
    return(NULL)
  }

  # Prepare input data
  r <- inputRaster
  ra <- terra::aggregate(r, 4, na.rm = TRUE)
  xy <- data.frame(crds(ra, na.rm = FALSE))
  v <- values(ra)
  # remove NAs
  i <- -c(which(is.na(v)))
  if(length(i) == 0){
    i <- 1:nrow(xy)
  }
  xy <- xy[i,]
  v <- v[i]

  #### Thin plate spline model
  if(fast){
    tps <- fastTps(xy, v, lon.lat = lon.lat, aRange = aRange, REML = REML,
                   verbose = FALSE)
  } else {
    tps <- Tps(xy, v, lon.lat = lon.lat, method = method, verbose = FALSE)
  }

  p <- rast(r)

  # use model to predict values at all locations
  p <- interpolate(p, tps, verbose = FALSE)

  return(p)
}
