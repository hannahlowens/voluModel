#' @title Sampling from a `SpatRaster` vector using 3D coordinates
#'
#' @description Gets values at x,y,z occurrences from a
#' given 3D environmental variable brick
#'
#' @param occs A `data.frame` with at least three columns
#' named "longitude", "latitude", and "depth", or that
#' can be coerced into this format.
#'
#' @param envBrick A `SpatRaster` vector object with
#' one environmental variable. Each layer represents
#' a depth slice. See Details for more information.
#'
#' @param verbose `logical`. Switching to `FALSE` mutes message
#' describing which columns in `occs1` and `occs2` are interpreted
#' as x, y, and z coordinates.
#'
#' @details The `SpatRaster` vector object should
#' have numeric names that correspond with the beginning
#' depth of a particular depth slice. For example, one
#' might have three layers, one from 0 to 10m, one from
#' 10 to 30m, and one from 30 to 100m. You would name the
#' layers in this brick `names(envBrick) <- c(0, 10, 30`.
#' `xyzSample` identifies the layer name that is closest
#' to the depth layer value at a particular X, Y
#' coordinate, and samples the environmental value at that
#' 3D coordinate.
#'
#' @return Vector of environmental values equal in length
#' to number of rows of input `occs` `data.frame`.
#'
#' @examples
#' library(terra)
#'
#' # Create test raster
#' r1 <- rast(ncol=10, nrow=10)
#' values(r1) <- 1:100
#' r2 <- rast(ncol=10, nrow=10)
#' values(r2) <- c(rep(20, times = 50), rep(60, times = 50))
#' r3 <- rast(ncol=10, nrow=10)
#' values(r3) <- 8
#' envBrick <- c(r1, r2, r3)
#' names(envBrick) <- c(0, 10, 30)
#'
#' # Create test occurrences
#' set.seed(0)
#' longitude <- sample(ext(envBrick)[1]:ext(envBrick)[2],
#'                     size = 10, replace = FALSE)
#' set.seed(0)
#' latitude <- sample(ext(envBrick)[3]:ext(envBrick)[4],
#'                    size = 10, replace = FALSE)
#' set.seed(0)
#' depth <- sample(0:35, size = 10, replace = TRUE)
#' occurrences <- as.data.frame(cbind(longitude,latitude,depth))
#'
#' # Test function
#' occSample3d <- xyzSample(occurrences, envBrick)
#'
#' # How to use
#' occurrences$envtValue <- occSample3d
#'
#' @importFrom terra rast extract
#'
#' @keywords dataPrep
#'
#' @export

xyzSample <- function(occs, envBrick, verbose = TRUE){
  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(ncol(occs) < 3){
    warning(paste0("'occs' must have at least three columns.\n"))
    return(NULL)
  }

  if(!inherits(envBrick, what = "SpatRaster")){
    warning(paste0("'envBrick' must be of class 'SpatRaster'.\n"))
    return(NULL)
  }

  if (!is.logical(verbose)) {
    warning(message("Argument 'verbose' is not of type 'logical'.\n"))
    return(NULL)
  }

  # Checking for appropriate environmental layer names
  suppressWarnings(layerNames <- as.numeric(names(envBrick)))
  if(any(is.na(layerNames))){
    warning(message("\nInput RasterBrick names inappropriate: \n",
            paste(names(envBrick), collapse = ", "), "\n",
            "Names must be numeric.\n"))
    return(NULL)
  }

  # Parse columns
  colNames <- colnames(occs)
  colParse <- columnParse(occs, wDepth = TRUE)
  if(is.null(colParse)){
    return(NULL)
  }
  xIndex <- colParse$xIndex
  yIndex <- colParse$yIndex
  zIndex <- colParse$zIndex
  interp <- colParse$reportMessage

  if(verbose){
    message(interp)
  }

  # Sampling values
  sampledValues <- NULL
  index <- unlist(lapply(occs[,zIndex], FUN = function(x) which.min(abs(layerNames - x))))
  occs$index <- index
  occs$sampledValues <- rep(NA, times = nrow(occs))
  indices <- unique(occs$index)
  for(i in indices){
    occs[occs$index == i,]$sampledValues <- terra::extract(x = envBrick[[i]],
                                                           y = occs[occs$index == i,c(xIndex,
                                                                                      yIndex)])[,2]
  }
  return(occs$sampledValues)
}
