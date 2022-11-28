#' @title Occurrence cell removal
#'
#' @description Removes cells from raster that contain occurrences
#'
#' @param occs A `data.frame` with at least two columns
#' named "longitude" and "latitude" or that
#' can be coerced into this format.
#'
#' @param rasterTemplate A `Raster*` object to serve
#' as a template for cells to be removed.
#'
#' @details This is an internal function to remove cells that
#' intersect with occurrences from a `Raster*` template object. This
#' template can then be overlaid onto a `RasterStack` or
#' `RasterBrick` to remove occurrences from all layers.
#'
#' @return A `Raster*`
#'
#' @examples
#'
#' library(terra)
#' # Create sample raster
#' r <- rast(ncol=10, nrow=10)
#' values(r) <- 1:100
#'
#' # Create test occurrences
#' set.seed(0)
#' longitude <- sample(ext(r)[1]:ext(r)[2],
#'                     size = 10, replace = FALSE)
#' set.seed(0)
#' latitude <- sample(ext(r)[3]:ext(r)[4],
#'                    size = 10, replace = FALSE)
#' occs <- data.frame(longitude, latitude)
#'
#' # Here's the function
#' result <- occCellRemoval(occs = occs, rasterTemplate = r)
#'
#' @import terra
#'
#' @keywords internal
#'
#' @noRd

occCellRemoval <- function(occs, rasterTemplate){
  # Handling alternative column names for occurrences
  colNames <- colnames(occs)
  cp <- columnParse(occs)
  xIndex <- cp$xIndex
  yIndex <- cp$yIndex

  # Meat of function
  occCells <- cellFromXY(object = rasterTemplate, occs[,c(xIndex,yIndex)])
  rasterTemplate[occCells] <- NA
  return(rasterTemplate)
}

#' @title 2D background sampling
#'
#' @description Samples in 2D at resolution of raster
#'
#' @param occs A dataframe with at least two columns
#' named "longitude" and "latitude", or that can be
#' coerced into this format.
#'
#' @param rasterTemplate A `Raster*` object to serve
#' as a template for generating background sampling
#' coordinates.
#'
#' @param mShp A shapefile defining the area from
#' which background points should be sampled.
#'
#' @param verbose `logical`. Switching to `FALSE` mutes message describing
#' which columns in `occs` are interpreted as x and y coordinates.
#'
#' @details This function is designed to sample background points
#' for distributional modeling in two dimensions. The returned
#' `data.frame` contains all points from across the designated
#' background. It is up to the user to determine how to
#' appropriately sample from those background points.
#'
#' @return A `data.frame` with 2D coordinates of points
#' for background sampling.
#'
#' @examples
#' library(terra)
#'
#' # Create sample raster
#' r <- rast(ncol=10, nrow=10)
#' values(r) <- 1:100
#'
#' # Create test occurrences
#' set.seed(0)
#' longitude <- sample(ext(r)[1]:ext(r)[2],
#'                     size = 10, replace = FALSE)
#' set.seed(0)
#' latitude <- sample(ext(r)[3]:ext(r)[4],
#'                    size = 10, replace = FALSE)
#' occurrences <- data.frame(longitude,latitude)
#'
#' # Generate background sampling buffer
#' buffPts <- vect(occurrences,
#'                 c("longitude", "latitude"))
#' crs(buffPts) <- crs(r)
#' mShp <- aggregate(buffer(buffPts, width = 1000000))
#'
#' # Here's the function
#' result <- mSampling2D(occs = occurrences, rasterTemplate = r, mShp = mShp)
#'
#' @import raster
#'
#' @keywords backgroundSampling
#'
#' @export

mSampling2D <- function(occs, rasterTemplate, mShp, verbose = TRUE){
  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(!grepl("SpatRaster", class(rasterTemplate))){
    warning(paste0("'rasterTemplate' must be of class 'SpatRaster'.\n"))
    return(NULL)
  }

  if(!is(mShp, "SpatVector")){
    warning(paste0("'mShp' must be of class 'SpatVector'.\n"))
    return(NULL)
  }

  if (!is.logical(verbose)) {
    warning(paste0("Argument 'verbose' is not of type 'logical'.\n"))
    return(NULL)
  }

  # Parse columns
  colNames <- colnames(occs)
  colParse <- columnParse(occs)
  if(is.null(colParse)){
    return(NULL)
  }
  xIndex <- colParse$xIndex
  yIndex <- colParse$yIndex
  interp <- colParse$reportMessage

  if(verbose){
    message(interp)
  }

  rasterTemplate <- crop(x = mask(x = rasterTemplate, mask = mShp), y = mShp)
  rawLayer <- occCellRemoval(occs = occs[,c(xIndex,yIndex)],
                             rasterTemplate)
  mPts <- data.frame(xyFromCell(rawLayer, cell = 1:ncell(rawLayer)))
  mPts$extract <- extract(x = rawLayer, y = mPts)[,2]
  mPts <- mPts[!is.na(mPts$extract),1:2]
  colnames(mPts) <- colNames[c(xIndex, yIndex)]
  return(mPts)
}

#' @title 3D background sampling
#'
#' @description Samples XYZ coordinates from a shapefile
#' from maximum to minimum occurrence depth at XYZ
#' resolution of envBrick.
#'
#' @param occs A `data.frame` with at least three columns
#' named "longitude", "latitude", and "depth", or that
#' can be coerced into this format.
#'
#' @param envBrick A `RasterBrick` object to serve
#' as a template for generating background sampling
#' coordinates.
#'
#' @param mShp A shapefile defining the area from
#' which background points should be sampled.
#'
#' @param depthLimit An argument controlling the depth
#' extent of sampling. Refer to `Details` for more information.
#'
#' @param verbose `logical`. Switching to `FALSE` mutes message describing
#' which columns in `occs` are interpreted as x, y, and z coordinates.
#'
#' @details This function is designed to sample background points for
#' distributional modeling in three dimensions. If a voxel (3D pixel)
#' in the `envBrick` intersects with an occurrence from `occs`, it is
#' removed. Note that this function returns points representing every
#' voxel in the background area within the specified depth range. It
#' is up to the user to downsample from these data as necessary,
#' depending on the model type being used.
#'
#' `depthLimit` argument options:
#' \itemize{
#'   \item `occs` Samples background from the full depth extent of `occs`.
#'   \item `all` Samples background from the full depth extent of `envBrick`.
#'   \item A `vector` of length 2 with maximum and minimum depth values from
#'   which to sample.
#' }
#'
#' @return A `data.frame` with 3D coordinates of points for background
#' sampling.
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
#' occurrences <- data.frame(longitude,latitude,depth)
#'
#' # Generate background sampling buffer
#' buffPts <- vect(occurrences,
#'                 c("longitude", "latitude"))
#' crs(buffPts) <- crs(r)
#' mShp <- aggregate(buffer(buffPts, width = 1000000))
#'
#' # Here's the function
#' occSample3d <- mSampling3D(occs = occurrences,
#'                            envBrick = envBrick,
#'                            mShp = mShp,
#'                            depthLimit = "occs")
#'
#' @import terra
#'
#' @keywords backgroundSampling
#'
#' @export

mSampling3D <- function(occs, envBrick, mShp, depthLimit = "all", verbose = TRUE){
  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(ncol(occs) < 3){
    warning(paste0("'occs' must have at least three columns.\n"))
    return(NULL)
  }

  if(!is(envBrick, "SpatRaster")){
    warning(paste0("'envBrick' must be of class 'SpatRaster'.\n"))
    return(NULL)
  }

  if(!is(mShp, "SpatVector")){
    warning(paste0("'mShp' must be of class 'SpatVector'.\n"))
    return(NULL)
  }

  if(!(class(depthLimit) %in% c("character","numeric"))){
    warning(paste0("'depthLimit' must be of class 'character' or 'numeric'.\n"))
    return(NULL)
  }

  if(is(depthLimit, "numeric")){
    if(length(depthLimit) != 2){
      warning(paste0("'depthLimit' arguments of 'numeric' must be of length 2.\n"))
      return(NULL)
    }
  }

  if(is(depthLimit, "character")){
    if(length(depthLimit) > 1){
      warning(paste0(depthLimit, " is not a valid value for 'depthLimit'\n"))
      return(NULL)
    } else if(!(depthLimit %in% c("all", "occs"))){
      warning(paste0(depthLimit, " is not a valid value for 'depthLimit'\n"))
      return(NULL)
    }
  }

  if (!is.logical(verbose)) {
    warning(paste0("Argument 'verbose' is not of type 'logical'.\n"))
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

  # Checking for appropriate environmental layer names
  layerNames <- as.numeric(gsub("[X]", "", names(envBrick)))
  if(sum(is.na(layerNames)) > 0){
    message("\nInput RasterBrick names inappropriate: \n",
            paste(names(envBrick), collapse = ", "), "\n",
            "Names must follow the format 'X' ",
            "followed by a number corresponding to ",
            "the starting depth of the layer.")
    return(NULL)
  }

  # Depth slice indices for occurrences
  occs$index <- unlist(lapply(occs[,zIndex],
                              FUN = function(x) which.min(abs(layerNames - x))))

  # Get depth range
  if(is(depthLimit, "numeric")){
    depthRange <- c(which.min(abs(layerNames - min(depthLimit))),
                    which.min(abs(layerNames - max(depthLimit))))
  } else if(depthLimit == "occs"){
    depthRange <- c(min(occs$index), max(occs$index))
  } else {
    depthRange <- c(1, nlyr(envBrick))
  }

  envBrick <- crop(x = mask(x = envBrick[[depthRange[[1]]:depthRange[[2]]]],
                        mask = mShp), y = mShp)
  mPts <- data.frame()
  for(i in 1:length(names(envBrick))){
    rawLayer <- envBrick[[i]]
    layerDepth <- as.numeric(gsub("[X]", "", names(rawLayer)))
    occsAtLayerDepth <- occs[occs$index == match(layerDepth, layerNames),]
    if (nrow(occsAtLayerDepth) > 0){
      rawLayer <- occCellRemoval(occs = occsAtLayerDepth[,c(xIndex,yIndex)],
                                 rawLayer)
    }
    tempPoints <- data.frame(xyFromCell(rawLayer,
                                        cell = 1:ncell(rawLayer)))
    tempPoints$extract <- extract(x = rawLayer, y = tempPoints)[,2]
    tempPoints <- tempPoints[!is.na(tempPoints$extract),1:2]
    colnames(tempPoints) <- colNames[c(xIndex, yIndex)]
    tempPoints[,zIndex] <- rep(layerDepth, times = nrow(tempPoints))
    mPts <- rbind(mPts, tempPoints)
  }
  colnames(mPts)[[3]] <- colNames[[zIndex]]
  return(mPts)
}
