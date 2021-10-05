#' @title Occurrence cell removal
#'
#' @description Removes cells from raster that contain occurrences
#'
#' @param occs A dataframe with at least two columns
#' named "longitude" and "latitude" or that
#' can be coerced into this format.
#'
#' @param rasterTemplate
#'
#' @details This is an internal function to remove cells that
#' intersect with occurrences from a `Raster*` template object. This
#' template can then be overlaid onto a `RasterStack` or
#' `RasterBrick` to remove occurrences from all layers.
#'
#' @return A `Raster*`
#'
#' @examples
#' # Create sample raster
#' r <- raster(ncol=10, nrow=10)
#' values(r) <- 1:100
#'
#' # Create test occurrences
#' set.seed(0)
#' longitude <- sample(extent(r)[1]:extent(r)[2],
#'                     size = 10, replace = FALSE)
#' set.seed(0)
#' latitude <- sample(extent(r)[3]:extent(r)[4],
#'                    size = 10, replace = FALSE)
#'
#' # Here's the function
#' result <- occCellRemoval(occs = occurrences, rasterTemplate = r)
#'
#' @import raster
#'
#' @keywords internal
#'
#' @noRd

occCellRemoval <- function(occs, rasterTemplate){
  # Handling alternative column names for occurrences
  colNames <- colnames(occs)
  xIndex <- grep(tolower(colNames), pattern = "long")
  yIndex <- grep(tolower(colNames), pattern = "lat")

  # Meat of function
  occCells <- cellFromXY(object = rasterTemplate, occs[,c(xIndex,yIndex)])
  rasterTemplate[occCells] <- NA
  return(rasterTemplate)
}

#' @title 2D Background Sampling
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
#' @details
#'
#' @return A `data.frame` with 2D coordinates of points
#' for background sampling.
#'
#' @examples
#' library(raster)
#' library(sp)
#'
#' # Create sample raster
#' r <- raster(ncol=10, nrow=10)
#' values(r) <- 1:100
#'
#' # Create test occurrences
#' set.seed(0)
#' longitude <- sample(extent(r)[1]:extent(r)[2],
#'                     size = 10, replace = FALSE)
#' set.seed(0)
#' latitude <- sample(extent(r)[3]:extent(r)[4],
#'                    size = 10, replace = FALSE)
#' occurrences <- as.data.frame(cbind(longitude,latitude))
#'
#' # Generate background sampling buffer
#' buffPts <- SpatialPoints(occurrences[,c("longitude", "latitude")])
#' crs(buffPts) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' mShp <- buffer(buffPts, width = 1, dissolve = TRUE)
#'
#' # Here's the function
#' result <- mSampling2D(occs = occurrences, rasterTemplate = r, mShp = mShp)
#'
#' @import raster
#'
#' @keywords backgroundSampling
#'
#' @export

mSampling2D <- function(occs, rasterTemplate, mShp){
  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(is.null(ncol(occs))){
    warning(paste0("'occs' must have at least two columns.\n"))
    return(NULL)
  }

  if(!grepl("Raster", class(rasterTemplate))){
    warning(paste0("'rasterTemplate' must be of class 'Raster*'.\n"))
    return(NULL)
  }

  if(class(mShp) != "SpatialPolygons"){
    warning(paste0("'mShp' must be of class 'SpatialPolygons'.\n"))
    return(NULL)
  }

  # Handling alternative column names for occurrences
  colNames <- colnames(occs)
  xIndex <- grep(tolower(colNames), pattern = "long")
  yIndex <- grep(tolower(colNames), pattern = "lat")
  if(length(xIndex) > 1){xIndex <- xIndex[[1]]}
  if(length(yIndex) > 1){yIndex <- yIndex[[1]]}
  if(length(xIndex) == 0){
    xIndex <- grep(tolower(colNames), pattern = "x")
    if(length(xIndex) > 1){xIndex <- xIndex[[1]]}
    if(length(xIndex) < 1){
      warning(message("Could not parse\n ",
                      paste0(colNames, collapse = ", "), "\n ",
                      "into x and/or y coordinates."))
      return(NULL)
    }
  }
  if(length(yIndex) == 0){
    yIndex <- grep(tolower(colNames), pattern = "y")
    if(length(yIndex) > 1){yIndex <- yIndex[[1]]}
    if(length(yIndex) < 1){
      warning(message("Could not parse\n ",
                      paste0(colNames, collapse = ", "), "\n ",
                      "into x and/or y coordinates."))
      return(NULL)
    }
  }

  message("Using ", colNames[xIndex], " and ", colNames[yIndex],
          "\n as x and y coordinates, respectively.")

  # Calculate lat/lon buffers and buffer
  rasterTemplate <- crop(mask(rasterTemplate, mask = mShp), y = mShp)
  # For each layer, toss cells where there are occurrences, then rasterToPoints for all cells in buffer
  rawLayer <- occCellRemoval(occs = occs[,c(xIndex,yIndex)], rasterTemplate)
  mPts <- data.frame(rasterToPoints(rawLayer)[,c("x", "y")])
  colnames(mPts) <- colNames[c(xIndex, yIndex)]
  return(mPts)
}

#' @title 3D Background Sampling
#'
#' @description Samples coordinates in 3D from maximum
#' to minimum occurrence depth by buffering occurrences
#' at XYZ resolution of envBrick
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
#' @details This function is designed to sample background points for
#' distributional modeling in three dimensions. If a voxel (3D pixel)
#' in the `envBrick` intersects with an occurrence from `occs`, it is
#' removed.
#'
#' `depthLimit` argument options:
#' \itemize{
#'   \item `occs` Samples background from the full depth extent of `occs`.
#'   \item `all` Samples background from the full depth extent of `envBrick`.
#'   \item A `vector` of length 2 with maximum and minimum depth values from which to sample.
#' }
#'
#' @return A `data.frame` with 3D coordinates of points for background
#' sampling.
#'
#' @examples
#' library(raster)
#'
#' # Create test raster
#' r1 <- raster(ncol=10, nrow=10)
#' values(r1) <- 1:100
#' r2 <- raster(ncol=10, nrow=10)
#' values(r2) <- c(rep(20, times = 50), rep(60, times = 50))
#' r3 <- raster(ncol=10, nrow=10)
#' values(r3) <- 8
#' envBrick <- brick(r1, r2, r3)
#' names(envBrick) <- c(0, 10, 30)
#'
#' # Create test occurrences
#' set.seed(0)
#' longitude <- sample(extent(envBrick)[1]:extent(envBrick)[2],
#'                     size = 10, replace = FALSE)
#' set.seed(0)
#' latitude <- sample(extent(envBrick)[3]:extent(envBrick)[4],
#'                    size = 10, replace = FALSE)
#' set.seed(0)
#' depth <- sample(0:35, size = 10, replace = TRUE)
#' occurrences <- as.data.frame(cbind(longitude,latitude,depth))
#'
#' # Generate background sampling buffer
#' buffPts <- SpatialPoints(occurrences[,c("longitude", "latitude")])
#' crs(buffPts) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' mShp <- buffer(buffPts, width = 1, dissolve = TRUE)
#'
#' # Test function
#' occSample3d <- mSampling3D(occurrences,
#'                            envBrick, mShp = mShp,
#'                            depthLimit = "occs")
#'
#' @import raster
#'
#' @keywords backgroundSampling
#'
#' @export

mSampling3D <- function(occs, envBrick, mShp, depthLimit = "all"){
  if(is.null(depthLimit)){depthLimit <- "all"}

  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(ncol(occs) < 3){
    warning(paste0("'occs' must have at least three columns.\n"))
    return(NULL)
  }

  if(is.null(ncol(occs))){
    warning(paste0("'occs' must have at least three columns.\n"))
    return(NULL)
  }

  if(class(envBrick) != "RasterBrick"){
    warning(paste0("'envBrick' must be of class 'RasterBrick'.\n"))
    return(NULL)
  }

  if(!(class(depthLimit) %in% c("character","numeric"))){
    warning(paste0("'depthLimit' must be of class 'character' or 'numeric'.\n"))
    return(NULL)
  }

  if(class(depthLimit) == "numeric"){
    if(length(depthLimit) != 2){
      warning(paste0("'depthLimit' arguments of 'numeric' must be of length 2.\n"))
      return(NULL)
    }
  }

  if(class(depthLimit) == "character"){
    if(length(depthLimit) > 1){
      warning(paste0(depthLimit, " is not a valid value for 'depthLimit'\n"))
      return(NULL)
    } else if(!(depthLimit %in% c("all", "occs"))){
      warning(paste0(depthLimit, " is not a valid value for 'depthLimit'\n"))
      return(NULL)
    }
  }

  # Handling alternative column names for occurrences
  colNames <- colnames(occs)
  xIndex <- grep(tolower(colNames), pattern = "long")
  yIndex <- grep(tolower(colNames), pattern = "lat")
  zIndex <- grep(tolower(colNames), pattern = "depth")
  if(length(xIndex) > 1){xIndex <- xIndex[[1]]}
  if(length(yIndex) > 1){yIndex <- yIndex[[1]]}
  if(length(zIndex) > 1){zIndex <- zIndex[[1]]}
  if(length(xIndex) == 0){
    xIndex <- grep(tolower(colNames), pattern = "x")
    if(length(xIndex) > 1){xIndex <- xIndex[[1]]}
    if(length(xIndex) < 1){
      warning(message("Could not parse\n ",
                      paste0(colNames, collapse = ", "), "\n ",
                      "into x, y, and/or z coordinates."))
      return(NULL)
    }
  }
  if(length(yIndex) == 0){
    yIndex <- grep(tolower(colNames), pattern = "y")
    if(length(yIndex) > 1){yIndex <- yIndex[[1]]}
    if(length(yIndex) < 1){
      warning(message("Could not parse\n ",
                      paste0(colNames, collapse = ", "), "\n ",
                      "into x, y, and/or z coordinates."))
      return(NULL)
    }
  }
  if(length(zIndex) == 0){
    zIndex <- grep(tolower(colNames), pattern = "z")
    if(length(zIndex) > 1){zIndex <- zIndex[[1]]}
    if(length(zIndex) < 1){
      warning(message("Could not parse\n ",
                      paste0(colNames, collapse = ", "), "\n ",
                      "into x, y, and/or z coordinates."))
      return(NULL)
    }
  }
  message("Using ", colNames[xIndex], ", ",
          colNames[yIndex], ", and ", colNames[zIndex],
          "\n as x, y, and z coordinates, respectively.")

  # Checking for appropriate environmental layer names
  layerNames <- tryCatch(expr = as.numeric(gsub("[X]", "", names(envBrick))),
                         warning = function(w){
                           message(w)
                           message("\nInput RasterBrick names inappropriate: \n",
                                   paste(names(envBrick), collapse = ", "), "\n",
                                   "Names must follow the format 'X' ",
                                   "followed by a number corresponding to ",
                                   "the starting depth of the layer.")
                           return(NULL)
                         })

  # Depth slice indices for occurrences
  occs$index <- unlist(lapply(occs[,zIndex],
                              FUN = function(x) which.min(abs(layerNames - x))))

  # Get depth range
  layerNames <- as.numeric(gsub("[X]", "", names(envBrick)))

  if(class(depthLimit) == "numeric"){
    depthRange <- c(which.min(abs(layerNames - min(depthLimit))),
                    which.min(abs(layerNames - max(depthLimit))))
  } else if(depthLimit == "occs"){
    depthRange <- c(min(occs$index), max(occs$index))
  } else {
    depthRange <- c(1, nlayers(envBrick))
  }

  # Calculate lat/long buffers and buffer
  rasterTemplate <- envBrick[[1]]
  envBrick <- crop(mask(envBrick[[depthRange[[1]]:depthRange[[2]]]], mask = mShp), y = mShp)
  # For each layer, toss cells where there are occurrences, then rasterToPoints for all cells in buffer
  mPts <- data.frame()
  for(i in 1:nlayers(envBrick)){
    rawLayer <- envBrick[[i]]
    layerDepth <- as.numeric(gsub("[X]", "", names(rawLayer)))
    occsAtLayerDepth <- occs[occs$index == match(layerDepth, layerNames),]
    if (nrow(occsAtLayerDepth) == 0){
      tempPoints <- data.frame(rasterToPoints(rawLayer)[,c("x", "y")])
      colnames(tempPoints) <- c(colNames[xIndex],colNames[yIndex])
      tempPoints[,zIndex] <- rep(layerDepth, times = nrow(tempPoints))
    } else {
      rawLayer <- occCellRemoval(occs = occsAtLayerDepth, rawLayer)
      tempPoints <- data.frame(rasterToPoints(rawLayer)[,c("x", "y")])
      colnames(tempPoints) <- c(colNames[xIndex],colNames[yIndex])
      tempPoints[,zIndex] <- rep(layerDepth, times = nrow(tempPoints))
    }
    mPts <- rbind(mPts, tempPoints)
  }
  colnames(mPts)[[3]] <- colNames[[zIndex]]
  return(mPts)
}
