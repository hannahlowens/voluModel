#' @title Sampling from rasterBrick using 3D coordinates
#'
#' @description Gets values at x,y,z occurrences from a
#' given 3D environmental variable brick
#'
#' @param occs A dataframe with at least three columns
#' named "longitude", "latitude", and "depth", or that
#' can be coerced into this format.
#'
#' @param envBrick A `rasterBrick` object with
#' one environmental variable. Each layer represents
#' a depth slice. See Details for more information.
#'
#' @details The `envBrick` `rasterBrick` object should
#' have numeric names that correspond with the beginning
#' depth of a particular depth slice. For example, one
#' might have three layers, one from 0 to 10m, one from
#' 10 to 30m, and one from 30 to 100m. You would name the
#' layers in this brick `names(envBrick) <- c(0, 10, 30`.
#' R will rename the layers "X0", "X10", and "X30". This
#' is expected behavior and `xyzSample` was written to
#' expect this. `xyzSample` identifies the layer name
#' that is closest to the depth layer value at a
#' particular X, Y coordinate, and samples the
#' environmental value at that 3D coordinate.
#'
#' @return Vector of environmental values equal in length
#' to number of rows of input `occs` `data.frame`.
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
#' # Test function
#' occSample3d <- xyzSample(occurrences, envBrick)
#'
#' # How to use
#' occurrences$envtValue <- occSample3d
#'
#' head(occurrences)
#'
#'
#' @import raster
#'
#' @keywords dataPrep
#'
#' @export

xyzSample <- function(occs, envBrick){
  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(ncol(occs) < 3){
    warning(paste0("'occs' must have at least three columns.\n"))
    return(NULL)
  }

  if(class(envBrick) != "RasterBrick"){
    warning(paste0("'envBrick' must be of class 'RasterBrick'.\n"))
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

  message(interp)

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

  # Sampling values
  sampledValues <- NULL
  index <- unlist(lapply(occs[,zIndex], FUN = function(x) which.min(abs(layerNames - x))))
  occs$index <- index
  occs$sampledValues <- rep(NA, times = nrow(occs))
  indices <- unique(occs$index)
  for(i in indices){
    occs[occs$index == i,]$sampledValues <- raster::extract(x = envBrick[[i]],
                                                            y = occs[occs$index == i,c(xIndex,
                                                                                       yIndex)])
  }
  return(occs$sampledValues)
}
