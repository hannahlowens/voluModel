#' @title Column Parsing
#'
#' @description Parses column names from input occurrence
#' `data.frame` for more seamless function
#'
#' @param occs A `data.frame` with at least two columns
#' named "longitude" and "latitude" or that
#' can be coerced into this format.
#'
#' @param wDepth Logical; flags whether a depth column should
#' also be sought.
#'
#' @details This is an internal function to return the putative
#' indices for latitude and longitude or x and y coordinates
#' of occurrences to allow for code that is more robust to
#' very common user error
#'
#' @return A `list` of length 2 with indices of the x and y
#' columns, respectively, followed by a message with a plain
#' text report of which columns were interpreted as x and y.
#'
#' @examples
#' library(raster)
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
#' set.seed(0)
#' depth <- sample(0:35, size = 10, replace = TRUE)
#' occurrences <- as.data.frame(cbind(longitude,latitude,depth))
#'
#' # Here's the function
#' result <- columnParse(occs = occurrences[,1:2],
#'                       wDepth = FALSE)
#' result <- columnParse(occs = occurrences,
#'                       wDepth = TRUE)
#'
#' @import raster
#'
#' @keywords internal
#'
#' @export

columnParse <- function(occs, wDepth = FALSE){
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

  # Depth parsing
  if(wDepth){
    zIndex <- grep(tolower(colNames), pattern = "depth")
    if(length(zIndex) > 1){zIndex <- zIndex[[1]]}
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
    reportMessage <- paste0("Using ", colNames[xIndex], ", ",
                            colNames[yIndex], ", and ", colNames[zIndex],
                            "\n as x, y, and z coordinates, respectively.")
    return(list(xIndex = xIndex, yIndex = yIndex, zIndex = zIndex,
                reportMessage = reportMessage))
  } else {
    reportMessage <- paste0("Using ", colNames[xIndex], " and ",
                            colNames[yIndex],
                            "\n as x and y coordinates, respectively.")
    return(list(xIndex = xIndex, yIndex = yIndex,
                reportMessage = reportMessage))
  }
}

#' @title Occurrence downsampling
#'
#' @description Reduces number of occurrences to resolution of input raster
#'
#' @param occs A `data.frame` with at least two columns
#' named "longitude" and "latitude" or that
#' can be coerced into this format.
#'
#' @param rasterTemplate A `Raster*` object to serve
#' as a template for the resolution at which `occs` should be
#' downsampled.
#'
#' @return A `data.frame` with two columns named "longitude"
#' and "latitude" or with names that were used when coercing
#' input data into this format.
#'
#' @examples
#' library(raster)
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
#' # Here's the function
#' result <- downsample(occs = occurrences, rasterTemplate = r)
#' head(result)
#'
#' @import raster
#'
#' @keywords inputProcessing
#' @export

downsample <- function(occs, rasterTemplate){
  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(!grepl("Raster", class(rasterTemplate))){
    warning(paste0("'rasterTemplate' must be of class 'Raster*'.\n"))
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

  message(interp)

  occCells <- cellFromXY(object = rasterTemplate, occs[,c(xIndex,yIndex)])
  occCells <- unique(occCells)
  occs <- xyFromCell(object = rasterTemplate, cell = occCells)
  occs <- occs[complete.cases(occs),]
  if(is.null(nrow(occs))){
    names(occs) <- colNames[c(xIndex,yIndex)]
    occs <- data.frame(t(occs))
  } else {
    colnames(occs) <- colNames[c(xIndex,yIndex)]
    occs <- data.frame(occs)
  }
  return(occs)
}

#' @title Bottom raster generation
#'
#' @description Samples deepest depth values from a
#' `SpatialPointsDataFrame` and generates a `RasterLayer`.
#'
#' @param rawPointData A `SpatialPointsDataFrame` object from which
#' bottom variables will be sampled. See Details for more about format.
#'
#' @return A `RasterLayer` designed to approximate sea bottom
#' measurements for modeling species' distributions and/or niches.
#'
#' @details `rawPointData` is a `SpatialPointsDataFrame` object that
#' contains measurements of a single environmental variable (e.g.
#' salinity, temperature, etc.) with x, y, and z coordinates. The
#' measurements in the `data.frame` should be organized so that each
#' column is a depth slice, increasing in depth from left to right. The
#' function was designed around the oceanographic data shapefiles supplied
#' by the World Ocean Atlas
#' (\url{https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/}).
#' The function selects the "deepest" (rightmost) measurement at each
#' x, y coordinate pair that contains data. These measurements are then
#' rasterized at the resolution and extent of the x,y coordinates.
#'
#' @examples
#' # Create point grid
#' coords <- data.frame(x = rep(seq(1:5), times = 5),
#'                     y = unlist(lapply(1:5, FUN = function(x) {
#'                       rep(x, times = 5)})))
#'
#' # Create data and add NAs to simulate uneven bottom depths
#' dd <- data.frame(SURFACE = 1:25,
#'                 d5M = 6:30,
#'                 d10M = 11:35,
#'                 d25M = 16:40)
#' dd$d25M[c(1:5, 18:25)] <- NA
#' dd$d10M[c(3:5, 21:23)] <- NA
#' dd$d5M[c(4, 22)] <- NA
#'
#' # Create SpatialPointsDataFrame
#' sp <- sp::SpatialPointsDataFrame(coords = coords,
#'                             data = dd)
#'
#' # Here's the function
#' result <- bottomRaster(rawPointData = sp)
#' plot(result)
#'
#' @import raster
#' @importFrom stats complete.cases
#'
#' @keywords inputProcessing
#' @export

# Samples bottom values from raster bricks
bottomRaster <- function(rawPointData){
  if(class(rawPointData) != "SpatialPointsDataFrame"){
    warning(paste0("'rawPointData' must be class 'SpatialPointsDataFrame'.\n"))
    return(NULL)
  }

  bottomSample <- apply(rawPointData@data, MARGIN = 1,
                        FUN = function(x) tail(x[!is.na(x)],1))
  rawPointData@data$Bottom <- bottomSample
  bRaster <- rasterFromXYZ(cbind(rawPointData@coords,rawPointData@data$Bottom))
  return(bRaster)
}
