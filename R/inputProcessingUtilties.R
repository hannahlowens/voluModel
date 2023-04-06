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
#' @param rasterTemplate A `SpatRaster` object to serve
#' as a template for the resolution at which `occs` should be
#' downsampled.
#'
#' @param verbose `logical`. Switching to `FALSE` mutes message describing
#' which columns in `occs` are interpreted as x and y coordinates.
#'
#' @return A `data.frame` with two columns named "longitude"
#' and "latitude" or with names that were used when coercing
#' input data into this format.
#'
#' @examples
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
#' occurrences <- as.data.frame(cbind(longitude,latitude))
#'
#' # Here's the function
#' result <- downsample(occs = occurrences, rasterTemplate = r)
#'
#' @import terra
#' @importFrom stats complete.cases
#'
#' @keywords inputProcessing
#' @export

downsample <- function(occs, rasterTemplate, verbose = TRUE){
  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(!grepl("SpatRaster", class(rasterTemplate))){
    warning(paste0("'rasterTemplate' must be of class 'SpatRaster'.\n"))
    return(NULL)
  }

  if (!is.logical(verbose)) {
    warning(message("Argument 'verbose' is not of type 'logical'.\n"))
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
#' `SpatVector` data frame and generates a `SpatRaster`.
#'
#' @param rawPointData A `SpatVector` object from which
#' bottom variables will be sampled. See Details for more about format.
#'
#' @return A `SpatRaster` designed to approximate sea bottom
#' measurements for modeling species' distributions and/or niches.
#'
#' @details `rawPointData` is a `SpatVector` object that
#' contains measurements of a single environmental variable (e.g.
#' salinity, temperature, etc.) with x, y, and z coordinates. The
#' measurements in the `data.frame` should be organized so that each
#' column is a depth slice, increasing in depth from left to right. The
#' function was designed around the oceanographic data shapefiles supplied
#' by the World Ocean Atlas
#' (\url{https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/}).
#' The function selects the "deepest" (rightmost) measurement at each
#' x, y coordinate pair that contains data. These measurements are then
#' rasterized at the resolution and extent of the x,y coordinates, under
#' the assumption that x and y intervals are equal and represent the center
#' of a cell.
#'
#' @examples
#'
#' library(terra)
#'
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
#' sp <- vect(dd, geom = c("x", "y"))
#'
#' # Here's the function
#' result <- bottomRaster(rawPointData = sp)
#' plot(result)
#'
#' @import terra
#'
#' @keywords inputProcessing
#' @export

# Samples bottom values from raster bricks
bottomRaster <- function(rawPointData){
  if(!is(rawPointData, "SpatVector")){
    warning(paste0("'rawPointData' must be class 'SpatVector'.\n"))
    return(NULL)
  }

  rpdf <- as.data.frame(rawPointData)

  template <- centerPointRasterTemplate(rawPointData)

  bottomSample <- apply(rpdf, MARGIN = 1,
                        FUN = function(x) tail(x[!is.na(x)],1))
  rawPointData$Bottom <- bottomSample

  bRaster <- terra::rasterize(x = rawPointData, y = template, field = "Bottom")
  return(bRaster)
}

#' @title Center Point Raster Template
#'
#' @description Samples deepest depth values from a
#' `SpatVector` point object and generates a `SpatRaster`.
#'
#' @param rawPointData A `SpatVector` object with points
#' that will represent the center of each cell in the output
#' template.
#'
#' @return An empty `SpatRaster` designed to serve as a template for
#' rasterizing `SpatVector` objects.
#'
#' @details `rawPointData` is a `SpatVector` object that
#' contains x and y coordinates.
#'
#' @seealso \code{\link[terra:rasterize]{rasterize}}
#'
#' @examples
#'
#' library(terra)
#'
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
#' sp <- vect(dd, geom = c("x", "y"))
#'
#' # Here's the function
#' template <- centerPointRaster(rawPointData = sp)
#' class(template)
#'
#' @import terra
#'
#' @keywords inputProcessing
#' @export
#'
centerPointRasterTemplate <- function(rawPointData){
  if(!is(rawPointData, "SpatVector")){
    warning(paste0("'rawPointData' must be class 'SpatVector'.\n"))
    return(NULL)
  }
  rpdGeom <- geom(rawPointData)

  centeringAdjustment <- min(abs(diff(unique(rpdGeom[,"x"]))), abs(diff(unique(rpdGeom[,"y"]))))/2
  oldExtent <- ext(rawPointData)
  newExtent <- ext(oldExtent[1] - centeringAdjustment,
                   oldExtent[2] + centeringAdjustment,
                   oldExtent[3] - centeringAdjustment,
                   oldExtent[4] + centeringAdjustment)

  template <- rast(nrows=length(unique(rpdGeom[,"y"])),
                   ncols=length(unique(rpdGeom[,"x"])), extent = newExtent)
  return(template)
}
