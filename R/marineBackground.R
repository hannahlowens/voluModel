#' @title Marine background shapefile generation
#'
#' @description Automatically generates background
#' shapefiles for sampling pseudoabsences and/or background
#' points for niche modeling or species distribution modeling.
#' Delineating background sampling regions can be one of the
#' trickiest parts of generating a good model. Automatically
#' generated background shapefiles should be inspected
#' carefully prior to model use.
#'
#' Useful references, among others:
#' \itemize{
#'   \item Barve N, Barve V, Jiménez-Valverde A, Lira-Noriega A,
#'   Maher SP, Peterson AT, Soberón J, Villalobos F. 2011. The
#'   crucial role of the accessible area in ecological niche
#'   modeling and species distribution modeling.
#'   \emph{Ecological modelling} 222:1810-9.
#'   \item Merow, C, Smith MJ, Silander JA. 2013. A practical
#'   guide to MaxEnt for modeling species’ distributions: what
#'   it does, and why inputs and settings matter." \emph{Ecography}
#'   36: 1058-69.
#'   \item Murphy SJ. 2021. Sampling units derived from geopolitical
#'   boundaries bias biodiversity analyses. \emph{Global Ecology
#'   and Biogeography} 30: 1876-88.
#' }
#'
#' @param occs A `data.frame` with at least two columns
#' named "longitude" and "latitude" or that
#' can be coerced into this format.
#'
#' @param clipToOcean `logical`. Clips shapefile to oceans where species
#' occurs. Useful in cases where buffers jump over narrow
#' peninsulas (e.g. Isthmus of Panama). Can be quite artificial at ocean
#' boundaries.
#'
#' @param ... Additional optional arguments to pass to
#' `getDynamicAlphaHull`.
#'
#' @return A `data.frame` with two columns named "longitude"
#' and "latitude" or with names that were used when coercing
#' input data into this format.
#'
#' @details The meat of this function is a special-case wrapper
#' around `getDynamicAlphaHull` from the `rangeBuilder` package.
#' The function documented here is especially useful in cases where
#' one wants to automatically generate training regions that overlap
#' the international date line. Regions that exceed the line are cut
#' and pasted into the appropriate hemisphere instead of being
#' deleted.
#'
#' If the argument `buff` is not supplied, a buffer is
#' calculated by taking the mean between the 10th and 90th percentile
#' of horizontal distances between occurrence points.
#'
#' @examples
#' library(raster)
#' # Create sample raster
#' r <- raster(ncol=10, nrow=10)
#' values(r) <- 1:100
#'
#' # Create test occurrences
#' set.seed(0)
#' longitude <- sample(-50:50,
#'                     size = 20, replace = FALSE)
#' set.seed(0)
#' latitude <- sample(-30:30,
#'                    size = 20, replace = FALSE)
#' occurrences <- as.data.frame(cbind(longitude,latitude))
#'
#' # Here's the function
#' result <- marineBackground(occs = occurrences, buff = 100000,
#'                            fraction = .9, partCount = 2, clipToOcean = TRUE)
#' plot(result)
#' points(occurrences, pch = 20)
#'
#' @import raster
#' @import rangeBuilder
#' @import rgeos
#' @import sp
#' @importFrom terra distance buffer shift
#' @importFrom methods as slot<-
#'
#' @seealso \code{\link[rangeBuilder:getDynamicAlphaHull]{getDynamicAlphaHull}}
#'
#' @keywords backgroundSampling
#'
#' @export


marineBackground <- function(occs, clipToOcean = TRUE, ...){
  args <- list(...)

  if("fraction" %in% names(args)){
    fraction <- args$fraction
  } else {
    fraction <- 0.95
  }

  if("partCount" %in% names(args)){
    partCount <- args$partCount
  } else {
    partCount <- 1
  }

  if("initialAlpha" %in% names(args)){
    initialAlpha <- args$initialAlpha
  } else{
    initialAlpha <- 3
  }

  if("clipToCoast" %in% names(args)){
    clipToCoast <- args$clipToCoast
  } else {
    clipToCoast <- "no"
  }

  if("alphaIncrement" %in% names(args)){
    alphaIncrement <- args$alphaIncrement
  } else{
    alphaIncrement <- 1
  }

  # Input checking
  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }
  if (!is.logical(clipToOcean)) {
    warning(message("Argument 'clipToOcean' is not of type 'logical'.\n"))
    return(NULL)
  }
  if (!is.numeric(fraction)) {
    warning(message("Argument 'fraction' is not of class 'numeric'.\n"))
    return(NULL)
  }
  if (!is.numeric(partCount)) {
    warning(message("Argument 'partCount' is not of type 'numeric'.\n"))
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

  # Calculate buffer
  if("buff" %in% names(args)){
    buff <- args$buff
    if (!is.numeric(buff)) {
      warning(message("Argument 'buff' is not of type 'numeric'.\n"))
      return(NULL)
    }
  } else{
    pDist <- terra::distance(as.matrix(occs[,c(xIndex, yIndex)]), lonlat = TRUE)
    buff <- mean(quantile(pDist, c(.1, .9), na.rm = TRUE))/2
  }

  if (!is.numeric(initialAlpha)) {
    warning(message("Argument 'initialAlpha' is not of type 'numeric'.\n"))
    return(NULL)
  }
  if (!(clipToCoast %in% c("no", "terrestrial", "aquatic"))) {
    warning(message(clipToCoast, " is not valid for 'clipToCoast'.\n"))
    return(NULL)
  }
  if (!is.numeric(alphaIncrement)) {
    warning(message("Argument 'alphaIncrement' is not of type 'numeric'.\n"))
    return(NULL)
  }

  # Hull part
  hull <- try(rangeBuilder::getDynamicAlphaHull(occs,
                                                initialAlpha = initialAlpha,
                                                alphaIncrement = alphaIncrement,
                                                coordHeaders=colnames(occs)[c(xIndex,
                                                                              yIndex)],
                                                clipToCoast = clipToCoast,
                                                fraction = fraction,
                                                partCount = partCount, buff = 0),
              silent = TRUE)
  if("try-error" %in% class(hull)){
    x1 <- min(occs[xIndex])
    x2 <- max(occs[xIndex])
    y1 <- min(occs[yIndex])
    y2 <- max(occs[yIndex])
    hull <- Polygon(cbind(c(x1,x1,x2,x2,x1),c(y1,y2,y2,y1,y1)))
    hull <- Polygons(list(hull), ID = "A")
    hull <- SpatialPolygons(list(hull))
    proj4string(hull) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    hull <- sp::spTransform(hull[[1]],
                            CRSobj = sp::CRS("+proj=eqc +lon_0=0 +datum=WGS84 +units=m +no_defs"))
  } else{
    hull <- sp::spTransform(hull[[1]],
                            CRSobj = sp::CRS("+proj=eqc +lon_0=0 +datum=WGS84 +units=m +no_defs"))
  }

  hullBuff <- terra::buffer(hull, width = buff, dissolve = TRUE)

  # Point part
  occsForM <- suppressWarnings(sp::SpatialPoints(occs[,c(xIndex, yIndex)],
                                                 proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
  occsForM <- sp::spTransform(occsForM,
                              CRSobj = sp::CRS("+proj=eqc +lon_0=0 +datum=WGS84 +units=m +no_defs"))
  occBuff <- suppressWarnings(terra::buffer(occsForM,
                                            width = buff, dissolve = TRUE))
  wholeM <- rgeos::gUnion(occBuff, hullBuff)

  # Crop out land
  land <- readRDS(system.file("extdata/smallLand.rds",
                              package='voluModel'))
  wholeM <- try(rgeos::gDifference(wholeM,land), silent = T)

  # Optional removal of unoccupied polygons
  if(clipToOcean){
    # First, split up disjunct polygons
    wholeM <- sp::disaggregate(wholeM)
    wholeM <- wholeM[apply(rgeos::gContains(wholeM, occsForM, byid = TRUE),
                           2, FUN = any)]
  }

  # Putting it all together and fixing the date line
  worldExtent <- extent(-20037508,
                        20037508,
                        -10018754,
                        10018754) # Plate-Carre world extent
  worldExtent <- as(worldExtent, 'SpatialPolygons')
  crs(worldExtent) <- sp::CRS("+proj=eqc +lon_0=0 +datum=WGS84 +units=m +no_defs")

  # Get rid of slop at poles
  wholeM <- crop(wholeM, extent(c(xmin(wholeM),
                                  xmax(wholeM),
                                  ymin(worldExtent),
                                  ymax(worldExtent))))
  # Wrap shapefiles at 180th meridian
  middle <- gIntersection(wholeM, worldExtent)
  ends <- gDifference(wholeM, worldExtent)
  if(!is.null(ends)){
    ends <- disaggregate(ends)
    if(length(ends) == 1){
      if(xmin(ends) < -20037508){
        ends <- terra::shift(ends, dx = 20037508*2)
      }
      if(xmax(ends) > 20037508){
        ends <- terra::shift(ends, dx = -20037508*2)
      }
    } else{
      result <- list()
      for(j in 1:length(ends)){
        if(xmin(ends[j,]) < -20037508){
          endTemp <- terra::shift(ends[j,], dx = 20037508*2)
          slot(endTemp@polygons[[1]], "ID") <- paste0(j)
        } else if(xmax(ends[j,]) > 20037508){
          endTemp <- terra::shift(ends[j,], dx = -20037508*2)
          slot(endTemp@polygons[[1]], "ID") <- paste0(j)
        }
        result[[j]] <- endTemp
      }
      ends <- do.call(rbind, result)
    }
    wholeM <- gUnion(middle, ends)
  } else{
    wholeM <- middle
  }

  wholeM <- spTransform(wholeM,
                        CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

  return(wholeM)
}
