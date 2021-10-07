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
#' result <- marineBackground(occs = occurrences)
#'
#' @import raster
#' @import rangeBuilder
#'
#' @seealso \code{\link[rangeBuilder:getDynamicAlphaHull]{getDynamicAlphaHull}}
#'
#' @keywords backgroundSampling


marineBackground <- function(occs, ...){
  args <- list(...)

  if("fraction" %in% names(args)){
    fraction <- args$fraction
  } else {
    fraction = 0.95
  }

  if("partCount" %in% names(args)){
    partCount <- args$partCount
  } else {
    partCount <- 3
  }

  if("buff" %in% names(args)){
    buff <- args$buff
  } else{
    buff <- 10000
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


  if (!is.numeric(fraction)) {
    warning(message("Argument 'fraction' is not of class 'numeric'.\n"))
    return(NULL)
  }
  if (!is.numeric(partCount)) {
    warning(message("Argument 'partCount' is not of type 'numeric'.\n"))
    return(NULL)
  }
  if (!is.numeric(buff)) {
    warning(message("Argument 'buff' is not of type 'numeric'.\n"))
    return(NULL)
  }
  if (!is.numeric(initialAlpha)) {
    warning(message("Argument 'initialAlpha' is not of type 'numeric'.\n"))
    return(NULL)
  }
  if (!(clipToCoast %in% c("no", "terrestrial", "aquatic"))) {
    warning(message(clipToCoast, " is not a valid selection for 'clipToCoast'.\n"))
    return(NULL)
  }
  if (!is.numeric(alphaIncrement)) {
    warning(message("Argument 'alphaIncrement' is not of type 'numeric'.\n"))
    return(NULL)
  }

  # Parse columns
  colNames <- colnames(occs)
  colParse <- voluModel:::columnParse(occs)
  if(is.null(colParse)){
    return(NULL)
  }
  xIndex <- colParse$xIndex
  yIndex <- colParse$yIndex
  interp <- colParse$reportMessage

  message(interp)


  # Calculate buffer
  pDist <- pointDistance(occs[,c(xIndex, yIndex)], lonlat = T)
  buff <- mean(quantile(pDist, c(.1, .9), na.rm = T))/2

  # Hull part
  hull <- try(getDynamicAlphaHull(occs, coordHeaders=c(xIndex,yIndex),
                                  clipToCoast = "no", fraction = .99, partCount = 1), silent = T)
  if("try-error" %in% class(hull)){
    x1 <- min(occs[xIndex])
    x2 <- max(occs[xIndex])
    y1 <- min(occs[yIndex])
    y2 <- max(occs[yIndex])
    hull <- Polygon(cbind(c(x1,x1,x2,x2,x1),c(y1,y2,y2,y1,y1)))
    hull <- Polygons(list(hull), ID = "A")
    hull <- SpatialPolygons(list(hull))
    proj4string(hull) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    hull <- spTransform(hull, CRSobj = CRS("+proj=eqc +lon_0=0 +datum=WGS84 +units=m +no_defs"))
  } else{
    hull <- spTransform(hull[[1]], CRSobj = CRS("+proj=eqc +lon_0=0 +datum=WGS84 +units=m +no_defs"))
  }

  # Project to Plate Carre
  hullBuff <- buffer(hull, width = buff, dissolve = T)

  # Point part
  occsForM <- suppressWarnings(SpatialPoints(occs[,c(xIndex, yIndex)],
                                             proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
  test <- intersect(occsForM, pacificShapefile) # Collect points that occur in the East Pacific for later...
  occsForM <- spTransform(occsForM, CRSobj = CRS("+proj=eqc +lon_0=0 +datum=WGS84 +units=m +no_defs"))
  occBuff <- suppressWarnings(buffer(occsForM, width = buff, dissolve = T))

  # Putting it all together and fixing the date line
  wholeM <- gUnion(occBuff, hullBuff)
  wholeM <- crop(wholeM, extent(c(xmin(wholeM),
                                  xmax(wholeM),
                                  ymin(worldExtent),
                                  ymax(worldExtent)))) # Gets rid of slop at poles
  middle <- gIntersection(wholeM, worldExtent)
  ends <- gDifference(wholeM, worldExtent)
  if(!is.null(ends)){
    ends <- disaggregate(ends)
    if(length(ends) == 1){
      if(xmin(ends) < -20037508){
        ends <- raster::shift(ends, dx = 20037508*2)
      }
      if(xmax(ends) > 20037508){
        ends <- raster::shift(ends, dx = -20037508*2)
      }
    } else{
      result <- list()
      for(j in 1:length(ends)){
        if(xmin(ends[j,]) < -20037508){
          endTemp <- raster::shift(ends[j,], dx = 20037508*2)
          slot(endTemp@polygons[[1]], "ID") <- paste0(j)
        } else if(xmax(ends[j,]) > 20037508){
          endTemp <- raster::shift(ends[j,], dx = -20037508*2)
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

  wholeM <- spTransform(wholeM, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

  # Remove East Pacific from training region in case of western Caribbean leakage
  if(nrow(test@coords) < 1){
    wholeM <- gDifference(wholeM, pacificShapefile)
  }

  envBuffers[[i]] <- wholeM

  return(wholeM)
}
