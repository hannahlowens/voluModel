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
#' @param verbose `logical`. Switching to `FALSE` mutes message describing
#' which columns in `occs` are interpreted as x and y coordinates.
#'
#' @param ... Additional optional arguments to pass to
#' `getDynamicAlphaHull`.
#'
#' @return A `SpatVector`
#'
#' @details The meat of this function is a special-case wrapper
#' around `getDynamicAlphaHull()` from the `rangeBuilder` package.
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
#' If `getDynamicAlphaHull()` cannot satisfy the provided conditions,
#' the occurrences are buffered and then a minimum convex hull is
#' drawn around the buffer polygons.
#'
#' @examples
#' \donttest{
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
#'                            fraction = .9, partCount = 2, clipToOcean = FALSE)
#' }
#'
#' @import terra
#' @import sf
#' @importFrom rangeBuilder getDynamicAlphaHull
#' @importFrom methods as slot<-
#'
#' @seealso \code{\link[rangeBuilder:getDynamicAlphaHull]{getDynamicAlphaHull}}
#'
#' @keywords backgroundSampling
#'
#' @export


marineBackground <- function(occs, clipToOcean = TRUE, verbose = TRUE, ...){
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
  if (!is.logical(verbose)) {
    warning(message("Argument 'verbose' is not of type 'logical'.\n"))
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

  if(verbose){
    message(interp)
  }

  # Calculate buffer
  if("buff" %in% names(args)){
    buff <- args$buff
    if (!is.numeric(buff)) {
      warning(message("Argument 'buff' is not of type 'numeric'.\n"))
      return(NULL)
    }
  } else{
    pDist <- distance(as.matrix(occs[,c(xIndex, yIndex)]), lonlat = TRUE)
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

  # Point part
  upj <- st_crs(4326) # Unprojected WGS84
  pj <- st_crs(4087) # Projected WGS84
  occsForM <- vect(x = occs[,c(xIndex, yIndex)],
                   geom = c(colNames[xIndex], colNames[yIndex]),
                   crs = upj$wkt)
  occsForM <- project(occsForM, y = pj$wkt)
  occBuff <- suppressWarnings(buffer(occsForM,
                                     width = buff))
  occBuff <- aggregate(occBuff)

  # Hull part
  hull <- try(getDynamicAlphaHull(occs, initialAlpha = initialAlpha,
                                  alphaIncrement = alphaIncrement,
                                  coordHeaders=colnames(occs)[c(xIndex,
                                                                yIndex)],
                                  clipToCoast = clipToCoast,
                                  fraction = fraction,
                                  partCount = partCount),
              silent = TRUE)
  gdahAlternative <- FALSE
  if("try-error" %in% class(hull)){
    gdahAlternative <- TRUE
  } else{
    if(hull$alpha == "alphaMCH"){
      gdahAlternative <- TRUE
    }
    hull <- st_transform(hull[[1]], crs = pj$wkt)
    if(xmin(ext(vect(hull))) < -20000000 ||
       xmax(ext(vect(hull))) > 20000000){
      gdahAlternative <- TRUE
    }
  }

  if(gdahAlternative){
    occBuffTmp <- disagg(occBuff)
    occBuffConv <- vector(mode = "list", length = length(occBuffTmp))
    for (i in 1:length(occBuffTmp)){
      occBuffConv[[i]] <- convHull(occBuffTmp[i])
      values(occBuffConv[[i]]) <- 1
    }
    wholeM <- aggregate(vect(occBuffConv))
  } else{
    hullBuff <- buffer(vect(hull), width = buff)
    hullBuff <- aggregate(hullBuff)
    wholeM <- union(occBuff, hullBuff)
    wholeM <- aggregate(wholeM)
  }

  # Crop out land
  land <- readRDS(system.file("extdata/smallLand.rds",
                              package='voluModel'))
  wholeM <- erase(wholeM, vect(land))

  # Optional removal of unoccupied polygons
  if(clipToOcean){
    # First, split up disjunct polygons
    wholeM <- disagg(wholeM)
    polysContainingPoints <- apply(relate(wholeM, occsForM, "contains"),
                                   MARGIN = 1, FUN = function(x) any(x))
    wholeM <- wholeM[polysContainingPoints]
  }

  # Putting it all together and fixing the date line
  worldExtent <- ext(-20037508,
                     20037508,
                     -10018754,
                     10018754) # Plate-Carre world extent

  # Get rid of slop at poles
  wholeM <- crop(wholeM, ext(c(xmin(wholeM),
                               xmax(wholeM),
                               ymin(worldExtent),
                               ymax(worldExtent))))
  # Wrap shapefiles at 180th meridian
  middle <- intersect(wholeM, worldExtent)
  ends <- erase(wholeM, worldExtent)
  if(length(ends) > 0){
    if(length(ends) == 1){
      if(xmin(ends) < -20037508){
        ends <- shift(ends, dx = 20037508*2)
      }
      if(xmax(ends) > 20037508){
        ends <- shift(ends, dx = -20037508*2)
      }
    } else{
      result <- list()
      for(j in 1:length(ends)){
        if(xmin(ends[j,]) < -20037508){
          endTemp <- shift(ends[j,], dx = 20037508*2)
        } else if(xmax(ends[j,]) > 20037508){
          endTemp <- shift(ends[j,], dx = -20037508*2)
        }
        result[[j]] <- endTemp
      }
      ends <- vect(unlist(result))
      ends <- aggregate(ends)
    }
    wholeM <- aggregate(union(middle, ends))
  } else{
    wholeM <- middle
  }

  wholeM <- aggregate(wholeM)
  wholeM <- project(wholeM, y = pj$wkt)
  wholeM <- project(wholeM, y = upj$wkt)

  return(wholeM)
}
