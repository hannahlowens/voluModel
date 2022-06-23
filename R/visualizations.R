# Visualization ----
#' @title Are Colors
#'
#' @description Checks to see if a given vector can be
#' interpreted by R as a color or colors
#'
#' @param col A vector of anything to be interpreted by `rgb`
#' as a color.
#'
#' @return A logical vector stating whether inputs
#' can be interpreted as colors.
#'
#' @examples
#'
#' areColors(col = c("red", "prairie_chicken", 2))
#'
#' @importFrom grDevices col2rgb
#'
#' @keywords internal plotting
#'
#' @export

areColors <- function(col) {
  if(is.null(col)){
    warning(paste0("'col' cannot be NULL.\n"))
    return(NULL)
  } else{
    result <- sapply(col, function(X) {
      tryCatch(is.matrix(col2rgb(X)),
               error = function(e) FALSE)
    })
    return(result)
  }
}

#' @title Test Intersection
#'
#' @description Tests whether two rasters overlap. Used in
#' `\code{\link[voluModel:diversityStack]{diversityStack}}`
#' function to verify all rasters in list overlap with the
#' template raster.
#'
#' @param a The first `Raster*` object
#'
#' @param b The second `Raster*` object
#'
#' @return A logical vector stating whether the two
#' inputs overlap
#'
#' @examples
#'
#' library(raster)
#' rast1 <- raster(ncol=10, nrow=10)
#' values(rast1) <- rep(0:1, 50)
#'
#' rast2 <- raster(ncol=10, nrow=10)
#' values(rast2) <- c(rep(0, 50), rep(1,50))
#'
#' testIntersection(rast1, rast2)
#'
#' rast1 <- crop(rast1, extent(10, 20, 30, 40))
#' rast2 <- crop(rast2, extent(-20, -10, -40, -30))
#'
#' testIntersection(rast1, rast2)
#'
#' @importFrom grDevices col2rgb
#'
#' @keywords internal plotting
#'
#' @export

testIntersection <- function(a,b){
  #reads in two rasters and tests for overlap T or F
  # if returns TRUE then there is overlap
  # try error is included b/c errors has come up with other test data
  !(inherits(try(intersect(a,b),T ), what ='try-error') || is.null(intersect(a,b)))
}

#' @title Point mapping
#'
#' @description A convenient wrapper around ggplot
#' to generate formatted occurrence point plots.
#'
#' @param occs A `data.frame` with at least two columns
#' named "longitude" and "latitude" or that
#' can be coerced into this format.
#'
#' @param spName A character string with the species
#' name to be used in the plot title.
#'
#' @param land An optional coastline polygon shapefile
#' of type `sf` to provide geographic context for the
#' occurrence points.
#'
#' @param ptCol Color for occurrence points on map
#'
#' @param landCol Color for land on map
#'
#' @param waterCol Color for water on map
#'
#' @param ptSize `numeric` value for `cex`;
#' size of occurrence points on map.
#'
#' @param ... Additional optional arguments to pass to
#' `ggplot` initial plot object.
#'
#' @return A `ggplot` plot object.
#'
#' @examples
#' occs <- read.csv(system.file("extdata/Steindachneria_argentea.csv",
#'                              package='voluModel'))
#' spName <- "Steindachneria argentea"
#' pointMap(occs = occs, spName = spName,
#'          land = rnaturalearth::ne_countries(scale = "small",
#'                                             returnclass = "sf")[1])
#'
#' @import ggplot2
#'
#' @seealso \code{\link[ggplot2:ggplot]{ggplot}}
#'
#' @keywords plotting
#' @export

pointMap <- function(occs, spName, land = NA,
                     ptCol = "#bd0026", landCol = "gray",
                     waterCol = "steelblue", ptSize = 1,
                     ...){
  args <- list(...)

  if("mapping" %in% names(args)){
    mapping <- args$mapping
  } else{
    mapping <- aes()
  }

  if("alpha" %in% names(args)){
    alpha <- args$alpha
  } else{
    alpha <- 2/3
  }

  # Input checking
  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(!is.character(spName)){
    warning(paste0("'spName' must be an object of class 'character'.\n"))
    return(NULL)
  }

  if(!any(is.na(land[[1]]), "sf" %in% class(land))){
    warning(paste0("'land' must either be NA or of class 'sf'."))
    return(NULL)
  }

  colVec <- c(ptCol, landCol, waterCol)
  colTest <- areColors(colVec)

  if(!any(c(all(colTest), length(colTest) < 3))){
    warning(paste0("'ptCol', 'landCol', and 'waterCol' must
                   be recognized colors.\n"))
    return(NULL)
  }

  if(!is.numeric(ptSize)){
    warning(paste0("'ptSize' must be numeric.\n"))
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

  # Where the actual function happens
  if(any(is.na(land))){
      point_map <- ggplot(mapping = mapping) +
        geom_point(data = occs, aes(x = occs[[xIndex]], y = occs[[yIndex]]),
                   colour = ptCol, cex = ptSize, shape = 20, alpha = alpha) +
        theme(panel.background = element_rect(fill = waterCol),
              panel.grid = element_blank()) +
        coord_sf(xlim = c(min(occs[[xIndex]]), max(occs[[xIndex]])),
                 ylim = c(min(occs[[yIndex]]), max(occs[[yIndex]])),
                 expand = .05, ) +
        xlab("Longitude") +
        ylab("Latitude") +
        ggtitle(paste0(spName, ", ", nrow(occs), " points"))
  }else{
    point_map <- ggplot(mapping = mapping) +
      geom_sf(data = land, color = landCol, fill = landCol) +
      geom_point(data = occs, aes(x = occs[[xIndex]], y = occs[[yIndex]]),
                 colour = ptCol, cex = ptSize, shape = 20, alpha = alpha) +
      theme(panel.background = element_rect(fill = waterCol),
            panel.grid = element_blank()) +
      coord_sf(xlim = c(min(occs[[xIndex]]), max(occs[[xIndex]])),
               ylim = c(min(occs[[yIndex]]), max(occs[[yIndex]])),
               expand = .05, ) +
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle(paste0(spName, ", ", nrow(occs), " points"))
  }
  return(point_map)
}

#' @title Comparative point mapping
#'
#' @description A convenient wrapper around `ggplot`
#' to generate formatted plots comparing two sets of
#' occurrence point plots.
#'
#' @param occs1 A `data.frame` with at least two columns
#' named "longitude" and "latitude" or that
#' can be coerced into this format.
#'
#' @param occs2 A `data.frame` with at least two columns
#' named "longitude" and "latitude" or that
#' can be coerced into this format.
#'
#' @param spName A character string with the species
#' name to be used in the plot title.
#'
#' @param land An optional coastline polygon shapefile
#' of type `sf` to provide geographic context for the
#' occurrence points.
#'
#' @param occs1Col Color for occurrence points on map
#'
#' @param occs2Col Color for occurrence points on map
#'
#' @param occs1Name An optional name for the first set
#' of occurrences, which will be color-coded to
#' `occs1Col` in the resulting plot.
#'
#' @param occs2Name An optional name for the first set
#' of occurrences, which will be color-coded to
#' `occs2Col` in the resulting plot.
#'
#' @param agreeCol Color for occurrence points shared
#' between `occs1` and `occs2`.
#'
#' @param landCol Color for land on map
#'
#' @param waterCol Color for water on map
#'
#' @param ptSize `numeric` value for `cex`;
#' size of occurrence points on map.
#'
#' @param ... Additional optional arguments to pass to
#' `ggplot` initial plot object.
#'
#' @return A `ggplot` plot object.
#'
#' @note The x and y column names of `occs1` and `occs2`
#' must match.
#'
#' @examples
#' occs <- read.csv(system.file("extdata/Steindachneria_argentea.csv",
#'                              package='voluModel'))
#' set.seed(0)
#' occs1 <- occs[sample(1:nrow(occs),
#'                      size = 24, replace = FALSE),]
#' set.seed(10)
#' occs2 <- occs[sample(1:nrow(occs),
#'                      size = 24, replace = FALSE),]
#'
#' spName <- "Steindachneria argentea"
#'
#'
#' pointCompMap(occs1 = occs1, occs2 = occs2,
#'              occs1Col = "red", occs2Col = "orange",
#'              agreeCol = "purple",
#'              occs1Name = "2D",
#'              occs2Name = "3D",
#'              landCol = "black",
#'              waterCol = "steelblue",
#'              spName = spName,
#'              ptSize = 2)
#'
#' @import ggplot2
#' @importFrom dplyr inner_join anti_join
#' @importFrom ggtext element_markdown
#'
#' @seealso \code{\link[ggplot2:ggplot]{ggplot}}
#'
#' @keywords plotting
#' @export

pointCompMap <- function(occs1, occs2,
                         spName, land = NA,
                         occs1Col = "#bd0026",
                         occs2Col = "#fd8d3c",
                         agreeCol = "black",
                         occs1Name = "Set 1",
                         occs2Name = "Set 2",
                         landCol = "gray",
                         waterCol = "steelblue",
                         ptSize = 1,
                         ...){
  args <- list(...)

  if("mapping" %in% names(args)){
    mapping <- args$mapping
  } else{
    mapping <- aes()
  }

  # Input checking
  if(!is.data.frame(occs1)){
    warning(paste0("'occs1' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(!is.data.frame(occs2)){
    warning(paste0("'occs2' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }

  if(!all(c(is.character(spName),
            is.character(occs1Name),
            is.character(occs2Name)))){
    warning(paste0("'spName', 'occs1Name', and 'occs2Name'
                   must be 'character' strings.\n"))
    return(NULL)
  }

  if(!any(is.na(land[[1]]), "sf" %in% class(land))){
    warning(paste0("'land' must either be NA or of class 'sf'."))
    return(NULL)
  }

  colVec <- c(occs1Col, occs2Col, agreeCol, landCol, waterCol)
  colTest <- areColors(colVec)

  if(!any(c(all(colTest), length(colTest) < 5))){
    warning(paste0("'pt1Col', 'pt2Col', 'agreeCol', 'landCol',
                    and 'waterCol' must be recognized colors.\n"))
    return(NULL)
  }

  if(!is.numeric(ptSize)){
    warning(paste0("'ptSize' must be numeric.\n"))
    return(NULL)
  }

  # Parse columns
  colNames1 <- colnames(occs1)
  colParse1 <- columnParse(occs1)
  if(is.null(colParse1)){
    return(NULL)
  }
  xIndex1 <- colParse1$xIndex
  yIndex1 <- colParse1$yIndex
  interp1 <- colParse1$reportMessage

  message(interp1)

  colNames2 <- colnames(occs2)
  colParse2 <- columnParse(occs2)
  if(is.null(colParse2)){
    return(NULL)
  }
  xIndex2 <- colParse2$xIndex
  yIndex2 <- colParse2$yIndex
  interp2 <- colParse2$reportMessage

  message(interp2)

  if(!all(c(colnames(occs1)[[xIndex1]] == colnames(occs2)[[xIndex2]],
           colnames(occs1)[[yIndex1]] == colnames(occs2)[[yIndex2]]))){
    warning(paste0("x and y column names in the two occurrence datasets
                   do not match.\n"))
    return(NULL)
  }

  # Where the function actually starts
  occsBoth <- NA
  occsBoth <- unique(dplyr::inner_join(occs2[,c(xIndex2, yIndex2)],
                                     occs1[,c(xIndex1, yIndex1)]))
  occsBoth$source <- rep_len("both", length.out = nrow(occsBoth))
  occs1 <- unique(dplyr::anti_join(occs1[,c(xIndex1, yIndex1)],occsBoth))
  occs1$source <- rep_len(occs1Name, length.out = nrow(occs1))
  occs2 <- unique(dplyr::anti_join(occs2[,c(xIndex2, yIndex2)],occsBoth))
  occs2$source <- rep_len(occs2Name, length.out = nrow(occs2))

  colParse1 <- columnParse(occs1)
  xIndex1 <- colParse1$xIndex
  yIndex1 <- colParse1$yIndex

  colParse2 <- columnParse(occs2)
  xIndex2 <- colParse2$xIndex
  yIndex2 <- colParse2$yIndex

  if(nrow(occs2)==0) {
    occs1 <- unique(occs1[,c(xIndex1, yIndex1)])
    occs1$source <- rep_len(occs1Name, length.out = nrow(occs1))
  }
  if (nrow(occs1)==0){
    occs2 <- unique(occs2[,c(xIndex2, yIndex2)])
    occs2$source <- rep_len(occs2Name, length.out = nrow(occs2))
  }

  allDat <- list(occsBoth[,c(colnames(occs1)[c(xIndex1,yIndex1)], "source")],
                 occs1[,c(colnames(occs1)[c(xIndex1,yIndex1)], "source")],
                 occs2[,c(colnames(occs2)[c(xIndex2,yIndex2)], "source")])
  occ_dat <- do.call("rbind", allDat[!is.na(allDat)])

  # Now add the occurrence points
  cols <- NULL
  for (x in occ_dat$source){
    if (x == occs1Name){
      cols <- c(cols, occs1Col)
    } else if (x == occs2Name){
      cols <- c(cols, occs2Col)
    } else{
      cols <- c(cols, agreeCol)
    }
  }

  occ_datIndices <- columnParse(occ_dat)

  if(any(is.na(land))){
    comparison_map <- ggplot() +
    geom_point(data = occ_dat, aes(x = occ_dat[[occ_datIndices$xIndex]],
                                   y = occ_dat[[occ_datIndices$yIndex]]),
               colour = cols, cex = ptSize, shape = 20, alpha = 1) +
    theme(panel.background = element_rect(fill = waterCol)) +
    theme(panel.grid = element_blank()) +
    coord_sf(xlim = c(min(occ_dat[[occ_datIndices$xIndex]]),
                      max(occ_dat[[occ_datIndices$xIndex]])),
             ylim = c(min(occ_dat[[occ_datIndices$yIndex]]),
                      max(occ_dat[[occ_datIndices$yIndex]])),
             expand = .05, ) +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(
      title = paste0("***", spName,"***<p>
    <span style='color:", agreeCol,";'>Overlapping</span>,
    <span style='color:", occs1Col, ";'>in ", occs1Name,
                     " dataset only</span>, and
    <span style='color:", occs2Col, ";'>in ",
                     occs2Name, " dataset only</span>")) +
    theme(plot.title = element_markdown(lineheight = .4))}
  else{
    comparison_map <- ggplot() +
      geom_sf(data = land, color = landCol, fill = landCol) +
      geom_point(data = occ_dat, aes(x = occ_dat[[occ_datIndices$xIndex]],
                                     y = occ_dat[[occ_datIndices$yIndex]]),
                 colour = cols, cex = ptSize, shape = 20, alpha = 1) +
      theme(panel.background = element_rect(fill = waterCol)) +
      theme(panel.grid = element_blank()) +
      coord_sf(xlim = c(min(occ_dat[[occ_datIndices$xIndex]]),
                        max(occ_dat[[occ_datIndices$xIndex]])),
               ylim = c(min(occ_dat[[occ_datIndices$yIndex]]),
                        max(occ_dat[[occ_datIndices$yIndex]])),
               expand = .05, ) +
      xlab("Longitude") +
      ylab("Latitude") +
      labs(
        title = paste0("***", spName,"***<p>
    <span style='color:", agreeCol,";'>Overlapping</span>,
    <span style='color:", occs1Col, ";'>in ", occs1Name,
                       " dataset only</span>, and
    <span style='color:", occs2Col, ";'>in ", occs2Name,
                       " dataset only</span>")) +
      theme(plot.title = element_markdown(lineheight = .4))
  }
  return(comparison_map)
}

#' @title Transparent Color
#'
#' @description Generates transparent colors
#'
#' @param color Anything that can be interpreted by `rgb`
#' as a color.
#'
#' @param percent A whole number between 0 and 100 specifying
#' how transparent the color should be.
#'
#' @return A `list` of length 2 with indices of the x and y
#' columns, respectively, followed by a message with a plain
#' text report of which columns were interpreted as x and y.
#'
#' @examples
#'
#' transpColor(color = "red", percent = 50)
#'
#' @importFrom grDevices rgb
#'
#' @keywords internal plotting
#'
#' @export

transpColor <- function(color, percent = 50) {
  colTest <- areColors(color)

  if(!colTest){
    warning(paste0("'color' must be a recognized color.\n"))
    return(NULL)
  }

  if(!is.numeric(percent)){
    warning(paste0("'percent' must be numeric.\n"))
    return(NULL)
  }

  if(!all(percent >= 0, 100 >= percent)){
    warning(paste0("'percent' must be between 0 and 100.\n"))
    return(NULL)
  }

  ## Get RGB values for named color
  rgb.val <- col2rgb(color)

  ## Make new color using input color as base and transparency set by alpha
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               maxColorValue = 255,
               alpha = (100 - percent) * 255 / 100)

  ## Save the color
  return(t.col)
}

#' @title Comparative raster mapping
#'
#' @description A convenient wrapper around `spplot`
#' to generate formatted plots comparing two rasters.
#' This is used in the context of voluModel to
#' overlay semi-transparent distributions (coded as 1)
#' in two different `RasterLayers`.
#'
#' @param rast1 A single `RasterLayer` showing the
#' distribution of the species corresponding to
#' `rast1Name`. Should have values of 0 (absence)
#' and 1 (presence). Can also be `NULL`.
#'
#' @param rast2 A single `RasterLayer` showing the
#' distribution of the species corresponding to
#' `rast2Name`. Should have values of 0 (absence)
#' and 1 (presence). Must match the extent and
#' resolution of `rast1`. Can also be `NULL.`
#'
#' @param rast1Name A character string with the name
#' of the species depicted in `rast1`.
#'
#' @param rast2Name A character string with the name
#' of the species depicted in `rast2`.
#'
#' @param land An optional coastline polygon shapefile
#' of type `sf` to provide geographic context for the
#' occurrence points.
#'
#' @param col1 Color for `rast1` presences
#'
#' @param col2 Color for `rast2` presences
#'
#' @param rast1Name An optional name for the first set
#' of occurrences, which will be color-coded to
#' `occs1Col` in the resulting plot.
#'
#' @param rast2Name An optional name for the first set
#' of occurrences, which will be color-coded to
#' `occs2Col` in the resulting plot.
#'
#' @param colBoth Color for cells with presences for
#' both `rast1` and `rast2`.
#'
#' @param colNeither Color for cells with absences for
#' both `rast1` and `rast2`.
#'
#' @param landCol Color for land on map.
#'
#' @param land An optional coastline polygon shapefile
#' of type `sf` to provide geographic context for the
#' occurrence points.
#'
#' @param title A title for the plot.
#'
#' @param ... Additional optional arguments to pass to
#' `spplot` initial plot object.
#'
#' @return A plot of class `trellis` overlaying mapped,
#' semitransparent extents of the input rasters
#'
#' @note The extents of `rast1` and `rast2`
#' must match.
#'
#' @examples
#' library(raster)
#' rast1 <- raster(ncol=10, nrow=10)
#' values(rast1) <- rep(0:1, 50)
#'
#' rast2 <- raster(ncol=10, nrow=10)
#' values(rast2) <- c(rep(0, 50), rep(1,50))
#'
#' rasterComp(rast1 = rast1, rast2 = rast2)
#'
#' @import raster
#' @importFrom latticeExtra as.layer
#'
#' @seealso \code{\link[raster:spplot]{spplot}}
#'
#' @keywords plotting
#' @export

rasterComp <- function(rast1 = NULL, rast2 = NULL,
                       col1 = "red", col2 = "blue",
                       rast1Name = "Set 1", rast2Name = "Set 2",
                       colNeither = "white", colBoth = "purple",
                       land = NA, landCol = "black",
                       title = "A Raster Comparison", ...){

  args <- list(...)

  if("maxpixels" %in% names(args)){
    maxpixels <- args$maxpixels
  } else{
    maxpixels <- 50000
  }

  colVec <- c(col1, col2, colBoth, colNeither, landCol)
  colTest <- areColors(colVec)

  if(!any(c(all(colTest), length(colTest) < 4))){
    warning(paste0("'col1', 'col2', 'colBoth', 'colNeither' and 'landCol'
                   must be recognized colors.\n"))
    return(NULL)
  }

  if(!any(is.na(land[[1]]), "sf" %in% class(land))){
    warning(paste0("'land' must either be NA or of class 'sf'."))
    return(NULL)
  }

  if(!all(c(is.character(rast1Name),
            is.character(rast2Name),
            is.character(title)))){
    warning(paste0("'rast1Name', 'rast2Name', and 'title'
                   must be 'character' strings.\n"))
    return(NULL)
  }

  if(any(all(!inherits(rast1, what = "RasterLayer"), !is.null(rast1)),
         all(!inherits(rast2, what = "RasterLayer"), !is.null(rast2)))){
    warning(paste0("'rast1' and 'rast2', must either be objects of type
                   'RasterLayer' or NULL.\n"))
    return(NULL)
  }

  if(any(is.null(rast1), is.null(rast2))){
    if(all(is.null(rast1),!is.null(rast2))){
      rast1 <- rast2
      values(rast1) <- 0
      message(paste0("'rast1' was null. Replaced with blank raster."))
    }else if(all(is.null(rast2),!is.null(rast1))){
      rast2 <- rast1
      values(rast2) <- 0
      message(paste0("'rast2' was null. Replaced with blank raster."))
    } else{
      rast1 <- raster(ncol=10, nrow=10)
      values(rast1) <- 0
      rast2 <- raster(ncol=10, nrow=10)
      values(rast2) <- 0
      message(paste0("Both 'rast1' and 'rast2' were null.\n",
                     "They were replaced with blank rasters."))
    }
  }

  # Here is where the function actually starts
  myCols <- c(transpColor("white", percent = 100),
              transpColor(col1, percent = 50),
              transpColor(col2, percent = 50),
              transpColor(colBoth, percent = 30))
  if(any(is.na(land))){
    if(all(cellStats(rast2, sum) > 0, cellStats(rast1, sum) > 0)){
      spplot(rast1, col.regions = myCols[c(1,2)], cuts = 1, colorkey = FALSE,
             key=list(space="right", points=list(pch = 22, cex = 2,
                                                 fill=c("white",myCols[c(2:4)])),
                      text=list(c("Neither", rast1Name, rast2Name, "Both"))),
             col="transparent", main = title,
             par.settings = list(mai = c(0,0,0,0)),
             maxpixels = maxpixels) +
        as.layer(spplot(rast2, col.regions = myCols[c(1,3)],
                        cuts = 1, col="transparent"))
    } else if(cellStats(rast2, sum) == 0){
      spplot(rast1, col.regions = myCols[c(1,2)], cuts = 1, colorkey = FALSE,
             key=list(space="right", points=list(pch = 22, cex = 2,
                                                 fill=c("white",myCols[c(2:4)])),
                      text=list(c("Neither", rast1Name, rast2Name, "Both"))),
             col="transparent", main = title,
             par.settings = list(mai = c(0,0,0,0)),
             maxpixels = maxpixels)
    } else{
      spplot(rast2, col.regions = myCols[c(1,3)], cuts = 1, colorkey = FALSE,
             key=list(space="right", points=list(pch = 22, cex = 2,
                                                 fill=c("white",myCols[3])),
                      text=list(c("Neither", rast2Name))),
             col="transparent", main = title,
             maxpixels = maxpixels,
             par.settings = list(mai = c(0,0,0,0)))
    }
  } else {
    if (all(cellStats(rast2, sum) > 0, cellStats(rast1, sum) > 0)){
      spplot(rast1, col.regions = myCols[c(1,2)],
             cuts = 1, colorkey = FALSE,
             key=list(space="right",
                      points=list(pch = 22, cex = 2,
                                  fill=c("white", myCols[c(2:4)])),
                      text=list(c("Neither", rast1Name, rast2Name, "Both"))),
             col="transparent",  main = title,
             maxpixels = maxpixels,
             par.settings = list(mai = c(0,0,0,0))) +
        as.layer(spplot(rast2, col.regions = myCols[c(1,3)], cuts = 1, col="transparent")) +
        as.layer(spplot(as(land,"Spatial"), fill=landCol))
    } else if (cellStats(rast2, sum) == 0){
      spplot(rast1, col.regions = myCols[c(1,2)],
             cuts = 1, colorkey = FALSE,
             key=list(space="right",
                      points=list(pch = 22, cex = 2,
                                  fill=c("white",myCols[2])),
                      text=list(c("Neither", rast1Name))),
             col="transparent", main = title,
             maxpixels = maxpixels,
             par.settings = list(mai = c(0,0,0,0))) +
        as.layer(spplot(as(land,"Spatial"), fill=landCol))
    } else{
      spplot(rast2, col.regions = myCols[c(1,3)], cuts = 1, colorkey = FALSE,
             key=list(space="right", points=list(pch = 22, cex = 2,
                                                 fill=c("white",myCols[3])),
                      text=list(c("Neither", rast2Name))),
             col="transparent", main = title,
             maxpixels = maxpixels,
             par.settings = list(mai = c(0,0,0,0))) +
        as.layer(spplot(as(land,"Spatial"), fill=landCol))
    }
  }
}

#' @title Diversity stack
#'
#' @description Takes list of rasters of species distributions
#' (interpreted as 1 = presence, 0 = absence), which do not
#' have to have the same extents, and stack them to create an
#' estimate of species richness that matches the extent and
#' resolution of a template.
#'
#' @param rasterList A `list` of `RasterLayer` objects, which
#' are interpreted as species distributions (1 = presence,
#' 0 = absence).
#'
#' @param template A `RasterLayer` with the desired extent
#'
#' @return A `RasterLayer`
#'
#' @examples
#' library(raster)
#' rast1 <- raster(ncol=10, nrow=10)
#' values(rast1) <- rep(0:1, 50)
#'
#' rast2 <- raster(ncol=10, nrow=10)
#' values(rast2) <- c(rep(0, 50), rep(1,50))
#'
#' rastList <- list(rast1, rast2)
#' result <- diversityStack(rasterList = rastList,
#'                          template = rast2)
#' result
#' plot(result)
#'
#' @import raster
#' @keywords plotting
#' @export

diversityStack <- function(rasterList, template){
  if(!inherits(rasterList, what = "list")){
    warning(paste0("'rasterList' must be of class 'list'.\n"))
    return(NULL)
  }

  if(!all(unlist(lapply(rasterList,
                        function(X){grepl("Raster*", class(X))})))){
    warning(paste0("All objects in 'rasterList' must be of class 'Raster*'.\n"))
    return(NULL)
  }

  if(!grepl("Raster*", class(template))){
    warning(paste0("'template' must be of class 'Raster*'.\n"))
    return(NULL)
  }

  diversityRaster <- raster(nrows = template@nrows, ncol = template@ncols,
                            ext = template@extent, crs = template@crs)
  values(diversityRaster) <- 0

  for(i in 1:length(rasterList)){
    temp <- rasterList[[i]]
    if(testIntersection(temp,template)){
      temp <- raster::resample(temp, template)
      temp[is.na(temp[])] <- 0
      diversityRaster <- diversityRaster + temp
    }
  }
  return(diversityRaster)
}

#' @title Single raster plot
#'
#' @description A convenient wrapper around `spplot`
#' to generate a formatted plot of a single raster.
#'
#' @param rast A single `Raster*` layer on a continuous
#' scale.
#'
#' @param land An optional coastline polygon shapefile
#' of type `sf` to provide geographic context for the
#' occurrence points.
#'
#' @param landCol Color for land on map.
#'
#' @param title A title for the plot.
#'
#' @param ... Additional optional arguments to pass to
#' `spplot` initial plot object or `viridis`.
#'
#' @return A plot of class `trellis` mapping the values
#' of the input raster layer
#'
#' @examples
#' library(raster)
#' rast <- raster(ncol=10, nrow=10)
#' values(rast) <- seq(0,99, 1)
#'
#' oneRasterPlot(rast = rast)
#'
#' @import raster
#' @importFrom viridisLite viridis
#' @importFrom latticeExtra as.layer
#'
#' @seealso \code{\link[viridisLite:viridis]{viridis}} \code{\link[raster:spplot]{spplot}}
#'
#' @keywords plotting
#' @export

oneRasterPlot <- function(rast,
                          land = NA, landCol = "black",
                          title = "A Raster", ...){
  #Input processing
  args <- list(...)

  if("maxpixels" %in% names(args)){
    maxpixels <- args$maxpixels
  } else{
    maxpixels <- 10000
  }

  if("alpha" %in% names(args)){
    alpha <- args$alpha
  } else{
    alpha <- 1
  }

  if("option" %in% names(args)){
    option <- args$option
  } else{
    option <- "plasma"
  }

  # Input error checking
  if(!grepl("Raster*", class(rast))){
    warning(paste0("'rast' must be of class 'Raster*'.\n"))
    return(NULL)
  }

  if(!any(is.na(land), "sf" %in% class(land))){
    warning(paste0("'land' must either be NA or of class 'sf'."))
    return(NULL)
  }

  colTest <- areColors(landCol)

  if(!any(c(all(colTest), length(colTest) < 1))){
    warning(paste0("'landCol' must be a recognized color.\n"))
    return(NULL)
  }

  if(!is.character(title)){
    warning(paste0("'title' must be a 'character' string.\n"))
    return(NULL)
  }

  #Function body
  at <- seq(from = cellStats(rast, min), to = cellStats(rast, max),
            by = (cellStats(rast, max)-cellStats(rast, min))/11)

  if(any(is.na(land))){
    spplot(rast, col = "transparent",
           col.regions = viridis(11, alpha = alpha,
                                 option = option),
           at = at, main = title,
           maxpixels = maxpixels)
  } else {
    spplot(rast, col = "transparent",
           col.regions = viridis(11, alpha = alpha,
                                 option = option),
           at = at, main = title,
           maxpixels = maxpixels) +
      as.layer(spplot(as(land,"Spatial"), fill=landCol, main = title))
  }
}

#' @title Plotting 3D model in 2D
#'
#' @description This script plots a semitransparent layer
#' of suitable habitat for each depth layer. The redder
#' the color, the shallower the layer, the bluer, the
#' deeper. The more saturated the color, the more layers
#' with suitable habitat.
#'
#' @param rast A `Raster*` with the 3D presence/absence
#' distribution of a species (interpreted as 1 = presence,
#' 0 = absence). This could be a `RasterBrick` or a
#' `RasterStack`.
#'
#' @param land An optional coastline polygon shapefile
#' of type `sf` to provide geographic context for the
#' occurrence points.
#'
#' @param landCol Color for land on map.
#'
#' @param title A title for the plot. If not title is
#' supplied, the title "Suitability from (MINIMUM
#' DEPTH) to (MAXIMUM DEPTH)" is inferred from
#' names of `RasterStack`.
#'
#' @param ... Additional optional arguments.
#'
#' @note Only include the depth layers that you actually
#' want to plot.
#'
#' @examples
#' library(raster)
#'
#' rast1 <- raster(ncol=10, nrow=10)
#' values(rast1) <- rep(0:1, 50)
#'
#' rast2 <- raster(ncol=10, nrow=10)
#' values(rast2) <- c(rep(0, 50), rep(1,50))
#'
#' rast3 <- raster(ncol=10, nrow=10)
#' values(rast3) <- rep(c(1,0,0,1), 25)
#'
#' distBrick <- brick(rast1, rast2, rast3)
#'
#' plotLayers(distBrick)
#'
#' @import raster
#' @importFrom viridisLite viridis
#' @importFrom latticeExtra as.layer
#'
#' @seealso \code{\link[viridisLite:viridis]{viridis}} \code{\link[raster:spplot]{spplot}}
#'
#' @keywords plotting
#'
#' @return A plot of class `trellis`
#'
#' @export


plotLayers <- function(rast,
                      land = NA, landCol = "black",
                      title = NULL, ...){
  #Input processing
  args <- list(...)

  if("maxpixels" %in% names(args)){
    maxpixels <- args$maxpixels
  } else{
    maxpixels <- 10000
  }

  # Input error checking
  if(!grepl("Raster*", class(rast))){
    warning(paste0("'rast' must be of class 'Raster*'.\n"))
    return(NULL)
  }

  if(!any(is.na(land), "sf" %in% class(land))){
    warning(paste0("'land' must either be NA or of class 'sf'."))
    return(NULL)
  }

  colTest <- areColors(landCol)

  if(!any(c(all(colTest), length(colTest) < 1))){
    warning(paste0("'landCol' must be a recognized color.\n"))
    return(NULL)
  }

  if(is.null(title)){
   title <- paste0("Suitability from ",
           names(rast)[[1]]," to ",
           names(rast)[[nlayers(rast)]])
  }

  #Function body
  redVal <- 1
  blueVal <- 0
  stepSize <- 1/(nlayers(rast) + 1)

  plotStart <- spplot(rast[[1]],
                      col.regions = c(rgb(0,0,0,0),
                                      rgb(redVal,0,blueVal,stepSize)),
                      cuts = 1, colorkey = FALSE, col="transparent",
                      main = title,
                      par.settings = list(mai = c(0,0,0,0)),
                      maxpixels = maxpixels)

  for(i in 2:nlayers(rast)){
    redVal <- redVal - stepSize
    blueVal <- blueVal + stepSize
    plotStart <- plotStart + as.layer(spplot(rast[[i]],
                                             col.regions = c(rgb(0,0,0,0),
                                                             rgb(redVal,0,
                                                                 blueVal,
                                                                 stepSize)),
                                             cuts = 1, col="transparent"))
  }

  if(!any(is.na(land))){
    plotStart <- plotStart +
      as.layer(spplot(as(land,"Spatial"), fill=landCol))
  }
  return(plotStart)
}
