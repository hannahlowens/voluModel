# Visualization ----
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
#' occs <- read.csv(system.file("extdata/Aphanopus_intermedius.csv",
#'                              package='voluModel'))
#' spName <- "Aphanopus intermedius"
#' pointMap(occs = occs, spName = spName, land = NA)
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

  areColors <- function(x) {
    sapply(x, function(X) {
      tryCatch(is.matrix(col2rgb(X)),
               error = function(e) FALSE)
    })
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
  colParse <- voluModel:::columnParse(occs)
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
                   colour = ptCol, cex = ptSize, shape = 20, alpha = 2/3) +
        theme(panel.background = element_rect(fill = waterCol),
              panel.grid = element_blank()) +
        coord_sf(xlim = c(min(occs[[xIndex]]), max(occs[[xIndex]])),
                 ylim = c(min(occs[[yIndex]]), max(occs[[yIndex]])), expand = .05, ) +
        xlab("Longitude") +
        ylab("Latitude") +
        ggtitle(paste0(spName, ", ", nrow(occs), " points"))
  }else{
    point_map <- ggplot(mapping = mapping) +
      geom_sf(data = land, color = landCol, fill = landCol) +
      geom_point(data = occs, aes(x = occs[[xIndex]], y = occs[[yIndex]]),
                 colour = ptCol, cex = ptSize, shape = 20, alpha = 2/3) +
      theme(panel.background = element_rect(fill = waterCol),
            panel.grid = element_blank()) +
      coord_sf(xlim = c(min(occs[[xIndex]]), max(occs[[xIndex]])),
               ylim = c(min(occs[[yIndex]]), max(occs[[yIndex]])), expand = .05, ) +
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle(paste0(spName, ", ", nrow(occs), " points"))
  }
  return(point_map)
}

#' @title Comparative point mapping
#'
#' @description A convenient wrapper around ggplot
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
#' @param pt1Col Color for occurrence points on map
#'
#' @param pt2Col Color for occurrence points on map
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
#' @return A `data.frame` with two columns named "longitude"
#' and "latitude" or with names that were used when coercing
#' input data into this format.
#'
#' @note The x and y column names of `occs1` and `occs2`
#' must match.
#'
#' @examples
#' occs <- read.csv(system.file("extdata/Aphanopus_intermedius.csv",
#'                              package='voluModel'))
#' spName <- "Aphanopus intermedius"
#' pointMap(occs = occs, spName = spName, land = NA)
#'
#' @import ggplot2
#' @import dplyr
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

  areColors <- function(x) {
    sapply(x, function(X) {
      tryCatch(is.matrix(col2rgb(X)),
               error = function(e) FALSE)
    })
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
  colParse1 <- voluModel:::columnParse(occs1)
  if(is.null(colParse1)){
    return(NULL)
  }
  xIndex1 <- colParse1$xIndex
  yIndex1 <- colParse1$yIndex
  interp1 <- colParse1$reportMessage

  message(interp1)

  colNames2 <- colnames(occs2)
  colParse2 <- voluModel:::columnParse(occs2)
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
  occs1 <- unique(anti_join(occs1[,c(xIndex1, yIndex1)],occsBoth))
  occs1$source <- rep_len(occs1Name, length.out = nrow(occs1))
  occs2 <- unique(anti_join(occs2[,c(xIndex2, yIndex2)],occsBoth))
  occs2$source <- rep_len(occs2Name, length.out = nrow(occs2))

  colParse1 <- voluModel:::columnParse(occs1)
  xIndex1 <- colParse1$xIndex
  yIndex1 <- colParse1$yIndex

  colParse2 <- voluModel:::columnParse(occs2)
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

  occ_datIndices <- voluModel:::columnParse(occ_dat)

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
    <span style='color:", occs2Col, ";'>in ", occs2Name, " dataset only</span>")) +
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
    <span style='color:black;'>Overlapping</span>,
    <span style='color:", occs1Col, ";'>in ", occs1Name,
                       " dataset only</span>, and
    <span style='color:", occs2Col, ";'>in ", occs2Name, " dataset only</span>")) +
      theme(plot.title = element_markdown(lineheight = .4))
  }
  return(comparison_map)
}

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color

  ## Get RGB values for named color
  rgb.val <- col2rgb(color)

  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)

  ## Save the color
  invisible(t.col)
}

rasterMapFunction <- function(rast1, rast2, title){
  myCols <- c(t_col("white", perc = 100, name = "clear"),
              t_col("red", perc = 50, name = "red50"),
              t_col("blue", perc = 50, name = "blue50"),
              t_col("purple", perc = 30, name = "purple50"))

  if (!is.null(rast1) && !is.null(rast2)){
    spplot(rast1, col.regions = myCols[c(1,2)], cuts = 1, colorkey = F,
           key=list(space="right", points=list(pch = 22, cex = 2, fill=c("white",myCols[c(2:4)])),
                    text=list(c("Absent", "2D", "3D", "Both"))), col="transparent", main = title,
           par.settings = list(mai = c(0,0,0,0))) +
      as.layer(spplot(rast2, col.regions = myCols[c(1,3)], cuts = 1, col="transparent")) +
      layer(sp.polygons(rangeBuilder::gshhs, fill="black"))
  } else if (is.null(rast1) && is.null(rast2)){
    r <- raster(ncol=3,nrow=3)
    r[] <- 1:(3*3)
    spplot(r, main = title)
  } else if(is.null(rast1)){
    spplot(rast2, col.regions = myCols[c(1,3)], cuts = 1, colorkey = F,
           key=list(space="right", points=list(pch = 22, cex = 2, fill=c("white",myCols[3])),
                    text=list(c("Absent", "3D"))), col="transparent", main = title,
           par.settings = list(mai = c(0,0,0,0))) +
      layer(sp.polygons(rangeBuilder::gshhs, fill="black"))
  } else {
    spplot(rast1, col.regions = myCols[c(1,2)], cuts = 1, colorkey = F,
           key=list(space="right", points=list(pch = 22, cex = 2, fill=c("white",myCols[2])),
                    text=list(c("Absent", "2D"))), col="transparent", main = title,
           par.settings = list(mai = c(0,0,0,0))) +
      layer(sp.polygons(rangeBuilder::gshhs, fill="black"))
  }
}

test_intersection <- function(a,b){
  #reads in two rasters and tests for overlap T or F
  # if returns TRUE then there is overlap
  # try error is included b/c errors has come up with other test data
  !(class(try(intersect(a,b),T ))=='try-error' || is.null(intersect(a,b)))
}

diversityStack <- function(rasterList, template){
  diversityRaster <- raster(nrows = template@nrows, ncol = template@ncols,
                            ext = template@extent, crs = template@crs)
  values(r=diversityRaster) <- 0

  for(i in 1:length(rasterList)){
    temp <- rasterList[[i]]
    if(test_intersection(temp,template)){
      temp <- raster::resample(temp, template)
      temp[is.na(temp[])] <- 0
      diversityRaster <- diversityRaster + temp
    }
  }
  return(diversityRaster)
}

oneRasterPlot <- function(rast, title, scaleRange){
  at <- seq(from = scaleRange[[1]], to = scaleRange[[2]], by = (scaleRange[2]-scaleRange[1])/11)
  spplot(rast, col = "transparent",
         col.regions = viridis::plasma(11), at = at,
         xlim=c(-100, 25), main = title) +
    latticeExtra::layer(sp.polygons(rangeBuilder::gshhs, fill = "black"))
}
