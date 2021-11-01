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
#' @param world An optional coastline shapefile to
#' provide geographic context for the occurrence points.
#'
#' @param ... Additional optional arguments to pass to
#' `ggplot` initial plot object.
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
#' library(rnaturalearth)
#'
#'
#' @import ggplot2
#'
#' @seealso \code{\link[ggplot2:ggplot]{ggplot}}
#'
#' @keywords plotting


pointMap <- function(occs, spName, world = NA, ...){
  args <- list(...)

  # Input checking
  if(!is.data.frame(occs)){
    warning(paste0("'occs' must be an object of class 'data.frame'.\n"))
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

  # Where the actual function happens
  point_map <- ggplot() +
    geom_sf(data = world, color = "gray", fill = "gray") +
    geom_point(data = occs, aes(x = occs[[xIndex]], y = occs[[yIndex]]),
               colour = "#bd0026", shape = 20, alpha = 2/3) +
    theme(panel.background = element_rect(fill = "steelblue")) +
    theme(panel.grid = element_blank()) +
    #xlab("Longitude") +
    #ylab("Latitude") +
    ggtitle(paste0(spName, ", ", nrow(occs), " points"))
  return(point_map)
}

doublePointMapDimensions <- function(ps1, ps2, spName, world){
  psBoth <- NA
  if (!is.null(nrow(ps1)) && !is.null(nrow(ps2))){
    psBoth <- unique(dplyr::inner_join(ps2[,c("decimalLongitude", "decimalLatitude")],
                                       ps1[,c("decimalLongitude", "decimalLatitude")]))
    psBoth$source <- rep_len("both", length.out = nrow(psBoth))
    psBoth <- psBoth[,c("decimalLongitude", "decimalLatitude", "source")]
    ps1 <- unique(anti_join(ps1[,c("decimalLongitude", "decimalLatitude")],psBoth))
    ps1$source <- rep_len("twoD", length.out = nrow(ps1))
    ps2 <- unique(anti_join(ps2[,c("decimalLongitude", "decimalLatitude")],psBoth))
    ps2$source <- rep_len("threeD", length.out = nrow(ps2))
    ps2 <- ps2[,c("decimalLongitude", "decimalLatitude", "source")]
  }
  else if(!is.null(nrow(ps1))) {
    ps1 <- unique(ps1[,c("decimalLongitude", "decimalLatitude")])
    ps1$source <- rep_len("twoD", length.out = nrow(ps1))
  } else if (!is.null(nrow(ps2))){
    ps2 <- unique(ps2[,c("decimalLongitude", "decimalLatitude")])
    ps2$source <- rep_len("threeD", length.out = nrow(ps2))
  } else {
    obis_map <- ggplot() +
      geom_sf(data = world, color = "gray", fill = "gray") +
      theme(panel.background = element_rect(fill = "steelblue")) +
      theme(panel.grid = element_blank()) +
      xlab("Longitude") +
      ylab("Latitude") +
      labs(
        title = paste0("***", spName,"***
    <span style='color:black;'>Overlapping</span>,
    <span style='color:#bd0026;'> in 2D dataset only</span>, and<br>
    <span style='color:#fd8d3c;'> in in 3D dataset only</span>")) +
      theme(plot.title = element_markdown(lineheight = .4))
    return(obis_map)
    break
  }
  allDat <- list(psBoth, ps1, ps2)
  occ_dat <- do.call("rbind", allDat[!is.na(allDat)])

  # Now add the occurrence points
  cols <- NULL
  for (x in occ_dat$source){
    if (x == "twoD"){
      cols <- c(cols, "#bd0026")
    } else if (x == "threeD"){
      cols <- c(cols, "#fd8d3c")
    } else{
      cols <- c(cols, "black")
    }
  }

  obis_map <- ggplot() +
    geom_sf(data = world, color = "gray", fill = "gray") +
    geom_point(data = occ_dat, aes(x = decimalLongitude, y = decimalLatitude),
               colour = cols, shape = 20, alpha = 1) +
    theme(panel.background = element_rect(fill = "steelblue")) +
    theme(panel.grid = element_blank()) +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(
      title = paste0("***", spName,"***
    <span style='color:black;'>Overlapping</span>,
    <span style='color:#bd0026;'>in 2D dataset only</span>, and<br>
    <span style='color:#fd8d3c;'> in 3D dataset only</span>")) +
    theme(plot.title = element_markdown(lineheight = .4))
  return(obis_map)
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
