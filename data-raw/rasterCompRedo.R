# Setup ----
library(terra)
library(tidyterra)
library(ggplot2)
library(voluModel) # Because of course
library(ggplot2) # For fancy plotting
library(viridisLite) # For high-contrast plotting palettes
library(dplyr) # To filter data
library(terra) # Now being transitioned in
library(sf) # Now being transitioned in
library(sp)
library(latticeExtra)
library(ggnewscale)

# Load data
oxygenSmooth <- rast(system.file("extdata/oxygenSmooth.tif",
                                 package='voluModel'))

occs <- read.csv(system.file("extdata/Steindachneria_argentea.csv",
                             package='voluModel'))

td <- tempdir()
unzip(system.file("extdata/woa18_decav_t00mn01_cropped.zip",
                  package = "voluModel"),
      exdir = paste0(td, "/temperature"), junkpaths = T)
temperature <- sf::st_read(paste0(td, "/temperature/woa18_decav_t00mn01_cropped.shp"))
temperature <- as(temperature, "Spatial")
temperature@data[temperature@data == -999.999] <- NA

# Turn into SpatRaster
template <- rast(nrows = length(unique(temperature@coords[,2])),
                 ncols = length(unique(temperature@coords[,1])),
                 extent = ext(temperature))
tempTerVal <- rasterize(x = temperature@coords, y = template,
                        values = temperature@data)

# Get names of depths
envtNames <- gsub("[d,M]", "", names(temperature))
envtNames[[1]] <- "0"
names(tempTerVal) <- envtNames
temperature <- tempTerVal

# Oxygen processing
names(oxygenSmooth) <- names(temperature)

occurrences <- occs %>% dplyr::select(decimalLongitude, decimalLatitude, depth) %>%
  distinct() %>% filter(dplyr::between(depth, 1, 2000))

# Gets the layer index for each occurrence by matching to depth
layerNames <- as.numeric(names(temperature))
occurrences$index <- unlist(lapply(occurrences$depth,
                                   FUN = function(x) which.min(abs(layerNames - x))))
indices <- unique(occurrences$index)
downsampledOccs <- data.frame()
for(i in indices){
  tempPoints <- occurrences[occurrences$index==i,]
  tempPoints <- downsample(tempPoints, temperature[[1]], verbose = FALSE)
  tempPoints$depth <- rep(layerNames[[i]], times = nrow(tempPoints))
  downsampledOccs <- rbind(downsampledOccs, tempPoints)
}
occsWdata <- downsampledOccs[,c("decimalLatitude", "decimalLongitude", "depth")]

occsWdata$temperature <- xyzSample(occs = occsWdata, temperature)
occsWdata$AOU <- xyzSample(occs = occsWdata, oxygenSmooth)
occsWdata <- occsWdata[complete.cases(occsWdata),]

# Land shapefile
land <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")[1]

# Study region
studyRegion <- marineBackground(occsWdata, buff = 1000000)

# Get limits
tempLims <- quantile(occsWdata$temperature,c(0, 1))
aouLims <- quantile(occsWdata$AOU,c(0, 1))

# Reclassify environmental bricks to presence/absence
temperaturePresence <- classify(temperature,
                                rcl = matrix(c(-Inf,tempLims[[1]],0,
                                               tempLims[[1]], tempLims[[2]], 1,
                                               tempLims[[2]], Inf, 0),
                                             ncol = 3, byrow = TRUE))
AOUpresence <- classify(oxygenSmooth,
                        rcl = matrix(c(-Inf, aouLims[[1]],0,
                                       aouLims[[1]], aouLims[[2]], 1,
                                       aouLims[[2]], Inf, 0),
                                     ncol = 3, byrow = TRUE))

# Put it all together
envelopeModel3D <- temperaturePresence * AOUpresence
envelopeModel3D <- mask(crop(envelopeModel3D, studyRegion),
                        mask = studyRegion)
names(envelopeModel3D) <- names(temperature)
rm(AOUpresence, downsampledOccs, occurrences, temperaturePresence,
   tempPoints, aouLims, envtNames, i, indices, layerNames, td, tempLims)

# Internal function shit ----
rast1 = envelopeModel3D[[1]]
rast1Name = "Surface"
rast2 = envelopeModel3D[[10]]
rast2Name = "45m"
land = land
landCol = "black"
title = "Suitability of Habitat for Luminous Hake\nAt Two Different Depths"
col1 = "#1b9e777F"
col2 = "#7570b37F"
maxpixels <- 50000
colVec <- c(col1, col2, landCol)
colTest <- areColors(colVec)
myCols <- c(transpColor("white", percent = 100),
            transpColor(col1, percent = 50),
            transpColor(col2, percent = 50),
            blendColor(transpColor(col1, percent = 50),
                       transpColor(col2, percent = 50)))

# Old version ----
sp::spplot(raster::raster(rast1), col.regions = myCols[c(1,2)],
           cuts = 1, colorkey = FALSE,
           key=list(space="right",
                    points=list(pch = 22, cex = 2,
                                fill=c("white", myCols[c(2:4)])),
                    text=list(c("Neither", rast1Name, rast2Name, "Both"))),
           col="transparent",  main = title,
           maxpixels = maxpixels,
           sp.layout=list(as(land, "Spatial"), fill=landCol, first=FALSE),
           par.settings = list(mai = c(0,0,0,0))) +
  as.layer(sp::spplot(raster::raster(rast2), col.regions = myCols[c(1,3)], cuts = 1, col="transparent"))

# New version ----

grat <- graticule(lon = seq(-180, 180, 10), lat = seq(-90,90,10), crs = crs(rast1))
plot.new()
plot(rast1, col = c(myCols[1], myCols[2]), legend = FALSE, mar = c(2,2,3,2))
plot(rast2, col = c(myCols[1], myCols[3]), legend = FALSE, add = T)
plot(grat, col="gray50", add = TRUE)
plot(vect(land), col = landCol, add = TRUE)
title(main = title, cex.main = 1.1)
legend(x = round(xmax(rast1)) + 1, y = round(ymax(rast1)) + 1,
       bty = "n",
       legend = c("Neither", rast1Name, rast2Name, "Both"),
       fill = myCols)
finalPlot <- recordPlot()

"#FFFFFF00" "#1B9E777F" "#7570B37F" "#488795FE"
