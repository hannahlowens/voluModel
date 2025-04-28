# testing depthMatch function

library(terra)
library(sf)

# reading in example data
occs <- read.csv(system.file("extdata/Steindachneria_argentea.csv",
                             package='voluModel'))
occs <- occs %>% dplyr::select(scientificName, decimalLatitude, decimalLongitude, depth)
colnames(occs) <- c("species", "latitude", "longitude", "depth")
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
occs_sf <- st_as_sf(x = occs,
               coords = c("longitude", "latitude"),
               crs = projcrs)

# creating raster brick
temp1 <- rast(ext(occs_sf))
temp2 <- rast(ext(occs_sf))
temp3 <- rast(ext(occs_sf))
temp <- c(temp1, temp2, temp3)
names(temp) <- c(10, 100, 1000)

# testing function
depth_occs <- depthMatch(occs = occs, rasterTemplate = temp)

