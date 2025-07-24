# testing cleanDepth function

library(rnaturalearth)
library(terra)
library(sf)
library(dplyr)
library(voluModel)

# reading in example data
occs <- read.csv(system.file("extdata/Steindachneria_argentea.csv",
                             package='voluModel'))
occs <- occs %>% dplyr::select(scientificName, decimalLatitude, decimalLongitude, depth)
colnames(occs) <- c("species", "latitude", "longitude", "depth")
target_crs <- "+proj=longlat +datum=WGS84"
occs_sf <- st_as_sf(x = occs,
                    coords = c("longitude", "latitude"),
                    crs = target_crs)

# creating example land polygon
land <- st_as_sf(ne_countries())[1] %>% st_set_crs(target_crs)
land <- land[-which(!(st_is_valid(land))),]

# creating example bathymetry layer
bath_data <- rast(extend(ext(occs_sf), 10), crs = target_crs, resolution = 1)
values(bath_data) <- sample(c(-4000:0), size = length(values(bath_data)))

# setting example depth_range
depth_range <- c(0, 300)

# test case 1: no land_poly or depth_range supplied, flag and bottom_correct are both false
testcase1 <- cleanDepth(occs = occs, bathy = bath_data)

# test case 2: no land_poly or depth_range supplied, flag is true but bottom_correct is false
testcase2 <- cleanDepth(occs = occs, bathy = bath_data, flag = T)

# test case 3: no land_poly or depth_range supplied, flag and bottom_correct are true
testcase3 <- cleanDepth(occs = occs, bathy = bath_data, flag = T, bottom_correct = T)

# test_case 4: land_poly is supplied, no depth_range, flag and bottom_correct are false
testcase4 <- cleanDepth(occs = occs, bathy = bath_data, land_poly = land)

# test_case 5: land_poly is supplied, no depth_range, flag is true and bottom_correct is false
testcase5 <- cleanDepth(occs = occs, bathy = bath_data, land_poly = land, flag = T)

# test case 6: land_poly is supplied, no depth_range, flag and bottom_correct are true
testcase6 <- cleanDepth(occs = occs, bathy = bath_data, land_poly = land, flag = T,
                        bottom_correct = T)

# test case 7: land_poly is supplied, depth_range is supplied, flag and bottom_correct are false
testcase7 <- cleanDepth(occs = occs, bathy = bath_data, land_poly = land,
                        depth_range = depth_range,
                        flag = T,
                        bottom_correct = T)
