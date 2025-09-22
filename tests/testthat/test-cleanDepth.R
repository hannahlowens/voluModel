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

# testing that cleanDepth outputs as expected
test_that("cleanDepth outputs as expected", {
  expect_error(cleanDepth())
  testcase1 <- cleanDepth(occs = occs, bathy = bath_data)
  expect_false(nrow(occs) == nrow(testcase1))
  testcase2 <- cleanDepth(occs = occs, bathy = bath_data, flag = T)
  expect_true(nrow(occs) == nrow(testcase2))
  testcase3 <- cleanDepth(occs = occs, bathy = bath_data, flag = T, bottom_correct = T)
  expect_true("bottom_correct_values" %in% names(testcase3))
  testcase4 <- cleanDepth(occs = occs, bathy = bath_data, land_poly = land)
  expect_false(nrow(testcase4) == nrow(testcase1))
  testcase5 <- cleanDepth(occs = occs, bathy = bath_data, land_poly = land,
                          depth_range = depth_range,
                          flag = T,
                          bottom_correct = T)
  expect_false(nrow(testcase1) == nrow(testcase5))
}
)

