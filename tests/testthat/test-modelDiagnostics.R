library(raster)
library(dplyr)

# Create first rasterBrick
r1 <- raster(ncol=10, nrow=10)
values(r1) <- 1:100
r2 <- raster(ncol=10, nrow=10)
values(r2) <- c(rep(20, times = 50), rep(60, times = 50))
r3 <- raster(ncol=10, nrow=10)
values(r3) <- 8
envBrick1 <- brick(r1, r2, r3)
names(envBrick1) <- c(0, 10, 30)

# Create second rasterBrick
r1 <- raster(ncol=10, nrow=10)
values(r1) <- 100:1
r2 <- raster(ncol=10, nrow=10)
values(r2) <- c(rep(10, times = 50), rep(20, times = 50))
r3 <- raster(ncol=10, nrow=10)
values(r3) <- rep(c(10,20,30,25), times = 25)
envBrick2 <- brick(r1, r2, r3)
names(envBrick2) <- c(0, 10, 30)

rastList <- list("temperature" = envBrick1, "salinity" = envBrick2)

# Create test occurrences
set.seed(0)
longitude <- sample(extent(envBrick1)[1]:extent(envBrick1)[2],
                    size = 10, replace = FALSE)
set.seed(0)
latitude <- sample(extent(envBrick1)[3]:extent(envBrick1)[4],
                   size = 10, replace = FALSE)
set.seed(0)
depth <- sample(0:35, size = 10, replace = TRUE)
occurrences <- as.data.frame(cbind(longitude,latitude,depth))

# Calibration
calibration <- lapply(rastList, FUN = function(x) xyzSample(occurrences, x))  %>% bind_rows

test_that("MESS3D warnings work", {
  expect_error(MESS3D())
  expect_warning(MESS3D(calibration = "a", projection = rastList))
  expect_warning(MESS3D(calibration = calibration, projection = NULL))
  names(rastList) <- c("depecheMode", "fleetwoodMac")
  expect_warning(MESS3D(calibration = calibration, projection = rastList))
})

rastList <- list("temperature" = envBrick1, "salinity" = envBrick2)

test_that("MESS3D outputs as expected", {
  temporary <- MESS3D(calibration = calibration, projection = rastList)
  expect_equal(class(temporary)[[1]], "RasterStack")
})
