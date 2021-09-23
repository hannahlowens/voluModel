library(raster)

test_that("xyzSample returns appropriate information", {
  # Create test raster
  r1 <- raster(ncol=10, nrow=10)
  values(r1) <- 1:100
  r2 <- raster(ncol=10, nrow=10)
  values(r2) <- c(rep(20, times = 50), rep(60, times = 50))
  r3 <- raster(ncol=10, nrow=10)
  values(r3) <- 8
  envBrick <- brick(r1, r2, r3)
  names(envBrick) <- c(0, 10, 30)

  # Create test occurrences
  set.seed(0)
  longitude <- sample(extent(envBrick)[1]:extent(envBrick)[2],
                      size = 10, replace = F)
  set.seed(0)
  latitude <- sample(extent(envBrick)[3]:extent(envBrick)[4],
                     size = 10, replace = F)
  set.seed(0)
  depth <- sample(0:35,
                  size = 10, replace = T)
  occurrences <- as.data.frame(cbind(longitude,latitude,depth))

  # Test function
  occSample3d <- xyzSample(occurrences, envBrick)

  expect_type(occSample3d, "double")
  expect_length(occSample3d, 10)
})
