library(raster)
library(rgeos)
library(sp)

test_that("occCellRemoval behaves as expected", {
  # Create sample raster
  r <- raster(ncol=10, nrow=10)
  values(r) <- 1:100

  # Create test occurrences
  set.seed(10)
  longitude <- sample(extent(r)[1]:extent(r)[2],
                      size = 10, replace = FALSE)
  set.seed(0)
  latitude <- sample(extent(r)[3]:extent(r)[4],
                     size = 10, replace = FALSE)
  occurrences <- as.data.frame(cbind(longitude,latitude))

  # Testing
  expect_error(voluModel:::occCellRemoval())

  testResult <- voluModel:::occCellRemoval(occurrences, r)
  expect_equal(class(testResult)[[1]], "RasterLayer")
  expect_length(testResult, 100)

  naCount <- length(testResult[is.na(testResult)])
  expect_equal(10, naCount)
})

test_that("mSampling2D input warnings behave as expected", {
  # Create sample raster
  r <- raster(ncol=10, nrow=10)
  values(r) <- 1:100

  # Create test occurrences
  set.seed(0)
  longitude <- sample(extent(r)[1]:extent(r)[2],
                      size = 10, replace = FALSE)
  set.seed(0)
  latitude <- sample(extent(r)[3]:extent(r)[4],
                     size = 10, replace = FALSE)
  occurrences <- as.data.frame(cbind(longitude,latitude))

  # Generate background sampling buffer
  buffPts <- SpatialPoints(occurrences[,c("longitude", "latitude")])
  crs(buffPts) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  mShp <- buffer(buffPts,
                 width = 600000, dissolve = TRUE)

  # Tests
  expect_error(mSampling2D())
  expect_warning(mSampling2D(occs = "a", rasterTemplate = r, mShp = mShp))
  expect_warning(mSampling2D(occs = occurrences[,1], rasterTemplate = r, mShp = mShp))
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = "a", mShp = mShp))
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = r, mShp = "a"))

  colnames(occurrences) <- c("cheese", "eggs")
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = r, mShp = mShp))

  colnames(occurrences) <- c("longitude", "longitude")
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = r, mShp = mShp))
  colnames(occurrences) <- c("latitude", "latitude")
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = r, mShp = mShp))

  occurrences$extra <- occurrences$longitude
  colnames(occurrences) <- rep("yak", times = length(colnames(occurrences)))
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = r, mShp = mShp))
  colnames(occurrences) <- rep("xebu", times = length(colnames(occurrences)))
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = r, mShp = mShp))
})

test_that("mSampling2D outputs as expected", {
  # Create sample raster
  r <- raster(ncol=10, nrow=10)
  values(r) <- 1:100

  # Create test occurrences
  set.seed(0)
  longitude <- sample(extent(r)[1]:extent(r)[2],
                      size = 10, replace = FALSE)
  set.seed(0)
  latitude <- sample(extent(r)[3]:extent(r)[4],
                     size = 10, replace = FALSE)
  occurrences <- as.data.frame(cbind(longitude,latitude))

  # Generate background sampling buffer
  buffPts <- SpatialPoints(occurrences[,c("longitude", "latitude")])
  crs(buffPts) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  mShp <- buffer(buffPts,
                 width = 1000000, dissolve = TRUE)

  # Testing
  expect_error(mSampling2D())

  testResult <- mSampling2D(occurrences, r, mShp = mShp)
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 4)
})

test_that("mSampling3D input warnings behave as expected", {
  # Create sample raster
  r <- raster(ncol=10, nrow=10)
  values(r) <- 1:100

  # Create sample raster brick
  rBrick <- brick(r, r*10, r*100)
  names(rBrick) <- c(0, 10, 100)

  # Create test occurrences
  set.seed(0)
  longitude <- sample(extent(rBrick)[1]:extent(rBrick)[2],
                      size = 10, replace = FALSE)
  set.seed(0)
  latitude <- sample(extent(rBrick)[3]:extent(rBrick)[4],
                     size = 10, replace = FALSE)
  set.seed(0)
  depth <- sample(0:98, size = 10, replace = TRUE)
  occurrences <- as.data.frame(cbind(longitude,latitude,depth))

  # Tests
  expect_error(mSampling3D())
  expect_warning(mSampling3D(occs = "a", envBrick = rBrick))
  expect_warning(mSampling3D(occs = occurrences[,1:2], envBrick = rBrick))
  expect_warning(mSampling3D(occs = occurrences, envBrick = "a"))

  colnames(occurrences) <- c("cheese", "eggs", "spam")
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick))

  colnames(occurrences) <- c("longitude", "longitude", "depth")
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick))
  colnames(occurrences) <- c("latitude", "latitude", "depth")
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick))
  colnames(occurrences) <- c("longitude", "depth", "depth")
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick))

  colnames(occurrences) <- c("y", "yum", "X")
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick))
  colnames(occurrences) <- c("x", "exlax", "depth")
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick))
  colnames(occurrences) <- c("z", "z", "sneeze")
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick))

  names(rBrick) <- c("a", "b", "c")
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick))
})

test_that("mSampling3D outputs as expected", {
  # Create sample raster
  r <- raster(ncol=10, nrow=10)
  values(r) <- 1:100

  # Create sample raster brick
  rBrick <- brick(r, r*10, r*100, r*1000)
  names(rBrick) <- c(0, 10, 100, 1000)

  # Create test occurrences
  set.seed(0)
  longitude <- sample(extent(rBrick)[1]:extent(rBrick)[2],
                      size = 10, replace = FALSE)
  set.seed(10)
  latitude <- sample(extent(rBrick)[3]:extent(rBrick)[4],
                     size = 10, replace = FALSE)
  set.seed(0)
  depth <- sample(0:98, size = 10, replace = TRUE)
  occurrences <- as.data.frame(cbind(longitude,latitude,depth))

  # Generate background sampling buffer
  buffPts <- SpatialPoints(occurrences[,c("longitude", "latitude")])
  crs(buffPts) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  mShp <- buffer(buffPts,
                 width = 1000000, dissolve = TRUE)

  # Testing
  expect_error(mSampling3D())

  testResult <- mSampling3D(occurrences, rBrick, mShp = mShp)
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 36)
})
