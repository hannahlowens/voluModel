library(raster)
library(rgeos)
library(sp)

# occCellRemoval() tests ----
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

test_that("occCellRemoval behaves as expected", {
  expect_error(voluModel:::occCellRemoval())

  testResult <- voluModel:::occCellRemoval(occurrences, r)
  expect_equal(class(testResult)[[1]], "RasterLayer")
  expect_length(testResult, 100)

  naCount <- length(testResult[is.na(testResult)])
  expect_equal(10, naCount)
})

#mSampling2d() tests ----
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
test_that("mSampling2D input warnings behave as expected", {
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
  expect_error(mSampling2D())

  testResult <- mSampling2D(occurrences, r, mShp = mShp)
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 4)
})

# mSampling3D() tests ----
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
set.seed(0)
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

test_that("mSampling3D input warnings behave as expected", {
  expect_error(mSampling3D())
  expect_warning(mSampling3D(occs = "a", envBrick = rBrick, mShp = mShp))
  expect_warning(mSampling3D(occs = occurrences[,1:2], envBrick = rBrick))
  expect_warning(mSampling3D(occs = occurrences, envBrick = "a"))

  colnames(occurrences) <- c("cheese", "eggs", "spam")
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp))

  colnames(occurrences) <- c("longitude", "longitude", "depth")
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp))
  colnames(occurrences) <- c("latitude", "latitude", "depth")
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp))
  colnames(occurrences) <- c("longitude", "depth", "depth")
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp))

  colnames(occurrences) <- c("y", "yum", "X")
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp))
  colnames(occurrences) <- c("x", "exlax", "depth")
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp))
  colnames(occurrences) <- c("z", "zebra", "sneeze")
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp))

  names(rBrick) <- c("a", "b", "c", "d")
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp))
  names(rBrick) <- c(0, 10, 100, 1000)

  colnames(occurrences) <- c("longitude", "longitude", "depth")
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = "cheese"))

  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp, depthLimit = rBrick))
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp, depthLimit = c(0,1,100)))
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick,  mShp = mShp, depthLimit = "cheese"))
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick,  mShp = mShp, depthLimit = c("all", "occs")))

})

test_that("mSampling3D outputs as expected", {
  # Testing
  expect_error(mSampling3D())

  testResult <- mSampling3D(occurrences, rBrick, mShp = mShp)
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 40)

  testResult <- mSampling3D(occurrences, rBrick, mShp = mShp, depthLimit = "occs")
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 28)

  testResult <- mSampling3D(occurrences, rBrick, mShp = mShp, depthLimit = "all")
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 40)

  testResult <- mSampling3D(occurrences, rBrick, mShp = mShp, depthLimit = c(5, 25))
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 20)
})
