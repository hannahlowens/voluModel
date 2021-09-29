library(raster)
library(rgeos)
library(sp)

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
