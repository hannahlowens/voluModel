library(raster)
library(fields)

test_that("interpolateRaster input warnings behave as expected", {
  # Create sample raster
  r <- raster(ncol=10, nrow=10)
  values(r) <- 1:100

  # Tests
  expect_error(interpolateRaster())
  expect_warning(interpolateRaster(inputRaster = "a"))
  expect_warning(interpolateRaster(inputRaster = r, fast = "a"))
  expect_warning(interpolateRaster(inputRaster = r, fast = F,
                                lon.lat = "a"))
  expect_warning(interpolateRaster(inputRaster = r, fast = T))
  expect_warning(interpolateRaster(inputRaster = r, fast = T,
                                aRange = "a"))
  expect_warning(interpolateRaster(inputRaster = r, fast = T,
                                   theta = "a"))
  expect_warning(interpolateRaster(inputRaster = r, fast = T,
                                   REML = "a"))
})

test_that("interpolateRaster returns appropriate information", {
  # Create sample raster
  r <- raster(ncol=10, nrow=10)
  values(r) <- 1:100

  # Introduce a "hole"
  values(r)[67:70] <- NA

  # Execute function
  interpolatedRaster <- interpolateRaster(r)
  expect_equal(class(r), class(interpolatedRaster))
})

test_that("interpolateRaster interpolates as expected", {

  # Create sample raster
  r <- raster(ncol=10, nrow=10)
  values(r) <- 1:100
  completeRaster <- r

  # Introduce a "hole"
  values(r)[67:70] <- NA

  # Execute function
  interpolatedRaster <- interpolateRaster(r)

  # Examine results
  expect_equal(cellStats(is.na(interpolatedRaster), sum), 0)
  expect_equal(cellStats(completeRaster, stat = max),
               cellStats(interpolatedRaster, stat = max))
  expect_equal(cellStats(completeRaster, stat = min),
               cellStats(interpolatedRaster, stat = min))
})

test_that("fast version of interpolateRaster returns appropriate information", {
  # Create sample raster
  r <- raster(ncol=100, nrow=100)
  values(r) <- 1:10000
  completeRaster <- r

  # Introduce a "hole"
  values(r)[100:105] <- NA

  # Execute function
  interpolatedRaster <- interpolateRaster(r, fast = T, aRange = 1)

  expect_equal(class(r), class(interpolatedRaster))
})

test_that("fast version of interpolateRaster interpolates as expected", {

  # Create sample raster
  r <- raster(ncol=100, nrow=100)
  values(r) <- 1:10000
  completeRaster <- r

  # Introduce a "hole"
  values(r)[100:105] <- NA

  # Execute function
  interpolatedRaster <- interpolateRaster(r, fast = T, aRange = 1)

  # Examine results
  expect_equal(cellStats(is.na(interpolatedRaster), sum), 0)
  expect_equal(cellStats(completeRaster, stat = max),
               cellStats(interpolatedRaster, stat = max))
  expect_equal(cellStats(completeRaster, stat = min),
               cellStats(interpolatedRaster, stat = min))
})
