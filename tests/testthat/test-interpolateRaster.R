library(raster)
library(fields)

# interpolateRaster() tests ----
# Create sample raster
r <- raster(ncol=10, nrow=10)
values(r) <- 1:100

completeRaster <- r

test_that("interpolateRaster input warnings behave as expected", {

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

# Introduce a "hole"
values(r)[c(12:13, 30)] <- NA

test_that("interpolateRaster returns appropriate information", {
 # Execute function
  interpolatedRaster <- interpolateRaster(r)
  expect_equal(class(r), class(interpolatedRaster))
})

test_that("interpolateRaster interpolates as expected", {
  # Execute function
  interpolatedRaster <- interpolateRaster(r)

  # Examine results
  expect_equal(cellStats(is.na(interpolatedRaster), sum), 0)
  expect_equal(cellStats(completeRaster, stat = max),
               cellStats(interpolatedRaster, stat = max))
  expect_equal(cellStats(completeRaster, stat = min),
               cellStats(interpolatedRaster, stat = min))
})

# Create sample raster
r <- raster(ncol=100, nrow=100)
values(r) <- 1:10000
completeRaster <- r

# Introduce a "hole"
values(r)[100:105] <- NA

# Execute function
interpolatedRaster <- interpolateRaster(r, fast = T, aRange = 1)

test_that("fast version of interpolateRaster returns appropriate information", {
  expect_equal(class(r), class(interpolatedRaster))
})

test_that("fast version of interpolateRaster interpolates as expected", {
  # Examine results
  expect_equal(cellStats(is.na(interpolatedRaster), sum), 0)
  expect_equal(cellStats(completeRaster, stat = max),
               cellStats(interpolatedRaster, stat = max))
  expect_equal(cellStats(completeRaster, stat = min),
               cellStats(interpolatedRaster, stat = min))
})

# smoothRaster() tests ----
# Create sample raster
r <- raster(ncol=100, nrow=100)
values(r) <- 1:10000

# Introduce a "bubble"
values(r)[820:825] <- 9999

test_that("smoothRaster input warnings behave as expected", {

  # Tests
  expect_error(smoothRaster())
  expect_warning(smoothRaster(inputRaster = "a"))
  expect_warning(smoothRaster(inputRaster = r, fast = "a"))
  expect_warning(smoothRaster(inputRaster = r, fast = F,
                                   lon.lat = "a"))
  expect_warning(smoothRaster(inputRaster = r, fast = T))
  expect_warning(smoothRaster(inputRaster = r, fast = T,
                                   aRange = "a"))
  expect_warning(smoothRaster(inputRaster = r, fast = T,
                                   theta = "a"))
  expect_warning(smoothRaster(inputRaster = r, fast = T,
                                   REML = "a"))
})

# Execute function
smoothedRaster <- smoothRaster(r)

test_that("smoothRaster returns appropriate information", {
  expect_equal(class(r), class(smoothedRaster))
})

test_that("smoothRaster interpolates as expected", {
  # Examine results
  expect_equal(cellStats(is.na(smoothedRaster), sum), 0)
  expect_equal(cellStats(r, stat = max),
               round(cellStats(smoothedRaster, stat = max), digits = 0))
})

# Create sample raster
r <- raster(ncol=100, nrow=100)
values(r) <- 1:10000
completeRaster <- r

# Introduce a "hole"
values(r)[100:105] <- 200

# Execute function
smoothedRaster <- smoothRaster(r, fast = T, aRange = 1)

test_that("fast version of smoothRaster returns appropriate information", {
  expect_equal(class(r), class(smoothedRaster))
})

test_that("fast version of smoothRaster interpolates as expected", {
  # Examine results
  expect_equal(cellStats(is.na(smoothedRaster), sum), 0)
  expect_equal(cellStats(smoothedRaster, stat = max),
               cellStats(smoothedRaster, stat = max))
  expect_equal(cellStats(smoothedRaster, stat = min),
               cellStats(smoothedRaster, stat = min))
})
