library(terra)
library(fields)

# interpolateRaster() tests ----
# Create sample raster
r <- rast(ncol=10, nrow=10)
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
  expect_warning(interpolateRaster(inputRaster = r, fast = T,
                                   REML = T, method = "guess"))
})

# Introduce a "hole"
values(r)[c(45:47, 55:57)] <- NA

test_that("interpolateRaster returns appropriate information", {
 # Execute function
  interpolatedRaster <- interpolateRaster(r)
  expect_equal(class(r), class(interpolatedRaster))
})

test_that("interpolateRaster interpolates as expected", {
  # Execute function
  interpolatedRaster <- interpolateRaster(r)

  # Examine results
  expect_true(global(interpolatedRaster, "isNA") == 0)
  expect_equal(global(completeRaster, max),
               global(interpolatedRaster, max))
  expect_equal(global(completeRaster, min),
               global(interpolatedRaster, min))
})

# Create sample raster
r <- rast(ncol=100, nrow=100)
values(r) <- rep(1:100, times = 100)
completeRaster <- r

# Introduce a "hole"
values(r)[c(5030:5040)] <- NA

# Execute function
interpolatedRaster <- interpolateRaster(r, fast = T, aRange = 1)

test_that("fast version of interpolateRaster returns appropriate information", {
  expect_equal(class(r), class(interpolatedRaster))
})

test_that("fast version of interpolateRaster interpolates as expected", {
  # Examine results
  expect_true(global(completeRaster, "isNA") == 0)
  expect_equal(global(completeRaster, max),
               global(interpolatedRaster, max))
  expect_equal(global(completeRaster, min),
               global(interpolatedRaster, min))
})

# smoothRaster() tests ----
# Introduce a "bubble"
values(r)[c(5030:5050,5130:5150)] <- 99

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
  expect_warning(smoothRaster(inputRaster = r, fast = T,
                              REML = T, method = "guess"))
})

# Execute function
smoothedRaster <- smoothRaster(r, method = "REML")

test_that("smoothRaster returns appropriate information", {
  expect_equal(class(r), class(smoothedRaster))
})

test_that("smoothRaster interpolates as expected", {
  # Examine results
  expect_true(global(smoothedRaster, "isNA") == 0)
  expect_equal(global(r, max),
               round(global(smoothedRaster, max), digits = 0))
})

# Create sample raster
r <- rast(ncol=100, nrow=100)
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
  expect_true(global(smoothedRaster, "isNA") == 0)
  expect_equal(global(smoothedRaster, max),
               global(smoothedRaster, max))
  expect_equal(global(smoothedRaster, min),
               global(smoothedRaster, min))
})

