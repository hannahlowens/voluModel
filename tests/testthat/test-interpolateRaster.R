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
