library(terra)
# Create test brick
r1 <- rast(ncol=10, nrow=10)
values(r1) <- 1:100
r2 <- rast(ncol=10, nrow=10)
values(r2) <- c(rep(20, times = 50), rep(60, times = 50))
r3 <- rast(ncol=10, nrow=10)
values(r3) <- 8
rBrick <- c(r1, r2, r3)
names(rBrick) <- c(0, 10, 100)

# Create sample raster
r <- r1

# Create test occurrences
set.seed(1)
longitude <- sample(ext(r)[1]:ext(r)[2],
                    size = 10, replace = FALSE)
set.seed(0)
latitude <- sample(ext(r)[3]:ext(r)[4],
                   size = 10, replace = FALSE)

set.seed(0)
depth <- sample(0:35, size = 10, replace = TRUE)

occurrences <- data.frame(longitude, latitude, depth)

# Generate background sampling buffer
buffPts <- vect(occurrences,
                c("longitude", "latitude"))
crs(buffPts) <- crs(r)
mShp <- aggregate(buffer(buffPts, width = 1000000))

# occCellRemoval() tests ----
test_that("occCellRemoval behaves as expected", {
  expect_error(voluModel:::occCellRemoval())

  testResult <- voluModel:::occCellRemoval(occurrences, r)
  expect_equal(class(testResult)[[1]], "SpatRaster")
  expect_length(values(testResult), 100)

  naCount <- length(testResult[is.na(testResult)])
  expect_equal(9, naCount)
})

#mSampling2d() tests ----
test_that("mSampling2D input warnings behave as expected", {
  expect_error(mSampling2D())
  expect_warning(mSampling2D(occs = "a", rasterTemplate = r,
                             mShp = mShp))
  expect_warning(mSampling2D(occs = occurrences[,1], rasterTemplate = r,
                             mShp = mShp))
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = "a",
                             mShp = mShp))
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = r,
                             mShp = "a"))
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = r,
                             mShp = mShp, verbose = "dragon"))

  colnames(occurrences) <- c("spam", "eggs")
  expect_warning(mSampling2D(occs = occurrences, rasterTemplate = r,
                             mShp = mShp))
})

test_that("mSampling2D outputs as expected", {
  expect_error(mSampling2D())

  testResult <- mSampling2D(occurrences, r, mShp = mShp)
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 25)
})

# mSampling3D() tests ----
test_that("mSampling3D input warnings behave as expected", {
  expect_error(mSampling3D())
  expect_warning(mSampling3D(occs = "a", envBrick = rBrick, mShp = mShp))
  expect_warning(mSampling3D(occs = occurrences[,1:2], envBrick = rBrick))
  expect_warning(mSampling3D(occs = occurrences[,1], envBrick = rBrick))
  expect_warning(mSampling3D(occs = occurrences, envBrick = "a"))
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick,
                             mShp = mShp, verbose = "dragon"))

  colnames(occurrences) <- c("spam", "eggs", "cheese")
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick,
                             mShp = mShp))
  colnames(occurrences) <- c("longitude", "latitude", "depth")

  names(rBrick) <- c("a", "b", "c")
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick,
                             mShp = mShp))

  names(rBrick) <- c(0, 10, 100)
  expect_warning(mSampling3D(occs = occurrences, envBrick = rBrick,
                             mShp = "cheese"))

  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp,
                             depthLimit = rBrick))
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick, mShp = mShp,
                             depthLimit = c(0,1,100)))
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick,  mShp = mShp,
                             depthLimit = "cheese"))
  expect_warning(mSampling3D(occs = occurrences,
                             envBrick = rBrick,  mShp = mShp,
                             depthLimit = c("all", "occs")))

})

test_that("mSampling3D outputs as expected", {
  # Testing
  testResult <- mSampling3D(occurrences, rBrick, mShp = mShp,
                            verbose = FALSE)
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 93)

  testResult <- mSampling3D(occurrences, rBrick, mShp = mShp,
                            depthLimit = "occs", verbose = FALSE)
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 59)

  testResult <- mSampling3D(occurrences, rBrick, mShp = mShp,
                            depthLimit = "all", verbose = FALSE)
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 93)

  testResult <- mSampling3D(occurrences, rBrick, mShp = mShp,
                            depthLimit = c(5, 25), verbose = FALSE)
  expect_true(is.data.frame(testResult))
  expect_equal(nrow(testResult), 59)
})
