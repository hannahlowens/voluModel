library(raster)

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

test_that("xyzSample input warnings behave as expected", {
  # Tests
  expect_error(xyzSample())
  expect_warning(xyzSample(occs = "a", envBrick = rBrick))
  expect_warning(xyzSample(occs = occurrences[,1:2], envBrick = rBrick))
  expect_warning(xyzSample(occs = occurrences, envBrick = "a"))
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick,
                           verbose = "eggs"))

  colnames(occurrences) <- c("cheese", "eggs", "spam")
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick))
  colnames(occurrences) <- c("latitude", "longitude", "depth")

  names(rBrick) <- c("a", "b", "c")
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick))
  names(rBrick) <- c(0, 10, 100)
})


test_that("xyzSample returns appropriate information", {
  # Test function
  occSample3d <- xyzSample(occurrences, rBrick)

  expect_type(occSample3d, "double")
  expect_length(occSample3d, 10)
  expect_equal(0, sum(is.na(occSample3d)))
})

test_that("xyzSample column parsing works", {
  # Test subset selection
  occurrences$"z" <- occurrences$depth
  occurrences$"zebra" <- occurrences$depth
  occSample3d <- xyzSample(occurrences, rBrick)
  expect_type(occSample3d, "double")
  expect_length(occSample3d, 10)
  expect_equal(0, sum(is.na(occSample3d)))
})
