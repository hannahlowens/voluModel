library(raster)

test_that("xyzSample input warnings behave as expected", {
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
  expect_error(xyzSample())
  expect_warning(xyzSample(occs = "a", envBrick = rBrick))
  expect_warning(xyzSample(occs = occurrences[,1:2], envBrick = rBrick))
  expect_warning(xyzSample(occs = occurrences, envBrick = "a"))

  colnames(occurrences) <- c("cheese", "eggs", "spam")
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick))

  colnames(occurrences) <- c("longitude", "longitude", "depth")
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick))
  colnames(occurrences) <- c("latitude", "latitude", "depth")
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick))
  colnames(occurrences) <- c("longitude", "depth", "depth")
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick))

  colnames(occurrences) <- c("y", "yum", "X")
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick))
  colnames(occurrences) <- c("x", "exlax", "depth")
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick))
  colnames(occurrences) <- c("z", "z", "sneeze")
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick))

  names(rBrick) <- c("a", "b", "c")
  expect_warning(xyzSample(occs = occurrences, envBrick = rBrick))
})


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
  expect_equal(0, sum(is.na(occSample3d)))
})

test_that("xyzSample column parsing works", {
  # Create sample raster
  r <- raster(ncol=10, nrow=10)
  values(r) <- 1:100

  # Create sample raster brick
  rBrick <- brick(r, r*10, r*100)
  names(rBrick) <- c(0, 10, 30)

  # Create test occurrences
  set.seed(0)
  longitude <- sample(extent(rBrick)[1]:extent(rBrick)[2],
                      size = 10, replace = FALSE)
  set.seed(0)
  latitude <- sample(extent(rBrick)[3]:extent(rBrick)[4],
                     size = 10, replace = FALSE)
  set.seed(0)
  depth <- sample(0:35, size = 10, replace = TRUE)
  occurrences <- as.data.frame(cbind(longitude,latitude,depth))

  # Test reordering
  occurrences <- occurrences[,c(3,1,2)]
  occSample3d <- xyzSample(occurrences, rBrick)
  expect_type(occSample3d, "double")
  expect_length(occSample3d, 10)

  # Test subset selection
  occurrences$"z" <- occurrences$depth
  occurrences$"zebra" <- occurrences$depth
  occSample3d <- xyzSample(occurrences, rBrick)
  expect_type(occSample3d, "double")
  expect_length(occSample3d, 10)
  expect_equal(0, sum(is.na(occSample3d)))
})
