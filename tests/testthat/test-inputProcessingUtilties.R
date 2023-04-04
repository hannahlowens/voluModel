library(terra)

# voluModel:::columnParse() tests ----
# Create sample raster
r <- rast(ncol=10, nrow=10)
values(r) <- 1:100

# Create sample raster brick
rBrick <- c(r, r*10, r*100)
names(rBrick) <- c(0, 10, 100)

# Create test occurrences
set.seed(0)
longitude <- sample(ext(rBrick)[1]:ext(rBrick)[2],
                    size = 10, replace = FALSE)
set.seed(0)
latitude <- sample(ext(rBrick)[3]:ext(rBrick)[4],
                   size = 10, replace = FALSE)
set.seed(0)
depth <- sample(0:98, size = 10, replace = TRUE)
occurrences <- as.data.frame(cbind(longitude, latitude, depth))

test_that("columnParse input warnings behave as expected", {

  expect_error(voluModel:::columnParse())
  expect_warning(voluModel:::columnParse(occs = "a"))

  # 2D
  expect_warning(voluModel:::columnParse(occs = occurrences[,1]))

  colnames(occurrences) <- c("cheese", "eggs")
  expect_warning(voluModel:::columnParse(occs = occurrences))

  colnames(occurrences) <- c("longitude", "longitude")
  expect_warning(voluModel:::columnParse(occs = occurrences))
  colnames(occurrences) <- c("latitude", "latitude")
  expect_warning(voluModel:::columnParse(occs = occurrences))

  occurrences$extra <- occurrences$longitude
  colnames(occurrences) <- rep("yak", times = length(colnames(occurrences)))
  expect_warning(voluModel:::columnParse(occs = occurrences))
  colnames(occurrences) <- rep("xebu", times = length(colnames(occurrences)))
  expect_warning(voluModel:::columnParse(occs = occurrences))

  # 3D
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))

  colnames(occurrences) <- c("cheese", "eggs", "spam")
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))

  colnames(occurrences) <- c("longitude", "longitude", "depth")
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))
  colnames(occurrences) <- c("latitude", "latitude", "depth")
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))
  colnames(occurrences) <- c("longitude", "depth", "depth")
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))

  colnames(occurrences) <- c("y", "yum", "X")
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))
  colnames(occurrences) <- c("x", "exlax", "depth")
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))
  colnames(occurrences) <- c("z", "z", "sneeze")
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))

  names(rBrick) <- c("a", "b", "c")
  expect_warning(voluModel:::columnParse(occs = occurrences, wDepth = TRUE))
})

result2D <- voluModel:::columnParse(occs = occurrences)
result3D <- voluModel:::columnParse(occs = occurrences, wDepth = TRUE)

test_that("columnParse outputs as expected", {
  # 2D
  expect_equal(length(result2D), 3)
  expect_true(is.numeric(result2D$xIndex))
  expect_true(is.numeric(result2D$yIndex))
  expect_true(is.character(result2D$reportMessage))

  # 3D
  expect_equal(length(result3D), 4)
  expect_true(is.numeric(result3D$xIndex))
  expect_true(is.numeric(result3D$yIndex))
  expect_true(is.numeric(result3D$zIndex))
  expect_true(is.character(result3D$reportMessage))
})

# downsample() tests ----
# Create sample raster
r <- rast(ncol=5, nrow=5)
values(r) <- 1:25

# Create test occurrences
set.seed(0)
longitude <- sample(ext(r)[1]:ext(r)[2],
                    size = 5, replace = TRUE)
set.seed(0)
latitude <- sample(ext(r)[3]:ext(r)[4],
                   size = 5, replace = TRUE)
occurrences <- as.data.frame(cbind(longitude,latitude))

test_that("downsample input warnings behave as expected", {
  expect_error(downsample())
  expect_warning(downsample(occs = "a", rasterTemplate = r))
  expect_warning(downsample(occs = occurrences[,1], rasterTemplate = r))
  expect_warning(downsample(occs = occurrences, rasterTemplate = "a"))
  expect_warning(downsample(occs = occurrences, rasterTemplate = r, verbose = "potato"))

  colnames(occurrences) <- c("spam", "eggs")
  expect_warning(downsample(occs = occurrences, rasterTemplate = r))
})

# Here's the function
result <- downsample(occs = occurrences, rasterTemplate = r)

test_that("downsample outputs as expected", {
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 2)
  expect_true(nrow(result) == 4)
})

test_that("downsample special case checks", {
  result <- downsample(occs = occurrences[1,], rasterTemplate = r)
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 2)
  expect_true(nrow(result) == 1)
})

# centerPointRasterTemplate() tests ----

# Create point grid
coords <- data.frame(x = rep(seq(1:5), times = 5),
                     y = unlist(lapply(1:5, FUN = function(x) {
                       rep(x, times = 5)})))

# Create data and add NAs to simulate uneven bottom depths
dd <- data.frame(SURFACE = 1:25,
                 d5M = 6:30,
                 d10M = 11:35,
                 d25M = 16:40)
dd$d25M[c(1:5, 18:25)] <- NA
dd$d10M[c(3:4, 21:23)] <- NA
dd$d5M[c(4, 15, 22)] <- NA
dd <- cbind(coords, dd)

# Create SpatVector
sp <- vect(dd, geom = c("x", "y"))

test_that("centerPointRasterTemplate input warnings behave as expected", {
  expect_error(centerPointRasterTemplate())
  expect_warning(centerPointRasterTemplate(rawPointData = "a"))
})

test_that("centerPointRasterTemplate outputs as expected", {
  result <- centerPointRasterTemplate(rawPointData = sp)
  expect_true(class(result) == "SpatRaster")
  expect_warning(values(result))
  expect_true(suppressWarnings(sum(is.na(values(result)))) == 25)
})

# bottomRaster() tests ----
test_that("bottomRaster input warnings behave as expected", {
  expect_error(bottomRaster())
  expect_warning(bottomRaster(rawPointData = "a"))
})

result <- bottomRaster(rawPointData = sp)

test_that("bottomRaster outputs as expected", {
  expect_true(class(result) == "SpatRaster")
  expect_true(global(result, "isNA") == 0)
  expect_true(result[2] == 22)
})
