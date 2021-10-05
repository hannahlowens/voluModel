# downsample() tests ----

# Create sample raster
r <- raster(ncol=10, nrow=10)
values(r) <- 1:100

# Create test occurrences
set.seed(0)
longitude <- sample(extent(r)[1]:extent(r)[2],
                    size = 20, replace = TRUE)
set.seed(0)
latitude <- sample(extent(r)[3]:extent(r)[4],
                   size = 20, replace = TRUE)
occurrences <- as.data.frame(cbind(longitude,latitude))

test_that("downsample input warnings behave as expected", {
  expect_error(downsample())
  expect_warning(downsample(occs = "a", rasterTemplate = r))
  expect_warning(downsample(occs = occurrences[,1], rasterTemplate = r))
  expect_warning(downsample(occs = occurrences, rasterTemplate = "a"))

  colnames(occurrences) <- c("cheese", "eggs")
  expect_warning(downsample(occs = occurrences, rasterTemplate = r))

  colnames(occurrences) <- c("longitude", "longitude")
  expect_warning(downsample(occs = occurrences, rasterTemplate = r))
  colnames(occurrences) <- c("latitude", "latitude")
  expect_warning(downsample(occs = occurrences, rasterTemplate = r))

  occurrences$extra <- occurrences$longitude
  colnames(occurrences) <- rep("yak", times = length(colnames(occurrences)))
  expect_warning(downsample(occs = occurrences, rasterTemplate = r))
  colnames(occurrences) <- rep("xebu", times = length(colnames(occurrences)))
  expect_warning(downsample(occs = occurrences, rasterTemplate = r))
})

# Here's the function
result <- downsample(occs = occurrences, rasterTemplate = r)

# bottomRaster() tests ----

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
dd$d10m[c(3:5, 21:23)] <- NA
dd$d5M[c(4, 22)] <- NA

# Create SpatialPointsDataFrame
sp <- SpatialPointsDataFrame(coords = coords,
                             data = dd)
