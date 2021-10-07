library(raster)
library(voluModel)

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

# Here's the function
result <- marineBackground(occs = occurrences, clipToCoast = "no",
                           fraction = .99, partCount = 1)

test_that("marineBackground input warnings behave as expected", {
  expect_error(marineBackground())
  expect_warning(marineBackground(occs = "a"))
  expect_warning(marineBackground(occs = occurrences, fraction = "a",
                                  partCount = 3, buff = 10000,
                                  initialAlpha = 3, alphaIncrement = 1,
                                  clipToCoast = 'no'))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = "a", buff = 10000,
                                  initialAlpha = 3, alphaIncrement = 1,
                                  clipToCoast = 'no'))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = 3, buff = "a",
                                  initialAlpha = 3, alphaIncrement = 1,
                                  clipToCoast = 'no'))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = 3, buff = 10000,
                                  initialAlpha = "a", alphaIncrement = 1,
                                  clipToCoast = 'no'))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = 3, buff = 10000,
                                  initialAlpha = 3, alphaIncrement = "a",
                                  clipToCoast = 'no'))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = 3, buff = 10000,
                                  initialAlpha = 3, alphaIncrement = 1,
                                  clipToCoast = 1))
})
