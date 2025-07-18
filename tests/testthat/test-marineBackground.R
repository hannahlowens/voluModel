# Read occurrences for test
occs <- read.csv(system.file("extdata/Steindachneria_argentea.csv",
                             package='voluModel'))

# Create test occurrences
set.seed(0)
occurrences <- occs[sample(1:nrow(occs),
            size = 24, replace = FALSE),]

test_that("marineBackground input warnings behave as expected", {
  expect_error(marineBackground())
  expect_warning(marineBackground(occs = "a"))
  badColNames <- occurrences
  colnames(badColNames) <- c("eggs", "spam")
  expect_warning(marineBackground(occs = badColNames))
  expect_warning(marineBackground(occs = occurrences, clipToOcean = "a"))
  expect_warning(marineBackground(occs = occurrences, fraction = "a",
                                  partCount = 3, buff = 10000,
                                  initialAlpha = 3, alphaIncrement = 1,
                                  clipToOcean = 'no'))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = "a", buff = 10000,
                                  initialAlpha = 3, alphaIncrement = 1,
                                  clipToOcean = 'no'))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = 3, buff = "a",
                                  initialAlpha = 3, alphaIncrement = 1,
                                  clipToOcean = 'no'))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = 3, buff = 10000,
                                  initialAlpha = "a", alphaIncrement = 1,
                                  clipToOcean = 'no'))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = 3, buff = 10000,
                                  initialAlpha = 3, alphaIncrement = "a",
                                  clipToOcean = 'no'))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = 3, buff = 10000,
                                  initialAlpha = 3, alphaIncrement = 1,
                                  clipToOcean = 1))
  expect_warning(marineBackground(occs = occurrences, fraction = 0.95,
                                  partCount = 3, buff = 10000,
                                  initialAlpha = 3, alphaIncrement = 1,
                                  clipToOcean = 1, verbose = "rabbit"))
})

test_that("marineBackground results as expected", {
  skip_on_cran() # Function's a little slow for CRAN's tastes
#  skip_on_ci() # Something weird happens at line 58
  result <- marineBackground(occs = occurrences, buff = 100000,
                             fraction = .9, partCount = 2, clipToOcean = TRUE)
  expect_true("SpatVector" %in% class(result))

  result <- marineBackground(occs = occurrences,
                             fraction = .9, partCount = 2,
                             clipToOcean = TRUE)
  expect_true("SpatVector" %in% class(result))
})

test_that("marineBackground Pacific results as expected", {
  skip_on_cran() # Function's a little slow for CRAN's tastes
  # Both sides

  # Create test occurrences
  set.seed(0)
  longitude <- c(sample(-180:-175,
                        size = 10, replace = TRUE),
                 sample(165:180,
                        size = 10, replace = TRUE))
  set.seed(0)
  latitude <- sample(-20:20,
                     size = 20, replace = TRUE)
  occurrences <- data.frame(longitude,latitude)

  result <- marineBackground(occs = occurrences, buff = 1000000,
                             fraction = .95, partCount = 2, clipToOcean = TRUE,
                             verbose = FALSE)
  expect_true("SpatVector" %in% class(result))

  #One side
  result <- marineBackground(occs = occurrences[1:10,], buff = 1000000,
                             fraction = .95, partCount = 2, clipToOcean = TRUE,
                             verbose = FALSE)
  expect_true("SpatVector" %in% class(result))
})
