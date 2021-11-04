library(rnaturalearth)
occs <- read.csv(system.file("extdata/Aphanopus_intermedius.csv", package='voluModel'))
spName <- "Aphanopus intermedius"
land <- ne_countries(scale = "medium", returnclass = "sf")[1]

test_that("input checks", {
  expect_identical(class(occs), "data.frame")
  expect_type(land, "list")
  expect_identical(class(land), c("sf", "data.frame"))
})

test_that("pointMap checks", {
  expect_error(pointMap())
  expect_warning(pointMap(occs = "a", spName = spName))
  expect_warning(pointMap(occs = occs[,1:2], spName = spName))
  expect_warning(pointMap(occs=occs, spName = 2))
  expect_warning(pointMap(occs = occs, spName = spName, land = "a"))
  expect_warning(pointMap(occs = occs, spName = spName, land = land,
                          ptCol = "eggs"))
  expect_warning(pointMap(occs = occs, spName = spName, land = land,
                          ptSize = "a"))
  point_map <- pointMap(occs = occs, spName = spName, land = land)
  expect_true(is.ggplot(point_map))
  point_map <- pointMap(occs = occs, spName = spName, land = NA)
  expect_true(is.ggplot(point_map))
})

set.seed(0)
occs1 <- occs[sample(1:nrow(occs),
                size = 24, replace = FALSE),]

set.seed(10)
occs2 <- occs[sample(1:nrow(occs),
                size = 24, replace = FALSE),]

test_that("pointCompMap checks", {
  expect_error(pointCompMap())
  expect_warning(pointCompMap(occs1 = "a", occs2 = occs2,
                              spName = spName))
  expect_warning(pointCompMap(occs1 = occs1[,1:2], occs2 = occs2,
                              spName = spName))
  expect_warning(pointCompMap(occs2 = "a", occs1 = occs1,
                              spName = spName))
  expect_warning(pointCompMap(occs2 = occs2[,1:2], occs1 = occs1,
                              spName = spName))
  expect_warning(pointCompMap(occs1=occs1, occs2 = occs2, spName = 2))
  expect_warning(pointCompMap(occs1 = occs1, occs2 = occs2, spName = spName, land = "a"))
  expect_warning(pointCompMap(occs1 = occs1, occs2 = occs2, spName = spName, land = land,
                          occs1Col = "eggs"))
  expect_warning(pointCompMap(occs1 = occs1, occs2 = occs2, spName = spName, land = land,
                          ptSize = "a"))

  badColNames <- occs1
  colnames(badColNames)[1:2] <- c("x", "y")
  expect_warning(pointCompMap(occs1 = badColNames, occs2 = occs2, spName = spName, land = land))

  point_map <- pointCompMap(occs1 = occs1, occs2 = occs2, spName = spName, land = land)
  expect_true(is.ggplot(point_map))
  point_map <- pointCompMap(occs1 = occs1, occs2 = occs2, spName = spName, land = NA)
  expect_true(is.ggplot(point_map))
  point_map <- pointCompMap(occs1 = rbind(occs1,occs2), occs2 = occs2, spName = spName, land = NA)
  expect_true(is.ggplot(point_map))
  point_map <- pointCompMap(occs1 = occs1, occs2 = rbind(occs1,occs2), spName = spName, land = NA)
  expect_true(is.ggplot(point_map))
})

test_that("transpCol checks", {
  expect_error(voluModel:::transpColor())
  expect_warning(voluModel:::transpColor(color = "eggs"))
  expect_warning(voluModel:::transpColor(color = "red", percent = "blue"))
  expect_warning(voluModel:::transpColor(color = "red", percent = 200))
  expect_equal(class(voluModel:::transpColor(color = "red", percent = 20)), "character")
})

