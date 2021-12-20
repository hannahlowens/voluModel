library(rnaturalearth)
library(raster)
library(ggplot2)

occs <- read.csv(system.file("extdata/Aphanopus_intermedius.csv",
                             package='voluModel'))
spName <- "Aphanopus intermedius"
land <- ne_countries(scale = "medium", returnclass = "sf")[1]

test_that("input checks", {
  expect_identical(class(occs), "data.frame")
  expect_type(land, "list")
  expect_identical(class(land), c("sf", "data.frame"))
})

test_that("areColors works as expected", {
  expect_error(areColors())
  expect_warning(areColors(NULL))
  expect_true(is.logical(areColors("red")))
})

test_that("pointMap checks", {
  expect_error(pointMap())
  expect_warning(pointMap(occs = "a",
                          spName = spName))
  expect_warning(pointMap(occs = occs[,1:2],
                          spName = spName))
  expect_warning(pointMap(occs=occs, spName = 2))
  expect_warning(pointMap(occs = occs,
                          spName = spName, land = "a"))
  expect_warning(pointMap(occs = occs,
                          spName = spName, land = land,
                          ptCol = "eggs"))
  expect_warning(pointMap(occs = occs,
                          spName = spName, land = land,
                          ptSize = "a"))
  point_map <- pointMap(occs = occs,
                        spName = spName, land = land)
  expect_true(is.ggplot(point_map))
  point_map <- pointMap(occs = occs,
                        spName = spName, land = NA)
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
  expect_warning(pointCompMap(occs1=occs1, occs2 = occs2,
                              spName = 2))
  expect_warning(pointCompMap(occs1 = occs1, occs2 = occs2,
                              spName = spName, land = "a"))
  expect_warning(pointCompMap(occs1 = occs1, occs2 = occs2,
                              spName = spName, land = land,
                          occs1Col = "eggs"))
  expect_warning(pointCompMap(occs1 = occs1, occs2 = occs2,
                              spName = spName, land = land,
                          ptSize = "a"))

  badColNames <- occs1
  colnames(badColNames)[1:2] <- c("x", "y")
  expect_warning(pointCompMap(occs1 = badColNames, occs2 = occs2,
                              spName = spName, land = land))

  point_map <- pointCompMap(occs1 = occs1, occs2 = occs2,
                            spName = spName, land = land)
  expect_true(is.ggplot(point_map))
  point_map <- pointCompMap(occs1 = occs1, occs2 = occs2,
                            spName = spName, land = NA)
  expect_true(is.ggplot(point_map))
  point_map <- pointCompMap(occs1 = rbind(occs1,occs2), occs2 = occs2,
                            spName = spName, land = NA)
  expect_true(is.ggplot(point_map))
  point_map <- pointCompMap(occs1 = occs1, occs2 = rbind(occs1,occs2),
                            spName = spName, land = NA)
  expect_true(is.ggplot(point_map))
})

test_that("transpColor checks", {
  expect_error(voluModel:::transpColor())
  expect_warning(voluModel:::transpColor(color = "eggs"))
  expect_warning(voluModel:::transpColor(color = "red",
                                         percent = "blue"))
  expect_warning(voluModel:::transpColor(color = "red",
                                         percent = 200))
  expect_equal(class(voluModel:::transpColor(color = "red",
                                             percent = 20)), "character")
})

# Set up tests for rasterCompFunction
rast1 <- raster(ncol=10, nrow=10)
values(rast1) <- rep(0:1, 50)

rast2 <- raster(ncol=10, nrow=10)
values(rast2) <- c(rep(0, 50), rep(1,50))

test_that("rasterComp works", {
  expect_warning(rasterComp(rast1 = "a"))
  expect_warning(rasterComp(rast1 = rast1, rast1Name = 2))
  expect_warning(rasterComp(rast1 = rast1, rast1Name = "First Raster",
                            rast2 = rast2, rast2Name = "Second Raster",
                            land = "b"))
  expect_warning(rasterComp(rast1 = rast1, rast1Name = "First Raster",
                            rast2 = rast2, rast2Name = "Second Raster",
                            col1 = "ukulele"))
  expect_equal(class(rasterComp(rast1 = rast1)), "trellis")
  expect_equal(class(rasterComp(rast2 = rast2)), "trellis")
  expect_equal(class(rasterComp()), "trellis")
  expect_equal(class(rasterComp(rast1 = rast1, rast2 = rast2)), "trellis")
  expect_equal(class(rasterComp(rast1 = rast1, land = land)), "trellis")
  expect_equal(class(rasterComp(rast2 = rast2, land = land)), "trellis")
  expect_equal(class(rasterComp(land = land)), "trellis")
  expect_equal(class(rasterComp(rast1 = rast1, rast2 = rast2, land = land)),
               "trellis")
})

test_that("diversityStack works", {
  divStack <- diversityStack(list(rast1, rast2), template = rast2)
  expect_true(grepl("Raster*", class(divStack)))
})

test_that("oneRasterPlot works", {
  divStack <- diversityStack(list(rast1, rast2), template = rast2)
  expect_warning(oneRasterPlot(rast = "a"))
  expect_warning(oneRasterPlot(rast = divStack, land = "a"))
  expect_equal(class(oneRasterPlot(divStack)), "trellis")
  expect_equal(class(oneRasterPlot(divStack, land = land)), "trellis")
})

rast1 <- raster(ncol=10, nrow=10)
values(rast1) <- rep(0:1, 50)

rast2 <- raster(ncol=10, nrow=10)
values(rast2) <- c(rep(0, 50), rep(1,50))

rast3 <- raster(ncol=10, nrow=10)
values(rast3) <- rep(c(1,0,0,1), 25)
distBrick <- brick(rast1, rast2, rast3)

test_that("plotLayers works", {
  expect_warning(plotLayers(rast = "a"))
  expect_warning(plotLayers(rast = distBrick, land = "a"))
  expect_equal(class(plotLayers(distBrick)), "trellis")
  expect_equal(class(plotLayers(distBrick, land = land)), "trellis")
})

