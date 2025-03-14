library(rnaturalearth)
library(ggplot2)
library(terra)

occs <- read.csv(system.file("extdata/Steindachneria_argentea.csv",
                             package='voluModel'))
spName <- "Steindachneria argentea"
land <- ne_countries(scale = "medium", returnclass = "sf")[1]

test_that("input checks", {
  expect_identical(class(occs), "data.frame")
  expect_type(land, "list")
  expect_identical(class(land), c("sf", "data.frame"))
})

test_that("areColors works as expected", {
  expect_error(voluModel:::areColors())
  expect_warning(voluModel:::areColors(NULL))
  expect_true(is.logical(voluModel:::areColors("red")))
})

test_that("pointMap checks", {
  expect_error(pointMap())
  expect_warning(pointMap(occs = "a",
                          spName = spName))
  expect_warning(pointMap(occs = occs[,c(1,3)],
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
  expect_warning(pointMap(occs = occs,
                          spName = spName, land = land,
                          verbose = "banana"))
  point_map <- pointMap(occs = occs, alpha = 1,
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
  expect_warning(pointCompMap(occs1 = occs1[,c(1,3)], occs2 = occs2,
                              spName = spName))
  expect_warning(pointCompMap(occs2 = "a", occs1 = occs1,
                              spName = spName))
  expect_warning(pointCompMap(occs2 = occs2[,c(1,3)], occs1 = occs1,
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
  expect_warning(pointCompMap(occs1 = occs1, occs2 = occs2,
                              spName = spName, land = land,
                              verbose = "banana"))

  badColNames <- occs1
  colnames(badColNames)[1:2] <- c("x", "y")
  expect_warning(pointCompMap(occs1 = badColNames, occs2 = occs2,
                              spName = spName, land = land))

  point_map <- pointCompMap(occs1 = occs1, occs2 = occs2,
                            spName = spName, land = land)
  expect_true(is.ggplot(point_map))
  point_map <- pointCompMap(occs1 = occs1, occs2 = occs2,
                            spName = spName, land = vect(land))
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

test_that("blendColor checks", {
  expect_warning(voluModel:::blendColor(col1 = "eggs"))
  expect_warning(voluModel:::blendColor(col1 = "#1B9E777F",
                                        col2 = "eggs"))
  expect_equal(class(voluModel:::blendColor(col1 = "#1B9E777F",
                                        col2 = "black")), "character")
  expect_equal(class(voluModel:::blendColor(col1 = "#1B9E777F",
                                            col2 = "#7570B37F")), "character")
})

# Set up tests for rasterComp function
rast1 <- rast(ncol=10, nrow=10)
values(rast1) <- rep(0:1, 50)

rast2 <- rast(ncol=10, nrow=10)
values(rast2) <- c(rep(0, 50), rep(1,50))

test_that("rasterComp works", {
  expect_warning(rasterComp(rast1 = "a"))
  expect_warning(rasterComp(rast1 = rast1, rast1Name = 2))
  expect_warning(rasterComp(rast1 = rast1, rast1Name = "First Raster",
                            rast2 = rast2, rast2Name = "Second Raster",
                            graticule = "b"))
  expect_warning(rasterComp(rast1 = rast1, rast1Name = "First Raster",
                            rast2 = rast2, rast2Name = "Second Raster",
                            land = "b"))
  expect_warning(rasterComp(rast1 = rast1, rast1Name = "First Raster",
                            rast2 = rast2, rast2Name = "Second Raster",
                            col1 = "ukulele"))
  expect_equal(class(rasterComp(rast1 = rast1)), "recordedplot")
  expect_equal(class(rasterComp(rast2 = rast2)), "recordedplot")
  expect_equal(class(rasterComp(graticule = FALSE)), "recordedplot")
  expect_equal(class(rasterComp(rast1 = rast1, rast2 = rast2)), "recordedplot")
  expect_equal(class(rasterComp(rast1 = rast1, land = land)), "recordedplot")
  expect_equal(class(rasterComp(rast2 = rast2, land = land)), "recordedplot")
  expect_equal(class(rasterComp(land = land)), "recordedplot")
  expect_equal(class(rasterComp(rast1 = rast1, rast2 = rast2, land = land)),
               "recordedplot")
})

test_that("oneRasterPlot works", {
  divStack <- diversityStack(list(rast1, rast2), template = rast2)
  expect_warning(oneRasterPlot(rast = "a"))
  expect_warning(oneRasterPlot(rast = divStack, land = "a"))
  expect_warning(oneRasterPlot(rast = divStack, graticule = "a"))
  expect_warning(oneRasterPlot(rast = divStack, landCol = "a"))
  expect_warning(oneRasterPlot(rast = divStack, scaleRange = "a"))
  expect_warning(oneRasterPlot(rast = divStack, verbose = "a"))
  expect_warning(oneRasterPlot(rast = divStack, scaleRange = c(1)))
  expect_message(oneRasterPlot(rast = divStack, scaleRange = c(1,10), verbose = TRUE))
  expect_equal(class(oneRasterPlot(rast = divStack)), c("recordedplot"))
  expect_equal(class(oneRasterPlot(rast = divStack,
                                   scaleRange = c(-1, 80))), c("recordedplot"))
  expect_equal(class(oneRasterPlot(divStack, land = land)), c("recordedplot"))
  expect_equal(class(oneRasterPlot(divStack, land = land,
                                   alpha = .5)), c("recordedplot"))
  expect_equal(class(oneRasterPlot(divStack, land = land,
                                   option = "mako")), c("recordedplot"))
  expect_equal(class(oneRasterPlot(divStack, land = land,
                                   plotLegend = TRUE)), c("recordedplot"))
  expect_equal(class(oneRasterPlot(divStack, land = land,
                                   n = 3)), c("recordedplot"))
  expect_equal(class(oneRasterPlot(divStack, land = land,
                                   varName = "Test")), c("recordedplot"))
  expect_equal(class(oneRasterPlot(divStack, land = land,
                                   legendRound = 1)), c("recordedplot"))
})

rast1 <- rast(ncol=10, nrow=10)
values(rast1) <- rep(0:1, 50)

rast2 <- rast(ncol=10, nrow=5)
values(rast2) <- c(rep(0, 25), rep(1,25))
rasterList <- list(rast1, rast2)

test_that("diversityStack works", {
  expect_warning(diversityStack(rasterList = "a"))
  expect_warning(diversityStack(rasterList = list("eggs", "bacon", "spam")))
  expect_warning(diversityStack(rasterList = rasterList, template = "b"))
  divStack <- diversityStack(rasterList = rasterList, template = rast2)
  expect_true(grepl("SpatRaster", class(divStack)))
})

rast1 <- rast(ncol=10, nrow=10)
values(rast1) <- rep(0:1, 50)

rast2 <- rast(ncol=10, nrow=10)
values(rast2) <- c(rep(0, 50), rep(1,50))

rast3 <- rast(ncol=10, nrow=10)
values(rast3) <- rep(c(1,0,0,1), 25)
distBrick <- c(rast1, rast2, rast3)
names(distBrick) <- c(1,2,3)

test_that("plotLayers works", {
  expect_warning(plotLayers(rast = "a"))
  expect_warning(plotLayers(rast = distBrick, land = "a"))
  expect_warning(plotLayers(rast = distBrick, landCol = "bacon"))
  expect_warning(plotLayers(rast = distBrick, graticule = 10))
  expect_equal(class(plotLayers(distBrick)), "recordedplot")
  expect_equal(class(plotLayers(distBrick, land = land)), "recordedplot")
})

rast1 <- rast(ncol=10, nrow=10)
values(rast1) <- rep(0:3, 25)

rast2 <- rast(ncol=10, nrow=10)
values(rast2) <- c(rep(0, 50), rep(1,25), rep(2,25))

rast3 <- rast(ncol=10, nrow=10)
values(rast3) <- rep(c(1,3,2,1), 25)

distBrick <- c(rast1, rast2, rast3)
names(distBrick) <- c(0:2)

test_that("verticalSample works", {
  expect_warning(verticalSample())
  expect_warning(verticalSample(x = distBrick,
                                sampleAxis = "nope"))
  expect_warning(verticalSample(x = distBrick,
                                axisValue = "nope"))
  expect_warning(verticalSample(x = distBrick, axisValue = "nope"))
  expect_warning(verticalSample(x = distBrick, axisValue = 200))
  expect_warning(verticalSample(x = distBrick, sampleAxis = "lat",
                                axisValue = 200))
  sampleResult <- voluModel::verticalSample(distBrick)
  expect_equal(class(sampleResult), "data.frame")
  sampleResult <- voluModel::verticalSample(distBrick, sampleAxis = "lat")
  expect_equal(class(sampleResult), "data.frame")
})

test_that("transectPlot works", {
  expect_warning(transectPlot())
  expect_warning(transectPlot(rast = distBrick,
                                sampleAxis = "nope"))
  expect_warning(transectPlot(rast = distBrick,
                                axisValue = "nope"))
  expect_warning(transectPlot(rast = distBrick, axisValue = "nope"))
  expect_warning(transectPlot(rast = distBrick, axisValue = 200))
  expect_warning(transectPlot(rast = distBrick, sampleAxis = "lat",
                                axisValue = 200))
  distBrickBroken <- distBrick
  names(distBrickBroken) <- c("eggs", "bacon", "spam")
  expect_warning(transectPlot(rast = distBrickBroken, sampleAxis = "lat"))
  expect_warning(transectPlot(rast = distBrick, scaleRange = 1))
  expect_warning(transectPlot(rast = distBrick, scaleRange = 20))
  expect_message(voluModel::transectPlot(distBrick, scaleRange = c(1,2),
                                          verbose = T))
  sampleResult <- voluModel::transectPlot(distBrick, scaleRange = c(1,2),
                                          verbose = T)
  expect_true("ggplot" %in% class(sampleResult))
  sampleResult <- voluModel::transectPlot(distBrick, sampleAxis = "lat",
                                          option = "mako", n = 4,
                                          scaleRange = c(0,10),
                                          legendRound = 0)
  expect_true("ggplot" %in% class(sampleResult))
})

