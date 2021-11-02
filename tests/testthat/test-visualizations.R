library(rnaturalearth)
occs <- read.csv(system.file("extdata/Aphanopus_intermedius.csv", package='voluModel'))
spName <- "Aphanopus intermedius"
land <- ne_countries(scale = "medium", returnclass = "sf")[1]

test_that("pointMap input checks", {
  expect_type(land, "list")
  expect_identical(class(land), c("sf", "data.frame"))
})

test_that("pointMap checks", {
  expect_error(pointMap())
  expect_warning(pointMap(occs = "a", spName = spName))
  expect_warning(pointMap(occs=occs, spName = 2))
  expect_warning(pointMap(occs = occs, spName = spName, land = "a"))
  expect_warning(pointMap(occs = occs, spName = spName, land = land,
                          ptCol = "eggs"))
  expect_warning(pointMap(occs = occs, spName = spName, land = land,
                          ptCol = 2))
  point_map <- pointMap(occs = occs, spName = spName, land = land)
  expect_true(is.ggplot(point_map))
  point_map <- pointMap(occs = occs, spName = spName, land = NA)
  expect_true(is.ggplot(point_map))
})
