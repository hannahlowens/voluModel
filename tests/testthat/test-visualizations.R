library(rnaturalearth)
occs <- read.csv(system.file("extdata/Aphanopus_intermedius.csv", package='voluModel'))
spName <- "Aphanopus intermedius"
world <- ne_countries(scale = "medium", returnclass = "sf")[1]

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
