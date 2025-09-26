# test of maxent modelling functions (partition_3D, maxent_3D, threshold_3D)

library(dplyr)
library(terra)
library(predicts)
library(rnaturalearth)
library(sf)
library(voluModel)

# Read occurrences for test
occs <- read.csv(system.file("extdata/Steindachneria_argentea.csv",
                             package='voluModel'))

# Create test occurrences
target_crs <- "+proj=longlat +datum=WGS84"
set.seed(0)
occtemp <- occs %>% dplyr::select(decimalLatitude, decimalLongitude, depth)
occurrences <- occtemp[sample(1:length(complete.cases(occtemp)),
                           size = 50, replace = FALSE),]
occurrences <- occurrences[complete.cases(occurrences),]
colnames(occurrences) <- c("latitude", "longitude", "depth")
dep1 <- which(occurrences$depth == 0)
dep2 <- which(!(occurrences$depth == 0))
occurrences$depth[dep1] <- 1
occurrences$depth[dep2] <- 2
occs_sf <- st_as_sf(x = occurrences,
                    coords = c("longitude", "latitude"),
                    crs = target_crs)

# creating example land polygon
land <- st_as_sf(ne_countries())[1] %>% st_set_crs(target_crs)
land <- land[-which(!(st_is_valid(land))),]

# creating example environmental layers
r1d1 <- rast(extent = ext(occs_sf), crs = target_crs, resolution = 0.5)
r2d1 <- rast(extent = ext(occs_sf), crs = target_crs, resolution = 0.5)
r1d2 <- rast(extent = ext(occs_sf), crs = target_crs, resolution = 0.5)
r2d2 <- rast(extent = ext(occs_sf), crs = target_crs, resolution = 0.5)
values(r1d1) <- sample(c(0:10000), size = length(values(r1d1)), replace = T)
values(r2d1) <- sample(c(0:100), size = length(values(r2d1)), replace = T)
values(r1d2) <- sample(c(20:10020), size = length(values(r1d2)), replace = T)
values(r2d2) <- sample(c(20:120), size = length(values(r2d2)), replace = T)
d1 <- c(r1d1, r2d1)
d1_mask <- mask(d1, land, inverse = T)
names(d1_mask) <- c("r1", "r2")
d2 <- c(r1d2, r2d2)
d2_mask <- mask(d2, land, inverse = T)
names(d2_mask) <- c("r1", "r2")
envs <- list(d1_mask, d2_mask)

# creating example maxent_df and maxent_coords using occs and bgs
occurrences <- data.frame(occurrences$longitude, occurrences$latitude,
                          occurrences$depth)
colnames(occurrences) <- c("longitude", "latitude", "depth")
bgsd1 <- crds(envs[[1]][[1]])[sample(nrow(crds(envs[[1]][[1]])),
                size = 1000,
              replace = F),]
bgsd2 <- crds(envs[[2]][[1]])[sample(nrow(crds(envs[[2]][[1]])),
                size = 1000,
                replace = F),]
occ1 <- occurrences %>% filter(depth == 1)
occ2 <- occurrences %>% filter(depth == 2)
p1 <- extract(envs[[1]], occ1[,1:2])
p2 <- extract(envs[[2]], occ2[,1:2])
en <- rbind(p1[,2:3], p2[,2:3])
a1 <- extract(envs[[1]], bgsd1)
a2 <- extract(envs[[2]], bgsd2)
a <- rbind(a1, a2)
en_final <- rbind(en, a)
p <- c(rep(1, times = nrow(occurrences)), rep(0, times = nrow(a)))
maxent_df <- cbind(p, en_final)

bg1depth <- rep(1, times = nrow(a1))
bgsd1 <- cbind(bgsd1, bg1depth)
colnames(bgsd1) <- c("longitude", "latitude", "depth")
bg2depth <- rep(2, times = nrow(a2))
bgsd2 <- cbind(bgsd2, bg2depth)
colnames(bgsd2) <- c("longitude", "latitude", "depth")
bgcoords <- rbind(bgsd1, bgsd2)
maxent_coords <- rbind(occurrences, bgcoords)

baddf <- maxent_df
colnames(baddf) <- c("a", "b", "c")
badcoords <- maxent_coords
colnames(badcoords) <- c("lon", "lat", "dep")

# testing partition_3D
test_that("partition_3D warnings behave as expected", {
  expect_error(partition_3D())
  expect_warning(partition_3D(maxent_df = baddf, coord_df = maxent_coords,
                              which_partition = 'k.fold', kfolds = 5))
  expect_warning(partition_3D(maxent_df = maxent_df, coord_df = badcoords,
                              which_partition = 'k.fold', kfolds = 5))
  expect_warning(partition_3D(maxent_df = maxent_df, coord_df = maxent_coords,
                              which_partition = 'spam', kfolds = 5))
  expect_warning(partition_3D(maxent_df = maxent_df, coord_df = maxent_coords,
                              which_partition = 'k.fold', kfolds = "h"))
}
)

test_that("partition_3D outputs as expected", {
  test_result1 <- partition_3D(maxent_df = maxent_df, coord_df = maxent_coords,
                               which_partition = 'k.fold', kfolds = 3)
  expect_equal(length(unique(test_result1$occ_partitions)), 3)
  expect_equal(length(unique(test_result1$bg_partitions)), 3)
  test_result2 <- partition_3D(maxent_df = maxent_df, coord_df = maxent_coords,
                               which_partition = 'k.fold', kfolds = 5)
  expect_equal(length(unique(test_result2$occ_partitions)), 5)
  expect_equal(length(unique(test_result2$bg_partitions)), 5)
  test_result3 <- partition_3D(maxent_df = maxent_df, coord_df = maxent_coords,
                               which_partition = 'block', orientation = "lat_lon")
  expect_equal(length(unique(test_result3$occ_partitions)), 4)
  expect_equal(length(unique(test_result3$bg_partitions)), 4)
  test_result4 <- partition_3D(maxent_df = maxent_df, coord_df = maxent_coords,
                               which_partition = 'block', orientation = "lon_lat")
  expect_true(any(test_result3$bg_partitions != test_result4$bg_partitions))
}
)

# Testing maxent_3D
test_partition <- partition_3D(maxent_df = maxent_df, coord_df = maxent_coords,
                               which_partition = 'k.fold', kfolds = 3)
test_proj <- env_stack_transform(envs, c("r1", "r2"))
depths <- c(1, 2)

bad_depths <- c(-1, -2)
bad_partition1 <- vector()
bad_partition2 <- list("a", "b")
bad_partition3 <- test_partition
bad_partition3$occ_partitions <- c(bad_partition3$occ_partitions, 2, 1, 3)
bad_occs <- occurrences
colnames(bad_occs) <- c("a", "b", "c")
bad_layers <- test_proj
names(bad_layers[[1]]) <- c("a", "b")
names(bad_layers[[2]]) <- c("a", "b")

test_that("maxent_3D warnings behave as expected", {
  expect_error(maxent_3D())
  expect_warning(maxent_3D(maxent_df = baddf, wanted_fc = c("L"), wanted_rm = c(1),
                           projection_layers = test_proj, occ_list = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"), wanted_rm = c(1),
                           projection_layers = test_proj, occ_list = bad_occs,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c("1"),
                           projection_layers = test_proj, occ_list = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c(1),
                           projection_layers = bad_layers, occ_list = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c(1),
                           projection_layers = test_proj, occ_list = occurrences,
                           depth_list = bad_depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("spam"),
                           wanted_rm = c(1),
                           projection_layers = test_proj, occ_list = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c(1), wanted_partition = bad_partition1,
                           projection_layers = test_proj, occ_list = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c(1), wanted_partition = bad_partition2,
                           projection_layers = test_proj, occ_list = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c(1), wanted_partition = bad_partition3,
                           projection_layers = test_proj, occ_list = occurrences,
                           depth_list = depths))
}
)

test_that("maxent_3D outputs as expected", {
  result1 <- maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"), wanted_rm = c(1),
                       projection_layers = test_proj, occ_list = occurrences,
                       depth_list = depths)
  expect_equal(class(result1), "list")
  expect_equal(class(result1$results), "data.frame")
  expect_contains(class(result1$models[[1]]), "MaxEnt_model")
  expect_contains(class(result1$predictions[[1]]), "SpatRaster")
  result2 <- maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"), wanted_rm = c(1),
                       wanted_partition = test_partition,
                       projection_layers = test_proj, occ_list = occurrences,
                       depth_list = depths)
  expect_equal(class(result2$partition_results), "list")
  expect_equal(class(result2$partition_results[[1]]), "data.frame")
  result3 <- maxent_3D(maxent_df = maxent_df, wanted_fc = c("L", "LQ"),
                       wanted_rm = c(1:2),
                       wanted_partition = test_partition,
                       projection_layers = test_proj, occ_list = occurrences,
                       depth_list = depths)
  expect_equal(nrow(result3$results), 4)
}
)

# testing threshold_3D
maxent_result <- maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"), wanted_rm = c(1),
                     projection_layers = test_proj, occ_list = occurrences,
                     depth_list = depths)
predicted_layers <- maxent_result$predictions[[1]]

test_that("threshold_3D warnings behave as expected", {
  expect_error(threshold_3D())
  expect_warning(threshold_3D(predicted_layers = predicted_layers,
                              thresholding_vals = c("0.95", "0.9"),
                              maxent_df = maxent_df, coord_df = maxent_coords))
  expect_warning(threshold_3D(predicted_layers = predicted_layers,
                              thresholding_vals = c(95, 90),
                              maxent_df = maxent_df, coord_df = maxent_coords))
  expect_warning(threshold_3D(predicted_layers = predicted_layers,
                              thresholding_vals = c(0.95, 0.90),
                              maxent_df = maxent_df[,2:3], coord_df = maxent_coords))
  expect_warning(threshold_3D(predicted_layers = predicted_layers,
                              thresholding_vals = c(0.95, 0.90),
                              maxent_df = maxent_df, coord_df = badcoords))
}
)

test_that("threshold_3D outputs as expected", {
  result1 <- threshold_3D(predicted_layers = predicted_layers,
                        thresholding_vals = c(0.95, 0.90),
                        maxent_df = maxent_df, coord_df = maxent_coords)
  expect_equal(class(result1), "list")
  expect_contains(class(result1$threshold_layers), "SpatRaster")
  expect_equal(class(result1$tss_results), "data.frame")
  result2 <- threshold_3D(predicted_layers = predicted_layers,
                          thresholding_vals = c(0.95, 0.90),
                          maxent_df = maxent_df, coord_df = maxent_coords,
                          weights = 1/3)
  expect_true(max(result1$tss_results$TSS) != max(result2$tss_results$TSS))
}
)

