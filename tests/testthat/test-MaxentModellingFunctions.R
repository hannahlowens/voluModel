# test of maxent modelling functions (partition_3D, maxent_3D, threshold_3D)
library(terra)
library(predicts)
library(rnaturalearth)
library(sf)
library(voluModel)

# Initial Settings & Base Data
target_crs <- "+proj=longlat +datum=WGS84"
set.seed(0)

occs_raw <- read.csv(system.file("extdata/Steindachneria_argentea.csv", package='voluModel'))

# Clean and sample raw occurrences
occtemp <- occs_raw[,c("decimalLatitude", "decimalLongitude", "depth")]
occtemp <- occtemp[complete.cases(occtemp[, c("decimalLatitude", "decimalLongitude", "depth")]),]
occurrences <- occtemp[sample(seq_len(nrow(occtemp)), size = 50, replace = FALSE), ]

# Re-structure occurrences and assign a balanced depth scheme
occurrences <- data.frame(
  longitude = occurrences$decimalLongitude,
  latitude  = occurrences$decimalLatitude,
  depth     = rep(c(1, 2), length.out = nrow(occurrences))
)

# Create Environmental Layers
land <- project(ne_countries(returnclass = "sv"), target_crs)
occs_v <- terra::vect(occurrences, geom = c("longitude", "latitude"), crs = target_crs)

# Generate uniform environmental rasters spanning the data coordinates
r_base <- terra::rast(extent = ext(occs_v), crs = target_crs, resolution = 0.5)

r1d1 <- terra::init(r_base, fun = function(x) as.numeric(sample(0:10000, x, replace = TRUE)))
r2d1 <- terra::init(r_base, fun = function(x) as.numeric(sample(0:100, x, replace = TRUE)))
r1d2 <- terra::init(r_base, fun = function(x) as.numeric(sample(20:10020, x, replace = TRUE)))
r2d2 <- terra::init(r_base, fun = function(x) as.numeric(sample(20:120, x, replace = TRUE)))

envs <- list(
  mask(c(r1d1, r2d1), land, inverse = TRUE),
  mask(c(r1d2, r2d2), land, inverse = TRUE)
)
names(envs[[1]]) <- names(envs[[2]]) <- c("r1", "r2")

# Create background point pools directly from ocean cells
bg_cells_d1 <- crds(envs[[1]][[1]])
bg_cells_d2 <- crds(envs[[2]][[1]])

bgsd1 <- data.frame(bg_cells_d1[sample(nrow(bg_cells_d1), size = 1000, replace = FALSE), ], depth = 1)
bgsd2 <- data.frame(bg_cells_d2[sample(nrow(bg_cells_d2), size = 1000, replace = FALSE), ], depth = 2)

colnames(bgsd1) <- colnames(bgsd2) <- c("longitude", "latitude", "depth")

# Coordinate dataframe containing all potential test points
all_coords <- rbind(
  data.frame(occurrences, p = 1),
  data.frame(bgsd1, p = 0),
  data.frame(bgsd2, p = 0)
)

# Extract raster values matching each row's explicit depth tier
extracted_vals <- matrix(NA_real_, nrow = nrow(all_coords), ncol = 2)
colnames(extracted_vals) <- c("r1", "r2")

for (d in c(1, 2)) {
  idx <- which(all_coords$depth == d)
  if (length(idx) > 0) {
    # extract returns a data frame where cols 2:3 are the raster values
    vals <- terra::extract(envs[[d]], all_coords[idx, c("longitude", "latitude")])
    extracted_vals[idx, ] <- as.matrix(vals[, 2:3])
  }
}

# Bind everything into a single evaluation dataframe
eval_df <- cbind(all_coords, extracted_vals)

# Drop any rows missing environmental variables (e.g., points falling on landmask)
valid_rows <- complete.cases(eval_df[, c("r1", "r2")])
eval_df  <- eval_df[valid_rows, ]

# Split the pristine dataframe into perfectly mirrored test inputs
maxent_df     <- eval_df[, c("p", "r1", "r2")]
maxent_coords <- eval_df[, c("longitude", "latitude", "depth")]

# Cleanup temporary variables from global testing environment
rm(eval_df, all_coords, extracted_vals, valid_rows, bgsd1, bgsd2)

# Create "bad" data
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
  expect_warning(partition_3D(maxent_df = maxent_df,
                              coord_df = maxent_coords,
                              which_partition = 'spam', kfolds = 5))
  expect_warning(partition_3D(maxent_df = maxent_df, coord_df = maxent_coords,
                              which_partition = 'k.fold', kfolds = "h"))
}
)

test_that("partition_3D k.fold outputs as expected", {
  test_result1 <- partition_3D(maxent_df = maxent_df,
                               coord_df = maxent_coords,
                               which_partition = 'k.fold',
                               kfolds = 3, return_format = "list")
  expect_equal(length(unique(test_result1$occ_partitions)), 3)
  expect_equal(length(unique(test_result1$bg_partitions)), 3)
  test_result2 <- partition_3D(maxent_df = maxent_df,
                               coord_df = maxent_coords,
                               which_partition = 'k.fold',
                               kfolds = 5, return_format = "vector")
  expect_equal(length(unique(test_result2)), 5)
  expect_equal(nrow(maxent_df), length(test_result2))
}
)

test_that("partition_3D k.fold outputs as expected", {
  skip_on_cran() # this is way too slow for CRAN
  test_result3 <- partition_3D(maxent_df = maxent_df,
                               coord_df = maxent_coords,
                               which_partition = 'block',
                               orientation = "lat_lon",
                               return_format = "list",
                               max_attempts = 10)
  expect_equal(length(unique(test_result3$occ_partitions)), 4)
  expect_equal(length(unique(test_result3$bg_partitions)), 4)
  test_result4 <- partition_3D(maxent_df = maxent_df, coord_df = maxent_coords,
                               which_partition = 'block', orientation = "lon_lat",
                               return_format = "list", max_attempts = 10)
  expect_equal(length(unique(test_result4$occ_partitions)), 4)
  expect_equal(length(unique(test_result4$bg_partitions)), 4)
  expect_equal(length(test_result3$bg_partitions),
               length(test_result4$bg_partitions))
}
)

# Testing maxent_3D
test_partition <- partition_3D(maxent_df = maxent_df, coord_df = maxent_coords,
                               which_partition = 'k.fold', kfolds = 3, return_format = "list")
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
  skip_if_not_installed("rJava")
  expect_warning(maxent_3D(maxent_df = baddf, wanted_fc = c("L"), wanted_rm = c(1),
                           projection_layers = test_proj, occs = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"), wanted_rm = c(1),
                           projection_layers = test_proj, occs = bad_occs,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c("1"),
                           projection_layers = test_proj, occs = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c(1),
                           projection_layers = bad_layers, occs = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c(1),
                           projection_layers = test_proj, occs = occurrences,
                           depth_list = bad_depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("spam"),
                           wanted_rm = c(1),
                           projection_layers = test_proj, occs = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c(1), wanted_partition = bad_partition1,
                           projection_layers = test_proj, occs = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c(1), wanted_partition = bad_partition2,
                           projection_layers = test_proj, occs = occurrences,
                           depth_list = depths))
  expect_warning(maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"),
                           wanted_rm = c(1), wanted_partition = bad_partition3,
                           projection_layers = test_proj, occs = occurrences,
                           depth_list = depths))
}
)

test_that("maxent_3D outputs as expected", {
  skip_if_not_installed("rJava")
  # Look for maxent.jar in the predicts package space
  jar_path <- system.file("java/maxent.jar", package = "predicts")

  # If it doesn't exist, skip the test gracefully without failing
  if (jar_path == "") {
    skip("maxent.jar is not installed in the 'predicts' package directory. Skipping test.")
  }

  result1 <- maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"), wanted_rm = c(1),
                       projection_layers = test_proj, occs = maxent_coords,
                       depth_list = depths)
  expect_equal(class(result1), "list")
  expect_equal(class(result1$results), "data.frame")
  expect_contains(class(result1$models[[1]]), "MaxEnt_model")
  expect_contains(class(result1$predictions[[1]]), "SpatRaster")
  result2 <- maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"), wanted_rm = c(1),
                       wanted_partition = test_partition,
                       projection_layers = test_proj, occs = maxent_coords,
                       depth_list = depths)
  expect_equal(class(result2$partition_results), "list")
  expect_equal(class(result2$partition_results[[1]]), "data.frame")
  result3 <- maxent_3D(maxent_df = maxent_df, wanted_fc = c("L", "LQ"),
                       wanted_rm = c(1:2),
                       wanted_partition = test_partition,
                       projection_layers = test_proj, occs = maxent_coords,
                       depth_list = depths)
  expect_equal(nrow(result3$results), 4)
}
)

test_that("threshold_3D warnings and outputs behave as expected", {
  skip_if_not_installed("rJava")
  maxent_result <- maxent_3D(maxent_df = maxent_df, wanted_fc = c("L"), wanted_rm = c(1),
                             projection_layers = test_proj, occs = maxent_coords,
                             depth_list = depths)
  predicted_layers <- maxent_result$predictions[[1]]
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

