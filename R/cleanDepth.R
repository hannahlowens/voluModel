#' @title Clean and flag coordinates based on intersection with a land polygon,
#' intersection with a bathymetry layer, and/or by a given depth range
#'
#' @description function to remove or flag coordinates on land, coordinates which are deeper
#' than the known bathymetry at a given xy location, and/or coordinates outside of
#' a specified depth range when cleaning coordinates for species in marine environments
#'
#' @param occs dataframe containing columns named "longitude", "latitude", and "depth," where
#' depth values are positive
#' @param bathy a spatRaster of bathymetry in the same units as the occurrence depth, where
#' values are negative
#' @param land_poly optional polygon of continents in same projection as occurrence data
#' @param depth_range optional range of depth values in the form of c(upper, lower) in which
#' all points should fall between. depth value should be positive, as in the case of the occs
#' @param flag default is F, if T, points are not removed, but flagged if they intersect, and a
#' column is added to the output indicating what points have been flagged
#' @param bottom_correct default is F, but if T, points are not removed, and a column is added
#' to the output displaying the bathymetry value at the flagged point for corrective purposes
#'
#' @details assumes depths are listed as positive in the occs and depth_range, while the
#' bathymetry layer lists them as negative when below the sea surface
#'
#' @return an object of class dataframe containing cleaned or flagged occurrences
#'
#' @examples
#' library(terra)
#' library(dplyr)
#'
#' # Create sample bathymetry
#' r1 <- rast(ncol=10, nrow=10)
#' values(r1) <- c(-100:0)
#'
#' # Create test occurrences
#' set.seed(0)
#' longitude <- sample(ext(r1)[1]:ext(r1)[2], size = 10, replace = FALSE)
#' set.seed(0)
#' latitude <- sample(ext(r1)[3]:ext(r1)[4], size = 10, replace = FALSE)
#' set.seed(0)
#' depth <- sample(1:100, size = 10, replace = TRUE)
#' occs <- data.frame(longitude, latitude, depth)
#'
#' #Heres the function
#' result <- cleanDepth(bathy = r1, occs = occs)
#'
#' @importFrom dplyr select
#' @importFrom terra extract
#' @import sf
#'
#' @keywords occurrence cleaning
#'
#' @export

cleanDepth <- function(occs, bathy, land_poly, depth_range, flag = FALSE,
                       bottom_correct = FALSE) {

  # bathymetry cleaning
  occ_mat <- occs %>% dplyr::select(longitude, latitude)
  depth_at_points <- terra::extract(bathy, occ_mat)
  occs$depth[which(occs$depth > 0 & !(is.na(occs$depth)))] <- 0 -
    occs$depth[which(occs$depth > 0 & !(is.na(occs$depth)))]
  bath_flag <- vector(length = nrow(occs))
  for (k in 1:nrow(occs)) {
    if(is.na(occs$depth[k])) {
      bath_flag[k] < - 0
    } else if(occs$depth[k] < depth_at_points[k,2] | depth_at_points[k,2] > 0) {
      bath_flag[k] <- 1
    } else {
      bath_flag[k] <- 0
    }
  }
  if(flag == T & bottom_correct == T) {
    flagged_points_bathymetry <- bath_flag
    bottom_correct_values <- rep(NA, times = nrow(occs))
    bottom_correct_values[which(bath_flag > 0)] <- depth_at_points[which(bath_flag > 0),2]
    output_df <- cbind(occs, flagged_points_bathymetry, bottom_correct_values)
    print(paste0("flagged ", length(which(bath_flag > 0)), " points intersecting bathymetry"))
  } else if(flag == T & bottom_correct == F) {
    flagged_points_bathymetry <- bath_flag
    output_df <- cbind(occs, flagged_points_bathymetry)
    print(paste0("flagged ", length(which(bath_flag > 0)), " points intersecting bathymetry"))
  } else if(flag == F & bottom_correct == T) {
    bottom_correct_values <- rep(NA, times = nrow(occs))
    bottom_correct_values[which(bath_flag > 0)] <- depth_at_points[which(bath_flag > 0),2]
    output_df <- cbind(occs, bottom_correct_values)
  } else {
    output_df <- occs[-which(bath_flag > 0),]
    print(paste0("removed ", length(which(bath_flag > 0)), " points intersecting bathymetry"))
  }

  # in the case that land_poly is supplied
  if(!(missing(land_poly))) {
    x_sf <- st_as_sf(output_df, coords = c("longitude", "latitude"), crs = st_crs(land_poly))
    within_list <- st_within(x_sf, land_poly)
    within_list_real <- vector(length = length(within_list))
    for(k in 1:length(within_list)) {
      within_list_real[k] <- length(within_list[[k]])
    }
    flag_points <- which(within_list_real != 0)
    if(length(flag_points) > 0) {
      if(flag == T) {
        flagged_points_land <- within_list_real
        output_df <- cbind(output_df, flagged_points_land)
        print(paste0("flagged ", length(flag_points), " points within land polygon"))
      } else {
        output_df <- output_df[-flag_points,]
        print(paste0("removed ", length(flag_points), " points within land polygon"))
      }
    } else {
      print("no points found on land")
    }
  }

  # in the case depth_range is supplied
  if(!(missing(depth_range))) {
    rem1 <- which(!(output_df$depth <= min(depth_range)))
    rem2 <- which(!(output_df$depth >= -max(depth_range)))
    if(length(rem1) > 0) {
      output_df <- output_df[-rem1,]
    }
    if(length(rem2) > 0) {
      output_df <- output_df[-rem2,]
    }
    print(paste0("removed ", length(rem1) + length(rem2), " points outside of depth range"))
  }

  output_df$depth <- abs(output_df$depth)
  return(output_df)
}
