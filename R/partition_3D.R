#' @title Create 3D partitions
#'
#' @descrition Creates partition schemes in 3D for model training and testing.
#' Can create block or kfold partitions
#'
#' @param maxent_df A dataframe with a column named 'p' which is a presence/absence
#' vector of 1's for presences and 0's for absences, and each subsequent column is the
#' environmental variable extracted at that presence or absence.
#'
#' @param coord_df A dataframe containing the longitude, latitude, and depth for each
#' cell in maxent_df, named "longitude," "latitude," and "depth"
#'
#' @param which_partition Desired partitioning scheme as character. will
#' only accept "k.fold" or "block"
#'
#' @param orientation For the case of block partitioning, which direction in
#' 2-dimensional space the blocks will be delimited by. Options are "lon_lat," or
#' "lat_lon"
#'
#' @param kfolds For the case of kfold partitioning, the desired number of folds
#'
#' @details maxent_df and coord_df should be the same dataframes to be provided
#' to the maxent_3D function for model production.
#' The spatial block partition, similarly to a traditional 2D partition,
#' will separate the occurrences into 4 groups of equal (or about equal) size,
#' with two groups making up two blocks on the upper half of the depth distribution
#' and two groups on the lower half. Background points are assigned to spatial groups
#' according to how they fall into the spatial partitions delimited by the occurrences.
#'
#' @return A list of two components, $occ_partitions, which is a vector of the same
#' length as the presences in maxent_df giving the groupings for each occurrence, and
#' $bg_partitions, a vector of the same length as the absences in maxent_df giving the
#' groupings for each background point.
#'
#' @examples
#'
#' library(dplyr)
#'
#' # create test dataframe
#' occ <- rep(1, times = 10)
#' bg <- rep(0, times = 1000)
#' env1 <- sample(c(1:100), size = 1010)
#' env2 <- sample(c(1:1000), size = 1010)
#' p <- c(occ, bg)
#' testdf <- data.frame(p, env1, env2)
#'
#' # create test coord data
#' r <- rast(ncol = 100, nrow = 100)
#' set.seed(0)
#' longitude <- sample(ext(r)[1]:ext(r)[2], size = 1010, replace = F)
#' set.seed(0)
#' latitude <- sample(ext(r)[3]:ext(r)[4], size = 1010, replace = F)
#' depth <- sample(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45), size = 1010, replace = T)
#' test_coords <- data.frame(longitude, latitude, depth)
#'
#' # Here's the function
#' result_kfold <- partition_3D(maxent_df = testdf, coord_df = test_coords,
#' which_partition = 'k.fold', kfolds = 3)
#'
#' result_block <- partition_3D(maxent_df = testdf, coord_df = test_coords,
#' which_partition = 'block', orientation = 'lat_lon')
#'
#' @import dplyr
#'
#' @keywords partition validation

partition_3D <- function(maxent_df, coord_df, which_partition, kfolds, orientation) {

  # first ensure input data and arguments are formatted correctly
  if(!("p" %in% colnames(maxent_df))) {
    warning(message("Argument 'maxent_df' is missing column 'p'.\n"))
    return(NULL)
  }

  if(any(!(c("longitude", "latitude", "depth") %in% colnames(coord_df)))) {
    warning(message("Argument 'coord_df' should have columns named 'longitude',
                    'latitude' and 'depth'.\n"))
    return(NULL)
  }

  wanted_sp <- maxent_df[which(maxent_df$p == 1),]
  wanted_sp_coords <- coord_df[which(maxent_df$p == 1),]
  keeprows <- complete.cases(wanted_sp)
  wanted_sp <- wanted_sp[keeprows,]
  wanted_sp_coords <- wanted_sp_coords[keeprows,]

  wanted_bg <- maxent_df[which(maxent_df$p == 0),]
  wanted_bg_coords <- coord_df[which(maxent_df$p == 0),]

  depths <- unique(wanted_bg_coords$depth)

  partition_list <- vector("list", length = length(depths))
  if(which_partition == 'k.fold') {
    if(!(is.numeric(kfolds))) {
      warning(message("Argument 'kfolds' should be numeric.\n"))
      return(NULL)
    }
    unique_bins_total <- 0
    while(any(!(c(1:kfolds) %in% unique_bins_total))) {
      for(i in seq_along(depths)) {
        sp_depth <- wanted_sp_coords[which(wanted_sp_coords$depth == depths[i]),]
        bg_depth <- wanted_bg_coords[which(wanted_bg_coords$depth == depths[i]),]
        if(nrow(sp_depth) < 1) {
          katdepth <- list(occs.grp = NA, bg.grp = sample(c(1:kfolds),
                                                          nrow(bg_depth),
                                                          replace = T))
          partition_list[[i]] <- katdepth
        } else {
          occs.grp <- sample(c(1:kfolds), nrow(sp_depth), replace = T)
          bg.grp <- sample(c(1:kfolds), nrow(bg_depth), replace = T)
          katdepth <- list(occs.grp = occs.grp, bg.grp = bg.grp)
          partition_list[[i]] <- katdepth
        }
      }
      just_occs <- vector("list", length = length(partition_list))
      for (i in 1:length(partition_list)) {
        just_occs[[i]] <- partition_list[[i]]$occs.grp
      }
      just_occs <- unlist(just_occs)[which(!(is.na(unlist(just_occs))))]
      unique_bins_total <- unique(just_occs)
    }
  } else if(which_partition == 'block') {

    # getting needed numbers
    occ_per_block <- round(nrow(wanted_sp)/4)
    occ_per_depth <- occ_per_block*2
    original_order <- c(1:nrow(wanted_sp_coords))
    wanted_sp_new <- cbind(wanted_sp_coords, original_order)
    depth_ordered <- wanted_sp_new[order(wanted_sp_new$depth),]
    row.names(depth_ordered) <- NULL

    # separating dfs into two above the depth cutoff and below the depth cutoff
    # for further clustering
    top_two_blocks_df <- depth_ordered[1:occ_per_depth,]
    bottom_two_blocks_df <- depth_ordered[(occ_per_depth+1):nrow(depth_ordered),]
    if(top_two_blocks_df$depth[nrow(top_two_blocks_df)] ==
       bottom_two_blocks_df$depth[1]) {
      if(nrow(top_two_blocks_df) >= nrow(bottom_two_blocks_df)) {
        movethis <- which(top_two_blocks_df$depth == bottom_two_blocks_df$depth[1])
        bottom_two_blocks_df <- rbind(top_two_blocks_df[movethis,],
                                      bottom_two_blocks_df)
        top_two_blocks_df <- top_two_blocks_df[-movethis,]
      } else {
        movethis <- which(bottom_two_blocks_df$depth ==
                            top_two_blocks_df$depth[nrow(top_two_blocks_df)])
        top_two_blocks_df <- rbind(top_two_blocks_df, bottom_two_blocks_df[movethis,])
        bottom_two_blocks_df <- bottom_two_blocks_df[-movethis,]
      }
    }

    # getting top 2 clusters
    if(orientation == "lon_lat") {
      tt_reorder <- top_two_blocks_df[order(top_two_blocks_df$longitude),]
    } else if(orientation == "lat_lon") {
      tt_reorder <- top_two_blocks_df[order(top_two_blocks_df$latitude),]
    } else {
      warning(message("Not a usable orientation.\n"))
      return(NULL)
    }
    if(nrow(tt_reorder) < occ_per_depth) {
      grp1 <- rep(1, times = length(1:(nrow(tt_reorder)/2)))
      grp2 <- rep(2, times = length((nrow(tt_reorder)/2):nrow(tt_reorder)))
    } else {
      grp1 <- rep(1, times = length(1:occ_per_block))
      grp2 <- rep(2, times = length((occ_per_block+1):nrow(tt_reorder)))
    }
    grp <- c(grp1, grp2)
    row.names(tt_reorder) <- NULL
    tt_final <- cbind(tt_reorder, grp)
    tt_final <- tt_final[order(tt_final$original_order),]

    # getting bottom two clusters
    if(orientation == "lon_lat") {
      bt_reorder <- bottom_two_blocks_df[order(bottom_two_blocks_df$longitude),]
    } else if(orientation == "lat_lon") {
      bt_reorder <- bottom_two_blocks_df[order(bottom_two_blocks_df$latitude),]
    } else {
      warning(message("Not a usable orientation.\n"))
      return(NULL)
    }
    if(nrow(bt_reorder) < occ_per_depth) {
      grp3 <- rep(3, times = length(1:(nrow(bt_reorder)/2)))
      grp4 <- rep(4, times = length((nrow(bt_reorder)/2):nrow(bt_reorder)))
    } else {
      grp3 <- rep(3, times = length(1:occ_per_block))
      grp4 <- rep(4, times = length((occ_per_block+1):nrow(bt_reorder)))
    }
    grp <- c(grp3, grp4)
    row.names(bt_reorder) <- NULL
    bt_final <- cbind(bt_reorder, grp)
    bt_final <- bt_final[order(bt_final$original_order),]

    # reform in total df
    full_df_occ <- rbind(tt_final, bt_final)
    full_df_occ <- full_df_occ[order(full_df_occ$original_order),]

    # getting partition limits for bg
    grp1occ <- full_df_occ %>% filter(grp == 1)
    grp2occ <- full_df_occ %>% filter(grp == 2)
    grp3occ <- full_df_occ %>% filter(grp == 3)
    grp4occ <- full_df_occ %>% filter(grp == 4)

    original_order_bg <- c(1:nrow(wanted_bg_coords))
    wanted_bg_coords <- cbind(wanted_bg_coords, original_order_bg)

    if(orientation == "lon_lat") {
      depth_cutoff <- max(tt_final$depth, na.rm = T)
      lon_cutoff <- (max(grp1occ$longitude) + min(grp2occ$longitude))/2

      grp1bg <- wanted_bg_coords %>% filter(depth <= depth_cutoff) %>%
        filter(longitude <= lon_cutoff)
      grp <- rep(1, times = nrow(grp1bg))
      grp1bg <- cbind(grp1bg, grp)

      grp2bg <- wanted_bg_coords %>% filter(depth <= depth_cutoff) %>%
        filter(longitude > lon_cutoff)
      grp <- rep(2, times = nrow(grp2bg))
      grp2bg <- cbind(grp2bg, grp)

      grp3bg <- wanted_bg_coords %>% filter(depth > depth_cutoff) %>%
        filter(longitude <= lon_cutoff)
      grp <- rep(3, times = nrow(grp3bg))
      grp3bg <- cbind(grp3bg, grp)

      grp4bg <- wanted_bg_coords %>% filter(depth > depth_cutoff) %>%
        filter(longitude > lon_cutoff)
      grp <- rep(4, times = nrow(grp4bg))
      grp4bg <- cbind(grp4bg, grp)

    } else if(orientation == "lat_lon") {
      depth_cutoff <- max(tt_final$depth, na.rm = T)
      lat_cutoff <- (max(grp1occ$latitude) + min(grp2occ$latitude))/2

      grp1bg <- wanted_bg_coords %>% filter(depth <= depth_cutoff) %>%
        filter(latitude <= lat_cutoff)
      grp <- rep(1, times = nrow(grp1bg))
      grp1bg <- cbind(grp1bg, grp)

      grp2bg <- wanted_bg_coords %>% filter(depth <= depth_cutoff) %>%
        filter(latitude > lat_cutoff)
      grp <- rep(2, times = nrow(grp2bg))
      grp2bg <- cbind(grp2bg, grp)

      grp3bg <- wanted_bg_coords %>% filter(depth > depth_cutoff) %>%
        filter(latitude <= lat_cutoff)
      grp <- rep(3, times = nrow(grp3bg))
      grp3bg <- cbind(grp3bg, grp)

      grp4bg <- wanted_bg_coords %>% filter(depth > depth_cutoff) %>%
        filter(latitude > lat_cutoff)
      grp <- rep(4, times = nrow(grp4bg))
      grp4bg <- cbind(grp4bg, grp)
    } else {
      warning(message("Not a usable orientation.\n"))
      return(NULL)
    }
    full_df_bg <- rbind(grp1bg, grp2bg, grp3bg, grp4bg)
    full_df_bg <- full_df_bg[order(full_df_bg$original_order_bg),]

    # creating partition_list
    for(i in seq_along(depths)) {
      wanted_depth_occ <- full_df_occ %>% filter(depth == depths[i])
      wanted_depth_bg <- full_df_bg %>% filter(depth == depths[i])
      if(nrow(wanted_depth_occ) < 1) {
        batdepth <- list(occs.grp = NA, bg.grp = wanted_depth_bg$grp)
      } else {
        batdepth <- list(occs.grp = wanted_depth_occ$grp,
                         bg.grp = wanted_depth_bg$grp)
      }
      partition_list[[i]] <- batdepth
    }

  } else {
    warning(message("Not a usable partition.\n"))
    return(NULL)
  }
  occs_list <- vector("list", length = length(partition_list))
  bg_list <- vector("list", length = length(partition_list))
  for(i in 1:length(partition_list)) {
    occs_list[[i]] <- partition_list[[i]]$occs.grp
    bg_list[[i]] <- partition_list[[i]]$bg.grp
  }
  if(length(which(is.na(unlist(occs_list)))) > 0) {
    occ_partitions <- unlist(occs_list)[-which(is.na(unlist(occs_list)))]
  } else {
    occ_partitions <- unlist(occs_list)
  }
  bg_partitions <- unlist(bg_list)
  output_final <- list(occ_partitions = occ_partitions,
                       bg_partitions = bg_partitions)
  return(output_final)
}
