#' @title Test different threshold levels and produce presence/absence layers for
#' 3D models
#'
#' @description Creates 3D presence/absence layers by setting all values in a suitability
#' layer above a certain threshold to 1 and all values below that threshold to 0. The
#' threshold value is determined by the sensitivity given by the user, where the
#' sensitivity is the percentage of occurrences to be counted as present in the
#' resulting presence/absence layer. For example, with a sensitivity of 0.9 or 90%,
#' The threshold is the suitability value where 90% of the occurrence points fall on
#' grid cells with suitability scores above that value.
#'
#' The function can try multiple different sensitivity levels, and for each will output
#' a presence/absence spatRaster stack, as well as the specificity (proportion of
#' psuedoabsences correctly considered absent), the true skill statistic (TSS), which is
#' a measure of how well the threshold balances commission and omission error, and the
#' suitability value of the threshold.
#'
#' The user can also downweight sensitivity when calculating TSS with the "weights"
#' parameter. This may be important to do as 3D niche models often have a higher ratio
#' of psuedoabsences to occurrences than 2D models.
#'
#' @param predicted_layers A spatRaster stack of suitability layers where each layer
#' corresponds to a depth slice
#'
#' @param thresholding_vals A vector of sensitivity levels for creating different
#' thresholds and presence/absence layers. If one wanted to test sensitivities of
#' 90%, and 95%, this would be input as c(0.9, 0.95)
#'
#' @param maxent_df 'data.frame' where first column is a vector
#' of presences named "p" containing 1's and 0's. Each row represents a cell in the
#' spatRaster volume with an x, y, z coordinate, and 1's are presences while 0's are
#' absences, or background points. Other columns are environmental variable values
#' extracted at the occurrence and background points.
#'
#' @param coord_df A dataframe containing the longitude, latitude, and depth for each
#' cell in maxent_df, named "longitude," "latitude," and "depth"
#'
#' @param weights a numeric giving what the sensitivity should be downweighted by. If
#' no value is given, TSS will be calculated according to its original formula
#'
#' @references Allouche O, Tsoar A, and Kadmon R. 2006. Assessing the accuracy of species
#' distribution models: prevalence, kappa and the true skill statistic (TSS). *Journal of
#' Applied Ecology*, 43, 1223-1232.
#'
#' @return a 'list' with two components:
#'
#' $threshold_layers, a spatRaster stack of presence absence rasters thresholded at the
#' sensitivity with the highest TSS
#'
#' $tss_results, a 'data.frame' containing the specificity, TSS, and suitability score
#' for each sensitivity given in thresholding_vals
#'
#' @examples
#' library(terra)
#' library(dplyr)
#'
#' # creating list of spatraster stacks where each element is a depth slice
#' r1_d1 <- rast(ncol = 100, nrow = 100)
#' set.seed(0)
#' values(r1_d1) <- sample(c(1:100), size = 1000, replace = T)
#' r2_d1 <- rast(ncol = 100, nrow = 100)
#' set.seed(0)
#' values(r2_d1) <- sample(c(1:1000), size = 1000, replace = T)
#' r1_d2 <- r1_d1
#' values(r1_d2) <- values(r1_d1)+10
#' r2_d2 <- r2_d1
#' values(r2_d2) <- values(r2_d1)+10
#' d1 <- c(r1_d1, r2_d1)
#' names(d1) <- c("valsr1", "valsr2")
#' d2 <- c(r1_d2, r2_d2)
#' names(d2) <- c("valsr1", "valsr2")
#' envlist <- list(d1, d2)
#'
#' # creating occs and bgs
#' set.seed(0)
#' occs <- sample(c(1:nrow(crds(envlist[[1]][[1]]))), size = 50, replace = F)
#' bgs <- sample(c(1:nrow(crds(envlist[[1]][[1]]))), size = 500, replace = F)
#'
#' occs_d1 <- crds(envlist[[1]][[1]])[occs[1:25],]
#' occs_d2 <- crds(envlist[[2]][[1]])[occs[26:50],]
#' bg_d1 <- crds(envlist[[1]][[1]])[bgs[1:250],]
#' bg_d2 <- crds(envlist[[1]][[1]])[bgs[251:500],]
#'
#' # extracting at occs and bgs
#' occ_valsr1_d1 <- extract(envlist[[1]][[1]], occs_d1)
#' occ_valsr1_d2 <- extract(envlist[[2]][[1]], occs_d2)
#' occ_valsr2_d1 <- extract(envlist[[1]][[2]], occs_d1)
#' occ_valsr2_d2 <- extract(envlist[[2]][[2]], occs_d2)
#'
#' occ_valsr1 <- rbind(occ_valsr1_d1, occ_valsr1_d2)
#' occ_valsr2 <- rbind(occ_valsr2_d1, occ_valsr2_d2)
#'
#' bg_valsr1_d1 <- extract(envlist[[1]][[1]], bg_d1)
#' bg_valsr1_d2 <- extract(envlist[[2]][[1]], bg_d2)
#' bg_valsr2_d1 <- extract(envlist[[1]][[2]], bg_d1)
#' bg_valsr2_d2 <- extract(envlist[[2]][[2]], bg_d2)
#'
#' bg_valsr1 <- rbind(bg_valsr1_d1, bg_valsr1_d2)
#' bg_valsr2 <- rbind(bg_valsr2_d1, bg_valsr2_d2)
#'
#' valsr1 <- rbind(occ_valsr1, bg_valsr1)
#' valsr2 <- rbind(occ_valsr2, bg_valsr2)
#'
#' p1 <- rep(1, times = 50)
#' p0 <- rep(0, times = 500)
#' p <- c(p1, p0)
#'
#' maxdf <- data.frame(p, valsr1, valsr2)
#'
#' # creating coord_df
#' coord_df <- rbind(occs_d1, occs_d2, bg_d1, bg_d2)
#' z <- c(rep(1, times = 25), rep(2, times = 25), rep(1, times = 250),
#' rep(2, times = 250))
#' coord_df <- cbind(coord_df, z)
#' colnames(coord_df) <- c("longitude", "latitude", "depth")
#'
#' # creating suitability rasters
#' suitd1 <- d1[[1]]
#' values(suitd1) <- runif(min = 0, max = 1, n = length(values(suitd1)))
#' suitd2 <- d2[[1]]
#' values(suitd2) <- runif(min = 0, max = 1, n = length(values(suitd2)))
#' suit <- c(suitd1, suitd2)
#'
#' # here's the function
#' result <- threshold_3D(predicted_layers = suit, thresholding_vals = c(0.9, 0.95),
#' maxent_df = maxdf, coord_df = coord_df, weights = 2/3)
#'
#' @import terra
#' @import dplyr
#'
#' @keywords threshold
#'
#' @export

threshold_3D <- function(predicted_layers,
                         thresholding_vals, maxent_df, coord_df, weights=NULL) {

  # initial check for proper formatting
  if(!(is.numeric(thresholding_vals))) {
    warning(message("Argument 'thresholding_vals' should be numeric.\n"))
    return(NULL)
  }

  if(any(thresholding_vals > 1)) {
    warning(message("Argument 'thresholding_vals' should be a fraction between 0 and 1.\n"))
    return(NULL)
  }

  if(!("p" %in% colnames(maxent_df))) {
    warning(message("Argument 'maxent_df' is missing column 'p'.\n"))
    return(NULL)
  }

  if(any(!(c("longitude", "latitude", "depth") %in% colnames(coord_df)))) {
    warning(message("Argument 'coord_df' should have columns named 'longitude',
                    'latitude' and 'depth'.\n"))
    return(NULL)
  }

  # making a dataframe occurrences and background
  full_df <- cbind(maxent_df, coord_df)
  occ_df <- full_df %>% filter(p == 1)
  bg_df <- full_df %>% filter(p == 0)

  depth_slices <- unique(bg_df$depth)

  sdm_suit_vals <- vector("list", length = length(depth_slices))
  for(i in 1:length(depth_slices)) {
    need_occ <- occ_df %>% filter(depth == depth_slices[i])
    if(nrow(need_occ) < 1) {
      sdm_suit_vals[[i]] <- NA
    } else {
      sdm_suit_vals[[i]] <- terra::extract(predicted_layers[[i]],
                                           data.frame(need_occ$longitude, need_occ$latitude))
    }
  }
  sdm_suits <- do.call(rbind, sdm_suit_vals)[,2]
  sdm_suits <- sdm_suits[which(!(is.na(sdm_suits)))]
  sdm_suits <- sdm_suits[order(sdm_suits, decreasing = T)]
  thresh_sdm_list <- vector("list", length = length(thresholding_vals))
  tss_list <- vector(length = length(thresholding_vals))
  spec_list <- vector(length = length(thresholding_vals))
  suit_list <- vector(length = length(thresholding_vals))
  for(i in seq_along(thresholding_vals)) {
    thresholded_layers <- vector("list", length = length(predicted_layers))
    real_abs_list <- vector("list", length = length(depth_slices))
    thresh <- trunc(length(sdm_suits)*thresholding_vals[i])
    thresh_val <- sdm_suits[thresh]
    a <- thresh
    c <- length(sdm_suits) - thresh
    for(j in 1:dim(predicted_layers)[3]) {
      rclmat <- matrix(data = c(0, thresh_val, 0,
                                thresh_val, 1, 1), nrow = 2, ncol = 3, byrow = T)
      thresholded_layers[[j]] <- classify(predicted_layers[[j]],
                                          rcl = rclmat)
      need_bg <- bg_df %>% filter(depth == depth_slices[j])
      real_abs_list[[j]] <- terra::extract(thresholded_layers[[j]],
                                           data.frame(need_bg$longitude, need_bg$latitude))
    }
    thresh_sdm_list[[i]] <- rast(thresholded_layers)
    real_abs <- do.call(rbind, real_abs_list)[,2]
    specificity <- length(which(real_abs == 0))/length(real_abs)
    sensitivity <- thresholding_vals[i]
    d <- length(which(real_abs == 0))
    b <- length(real_abs) - length(which(real_abs == 0))
    if(is.null(weights)) {
      tss <- ((a*b) + (a*d) - (c*b))/((a+c)*(b+d))
    } else {
      weightnum <- weights*100
      weightden <- 100
      tss <- ((-(weightnum)*a*b) + (a*d) - ((weightden)*c*b))/((weightden)*(a+c)*(b+d))
    }
    tss_list[i] <- tss
    spec_list[i] <- specificity
    suit_list[i] <- thresh_val
  }
  choose_final <- which(tss_list == max(tss_list))
  if(length(choose_final) > 1) {
    wanted <- thresholding_vals[choose_final][order(thresholding_vals[choose_final])]
    choose_final <- length(wanted)
  }
  tss_df <- data.frame(thresholding_vals, spec_list, tss_list, suit_list)
  colnames(tss_df) <- c("Sensitivity", "Specificity", "TSS",
                        "Suitability")
  final_threshold_output <- list(threshold_layers = thresh_sdm_list[[choose_final]],
                                 tss_results = tss_df)
  return(final_threshold_output)
}

