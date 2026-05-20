#' Create 3D partitions
#'
#' @description Creates partition schemes in 3D for model training and testing.
#' Can create block or kfold partitions, with output as a `vector` or `list`
#'
#' @param maxent_df Data frame with column 'p' (1=presence, 0=absence).
#' @param coord_df  Data frame with 'longitude','latitude','depth' aligned with maxent_df.
#' @param which_partition "k.fold" (default) or "block".
#' @param kfolds Integer >= 2 for k.fold.
#' @param orientation For "block": "lon_lat" (default) or "lat_lon".
#' @param return_format "vector" (default) or "list".
#' @param ensure_all_folds (k.fold) Ensure all folds appear among presences with known depth (default TRUE).
#' @param max_attempts (k.fold) Retry cap when ensure_all_folds=TRUE (default 100).
#' @param na_strategy (k.fold) How to handle NA depth rows: "NA" (default) leaves them NA; "random" assigns a random fold.
#'
#' @details maxent_df and coord_df should be the same dataframes to be provided
#' to `maxent_3D()` for model production.
#' The spatial block partition, similarly to a traditional 2D partition,
#' will separate the occurrences into 4 groups of equal (or about equal) size,
#' with two groups making up two blocks on the upper half of the depth distribution
#' and two groups on the lower half. Background points are assigned to spatial groups
#' according to how they fall into the spatial partitions delimited by the occurrences.
#' Note that this means the number of background points in each partition may not be as
#' close to equal as the occurrences.
#'
#' @return If return_format="vector": integer vector (1..k or 1..4), length == nrow(maxent_df).
#' Unassignable rows get NA. If "list": list(occ_partitions, bg_partitions).
#'
#' @examples
#' \donttest{
#' # create test dataframe
#' occ <- rep(1, times = 10)
#' bg <- rep(0, times = 1000)
#' env1 <- sample(c(1:100), size = 1010, replace = TRUE)
#' env2 <- sample(c(1:1000), size = 1010, replace = TRUE)
#' p <- c(occ, bg)
#' testdf <- data.frame(p, env1, env2)
#'
#' # create test coord data
#' r <- terra::rast(ncol = 100, nrow = 100)
#' set.seed(0)
#' longitude <- sample(terra::ext(r)[1]:terra::ext(r)[2], size = 1010, replace = TRUE)
#' set.seed(0)
#' latitude <- sample(terra::ext(r)[3]:terra::ext(r)[4], size = 1010, replace = TRUE)
#' depth <- sample(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45), size = 1010, replace = TRUE)
#' test_coords <- data.frame(longitude, latitude, depth)
#'
#' # Here's the function
#' result_kfold <- partition_3D(maxent_df = testdf, coord_df = test_coords,
#' which_partition = 'k.fold', kfolds = 3)
#'
#' result_block <- partition_3D(maxent_df = testdf, coord_df = test_coords,
#' which_partition = 'block', orientation = 'lat_lon')
#' }
#'
#' @keywords partition validation
#'
#' @export
#'

partition_3D <- function(maxent_df, coord_df,
                         which_partition = "k.fold",
                         kfolds = NULL,
                         orientation = "lon_lat",
                         return_format = "vector",
                         ensure_all_folds = TRUE,
                         max_attempts = 100,
                         na_strategy = "NA") {

  if (!("p" %in% names(maxent_df))) {
    warning("maxent_df must contain column 'p' (1=presence, 0=absence).")
    return(NULL)
  }

  if(!(which_partition %in% c("k.fold", "block"))){
    warning("which_partition must be k.fold or block")
    return(NULL)
  }

  if(which_partition == "k.fold"){
    if(!is.numeric(kfolds)){
      warning("k folds must be numeric")
      return(NULL)
    }
  }

  need_cols <- c("longitude","latitude","depth")
  if (!all(need_cols %in% names(coord_df))) {
    warning("coord_df must have columns: 'longitude','latitude','depth'.")
    return(NULL)
  }
  if (nrow(maxent_df) != nrow(coord_df)) {
    warning("maxent_df and coord_df must have the same number of rows in the same order.")
    return(NULL)
  }

  # Meat of function
  n_all  <- nrow(maxent_df)
  fold   <- rep(NA_integer_, n_all)  # unified output

  is_pres <- maxent_df$p == 1
  is_bg   <- !is_pres

  occ_xyz <- coord_df[is_pres, , drop = FALSE]
  bg_xyz  <- coord_df[is_bg,  , drop = FALSE]

  if (which_partition == "k.fold") {
    split_into_k <- function(n, k) {
      if (n == 0) integer(0) else rep_len(seq_len(k), length.out = n)
    }
    if (is.null(kfolds) || !is.numeric(kfolds) ||
        length(kfolds) != 1 || kfolds < 2) {
      stop("For 'k.fold', provide numeric 'kfolds' >= 2.")
    }

    # --- Presences: per-depth where depth known; NA depth handled by na_strategy ---
    depth_levels_occ <- sort(unique(occ_xyz$depth[!is.na(occ_xyz$depth)]))
    n_known_occ <- sum(!is.na(occ_xyz$depth))

    assign_occ_once <- function() {
      occ_folds <- rep(NA_integer_, nrow(occ_xyz))
      for (d in depth_levels_occ) {
        idx <- which(occ_xyz$depth == d)
        n   <- length(idx)
        if (n >= kfolds) {
          f <- split_into_k(n, kfolds)
          occ_folds[idx] <- sample(f, size = n, replace = FALSE)
        } else {
          occ_folds[idx] <- sample.int(kfolds, size = n, replace = TRUE)
        }
      }
      # NA-depth handling
      na_idx <- which(is.na(occ_xyz$depth))
      if (length(na_idx) && na_strategy == "random") {
        occ_folds[na_idx] <- sample.int(kfolds, length(na_idx), replace = TRUE)
      } # else remain NA
      occ_folds
    }

    if (length(depth_levels_occ)) {
      attempt <- 1L
      repeat {
        occ_folds <- assign_occ_once()
        present_folds <- unique(occ_folds[!is.na(occ_folds)])
        if (ensure_all_folds && n_known_occ >= kfolds) {
          ok <- length(setdiff(seq_len(kfolds), present_folds)) == 0L
        } else {
          ok <- TRUE
        }
        if (ok || attempt >= max_attempts) break
        attempt <- attempt + 1L
      }
      if (ensure_all_folds && n_known_occ >= kfolds &&
          attempt >= max_attempts &&
          length(setdiff(seq_len(kfolds), unique(occ_folds[!is.na(occ_folds)]))) > 0L) {
        warning("Could not ensure all folds among presences with known depth; proceeding.")
      }
    } else {
      occ_folds <- rep(NA_integer_, nrow(occ_xyz))
      if (na_strategy == "random") {
        # All presences have NA depth — assign randomly if requested
        occ_folds[] <- sample.int(kfolds, length(occ_folds), replace = TRUE)
      } else {
        warning("No presences have known depth; k.fold assigns NA to all presences.")
      }
    }

    # --- Backgrounds: per-depth where depth known; NA depth via na_strategy ---
    depth_levels_bg <- sort(unique(bg_xyz$depth[!is.na(bg_xyz$depth)]))
    bg_folds <- rep(NA_integer_, nrow(bg_xyz))
    for (d in depth_levels_bg) {
      idx <- which(bg_xyz$depth == d)
      n   <- length(idx)
      if (n >= kfolds) {
        f <- split_into_k(n, kfolds)
        bg_folds[idx] <- sample(f, size = n, replace = FALSE)
      } else {
        bg_folds[idx] <- sample.int(kfolds, size = n, replace = TRUE)
      }
    }
    na_bg <- which(is.na(bg_xyz$depth))
    if (length(na_bg) && na_strategy == "random") {
      bg_folds[na_bg] <- sample.int(kfolds, length(na_bg), replace = TRUE)
    } # else remain NA

    # Map into unified vector
    fold[which(is_pres)] <- occ_folds
    fold[which(is_bg)]   <- bg_folds

    if (return_format == "list") {
      return(list(occ_partitions = occ_folds, bg_partitions = bg_folds))
    }
    return(fold)
  }

  # -------- BLOCK partitioning (missing coords -> NA) --------
  pres_with_depth <- which(!is.na(occ_xyz$depth))
  if (!length(pres_with_depth)) {
    warning("No presence rows have non-NA depth; cannot form 3D blocks. Returning NA folds.")
    if (return_format == "list") {
      return(list(occ_partitions = rep(NA_integer_, nrow(occ_xyz)),
                  bg_partitions  = rep(NA_integer_, nrow(bg_xyz))))
    }
    fold[which(is_pres)] <- NA_integer_
    fold[which(is_bg)]   <- NA_integer_
    return(fold)
  }

  # Depth-balanced split of presences with known depth
  occ_d <- occ_xyz[pres_with_depth, , drop = FALSE]
  ord_by_depth <- order(occ_d$depth, na.last = TRUE)
  occ_d <- occ_d[ord_by_depth, , drop = FALSE]

  n_occ <- nrow(occ_d)
  half  <- floor(n_occ / 2)

  top_df    <- if (half > 0) occ_d[seq_len(half), , drop = FALSE] else occ_d[0, , drop = FALSE]
  bottom_df <- if (n_occ > half) occ_d[seq(half + 1L, n_occ), , drop = FALSE] else occ_d[0, , drop = FALSE]

  # Robust depth cutoff calculation from presences
  d_top_max <- if (nrow(top_df) > 0) max(top_df$depth, na.rm = TRUE) else -Inf
  d_bot_min <- if (nrow(bottom_df) > 0) min(bottom_df$depth, na.rm = TRUE) else Inf

  if (d_top_max == d_bot_min) {
    # If the row split falls inside the same discrete depth layer due to skew,
    # use the midpoint of unique depth layers to ensure a geometric split for backgrounds
    unique_depths <- sort(unique(occ_xyz$depth[!is.na(occ_xyz$depth)]))
    if (length(unique_depths) >= 2) {
      depth_cutoff <- unique_depths[floor(length(unique_depths) / 2)]
    } else {
      depth_cutoff <- d_top_max
    }
  } else {
    # Clean separation between layers
    depth_cutoff <- (d_top_max + d_bot_min) / 2
  }

  axis_var <- if (orientation == "lon_lat") "longitude" else "latitude"

  split_by_axis <- function(df_half) {
    n <- nrow(df_half)
    if (n == 0L) return(integer(0))
    grp <- rep(NA_integer_, n)
    valid <- which(!is.na(df_half[[axis_var]]))
    if (length(valid)) {
      o <- order(df_half[[axis_var]][valid], na.last = NA)
      v_idx <- valid[o]
      m <- length(v_idx)
      cut <- floor(m / 2)
      if (m > 0) {
        grp[v_idx[seq_len(cut)]] <- 1L
        if (m > cut) grp[v_idx[seq(cut + 1L, m)]] <- 2L
      }
    }
    grp
  }

  top_grp12     <- split_by_axis(top_df)       # 1/2/NA
  bottom_grp12  <- split_by_axis(bottom_df)    # 1/2/NA
  bottom_grp34  <- bottom_grp12
  bottom_grp34[bottom_grp34 == 1L] <- 3L
  bottom_grp34[bottom_grp34 == 2L] <- 4L

  occ_groups_subset <- c(top_grp12, bottom_grp34)   # depth-ordered
  occ_groups_all <- rep(NA_integer_, nrow(occ_xyz))
  tmp <- integer(length = nrow(occ_d))
  tmp[] <- NA_integer_

  # FIX BUG 3: Correctly map back using original order vector
  tmp[ord_by_depth] <- occ_groups_subset
  occ_groups_all[pres_with_depth] <- tmp

  # Calculate separate spatial cutoffs for top and bottom layers
  cutoff_from_half <- function(df_half, grp_half12) {
    sel1 <- which(grp_half12 == 1L & !is.na(df_half[[axis_var]]))
    sel2 <- which(grp_half12 == 2L & !is.na(df_half[[axis_var]]))
    if (!length(sel1) || !length(sel2)) return(NA_real_)
    (max(df_half[[axis_var]][sel1], na.rm = TRUE) + min(df_half[[axis_var]][sel2], na.rm = TRUE)) / 2
  }

  axis_cutoff_top <- cutoff_from_half(top_df, top_grp12)
  axis_cutoff_bot <- cutoff_from_half(bottom_df, bottom_grp12)

  # Fallbacks if a layer has no spatial spread
  global_axis_median <- stats::median(occ_xyz[[axis_var]], na.rm = TRUE)
  if (is.na(axis_cutoff_top)) axis_cutoff_top <- global_axis_median
  if (is.na(axis_cutoff_bot)) axis_cutoff_bot <- global_axis_median

  # Background quadrants; missing depth or axis -> NA
  bg_axis <- bg_xyz[[axis_var]]
  cond_top    <- !is.na(bg_xyz$depth) & bg_xyz$depth <= depth_cutoff
  cond_bottom <- !is.na(bg_xyz$depth) & !cond_top

  # Calculate distinct spatial cutoffs for the top and bottom layers
  axis_cutoff_top <- cutoff_from_half(top_df, top_grp12)
  axis_cutoff_bot <- cutoff_from_half(bottom_df, bottom_grp12)

  # Robust fallbacks if a specific layer lacks spatial variation
  global_axis_median <- stats::median(occ_xyz[[axis_var]], na.rm = TRUE)
  if (is.na(axis_cutoff_top)) axis_cutoff_top <- global_axis_median
  if (is.na(axis_cutoff_bot)) axis_cutoff_bot <- global_axis_median

  # Evaluate top background points using the top cutoff line,
  # and bottom background points using the bottom cutoff line
  cond_left_top  <- !is.na(bg_axis) & bg_axis <= axis_cutoff_top
  cond_left_bot  <- !is.na(bg_axis) & bg_axis <= axis_cutoff_bot

  bg_groups <- rep(NA_integer_, nrow(bg_xyz))
  bg_groups[cond_top & cond_left_top]      <- 1L
  bg_groups[cond_top & !cond_left_top]     <- 2L
  bg_groups[cond_bottom & cond_left_bot]   <- 3L
  bg_groups[cond_bottom & !cond_left_bot]  <- 4L

  # Fill unified vector
  fold[which(is_pres)] <- occ_groups_all
  fold[which(is_bg)]   <- bg_groups

  if (return_format == "list") {
    return(list(occ_partitions = occ_groups_all, bg_partitions = bg_groups))
  } else {
    return(fold)
  }
}
