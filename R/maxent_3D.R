#' @title Create 3D Ecological Niche Models with Maxent
#'
#' @description Uses MaxEnt from 'predicts' package to test multiple models with
#' different feature class combinations, regularization multipliers, and a user
#' supplied partitioning scheme for training and testing. The function outputs
#' model objects, model results, as well as prediction SpatRasters of each model
#' projected back onto geographic space using the supplied environmental SpatRaster
#' stacks
#'
#' @param maxent_df 'data.frame' where the first column is a vector
#' of presences named "p" containing 1's and 0's. Each row represents a cell in the
#' spatRaster volume with an x, y, z coordinate, and 1's are presences while 0's are
#' absences, or background points. Other columns are environmental variable values
#' extracted at the occurrence and background points, and should have the same names
#' as the names of the environmental layers in the projection_layers list of SpatRaster
#' stacks.
#'
#' @param wanted_fc a character vector giving what feature class
#' combinations should be tried. "L" refers to linear, "Q" refers to quadratic, "H"
#' refers to hinge, and "P" refers to product. Should be in the format c("L", "Q", "LQ")
#' etc.
#'
#' @param wanted_rm regularization multipliers to be tried in format c(1:4)
#'
#' @param wanted_partition optional, should be the output of 'partition_3D'. if no
#' partition is supplied, all points will be used for training
#'
#' @param projection_layers list of SpatRaster stacks by depth for predictions,
#' must all be cropped to the depth slice with the largest extent and masked
#' to the accessible area of the matching depth slice. Each element is a depth slice,
#' and each stack should contain all of the environmental variables used in the model
#' with the same names as that used in the model.
#'
#' @param occ_list occurrence data frame of longitude, latitude, and depth, with columns
#' named "longitude", "latitude", and "depth".
#'
#' @param depth_list vector of depths corresponding to depth slices of list elements of
#' projection_layers. Should be positive and go from shallowest depth to deepest depth
#'
#' @details The names of the projection_layers should be the same as the column names
#' of the environmental variables in maxent_df. The number of models output will be the
#' number of feature classes multiplied by the number of regularization multipliers.
#' For example, a wanted_fc of c("L", "Q", "P") and a wanted_rm of c(1:3) will output 9
#' total models.
#'
#' @return An object of class 'list' with four components:
#'
#' $models, a list containing each model object produced.
#'
#' $results, a 'data.frame' where each row is a model corresponding to the list element
#' in $models and $predictions. If there was no partition supplied for training and
#' testing, each column will report the feature class, regularization multiplier, AUC,
#' total coefficients, nonzero coefficients, AICc, and delta AICc. if a partition is
#' used, it will report the average of these statistics across all partitions.
#'
#' $predictions, a 'list' of spatraster stacks where each list element is a model
#' corresponding to the rows of $results and elements of $models projected onto the
#' supplied projection_layers. Each layer in the stack is a depth slice.
#'
#' $partition_results, a 'list' object of the same length as the number of partition
#' groups containing a 'data.frame' of results for each model for each partition. Only
#' produced if a partition is supplied.
#'
#' @examples
#'
#' library(dplyr)
#' library(predicts)
#' library(terra)
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
#' # creating occ_list
#' coords <- rbind(occs_d1, occs_d2)
#' colnames(coords) <- c("longitude", "latitude")
#' depth <- c(rep(1, times = 25), rep(2, times = 25))
#' occ_list <- cbind(coords, depth)
#'
#' # here's the function
#' result <- maxent_3D(maxent_df = maxdf, wanted_fc = c("L", "Q"),
#'                     wanted_rm = c(1:2), projection_layers = envlist,
#'                     occ_list = occ_list, depth_list = c(1,2))
#'
#' @import dplyr
#' @import predicts
#' @import terra
#'
#' @keywords MaxEnt
#'
#' @export


maxent_3D <- function(maxent_df, wanted_fc, wanted_rm, wanted_partition=NULL,
                      projection_layers, occ_list, depth_list) {

  # initial check of proper formatting
  if(!("p" %in% colnames(maxent_df))) {
    warning(message("Argument 'maxent_df' is missing column 'p'.\n"))
    return(NULL)
  }

  if(any(!(c("longitude", "latitude", "depth") %in% colnames(occ_list)))) {
    warning(message("Argument 'occ_list' should have columns named 'longitude',
                    'latitude' and 'depth'.\n"))
    return(NULL)
  }

  if(!(is.numeric(wanted_rm))) {
    warning(message("Argument 'wanted_rm' must be a numeric vector.\n"))
    return(NULL)
  }

  if(any(!(names(projection_layers[[1]]) %in% colnames(maxent_df)))) {
    warning(message("Argument 'projection_layers' should contain the same names as
                    the column names for 'maxent_df'.\n"))
    return(NULL)
  }

  if(!(is.numeric(depth_list)) | any(depth_list < 0)) {
    warning(message("Argument 'depth_list' should be numeric, go from the shallowest
                    to the deepest depth, and should be all positive.\n"))
    return(NULL)
  }

  # lets remove incomplete cases from the maxent_df so all points match
  maxent_df_present <- maxent_df %>% filter(p == 1)
  maxent_df_absent <- maxent_df %>% filter(p == 0)
  maxent_df_present <- maxent_df_present[complete.cases(maxent_df_present),]
  df_for_maxent <- rbind(maxent_df_present, maxent_df_absent)

  # first we'll make the fc input readable to maxent
  new_wanted_fc <- vector("list", length = length(wanted_fc))
  for(i in 1:length(wanted_fc)) {
    if(wanted_fc[i] == "L") {
      wanted_fc1 <- c("noquadratic", "noproduct", "nohinge", "noautofeature")
    } else if(wanted_fc[i] == "Q") {
      wanted_fc1 <- c("nolinear", "noproduct", "nohinge", "noautofeature")
    } else if(wanted_fc[i] == "H") {
      wanted_fc1 <- c("nolinear", "noquadratic", "noproduct", "noautofeature")
    } else if(wanted_fc[i] == "P") {
      wanted_fc1 <- c("nolinear", "noquadratic", "nohinge", "noautofeature")
    } else if(wanted_fc[i] == "LQ") {
      wanted_fc1 <- c("nohinge", "noproduct", "noautofeature")
    } else if(wanted_fc[i] == "LH") {
      wanted_fc1 <- c("noproduct", "noquadratic", "noautofeature")
    } else if(wanted_fc[i] == "LP") {
      wanted_fc1 <- c("noquadratic", "nohinge", "noautofeature")
    } else if(wanted_fc[i] == "QH") {
      wanted_fc1 <- c("nolinear", "noproduct", "noautofeature")
    } else if(wanted_fc[i] == "QP") {
      wanted_fc1 <- c("nolinear", "nohinge", "noautofeature")
    } else if(wanted_fc[i] == "HP") {
      wanted_fc1 <- c("nolinear", "noquadratic", "noautofeature")
    } else if(wanted_fc[i] == "LQP") {
      wanted_fc1 <- c("nohinge", "noautofeature")
    } else if(wanted_fc[i] == "LQH") {
      wanted_fc1 <- c("noproduct", "noautofeature")
    } else if(wanted_fc[i] == "QHP") {
      wanted_fc1 <- c("nolinear", "noautofeature")
    } else if(wanted_fc[i] == "LHP") {
      wanted_fc1 <- c("noquadratic", "noautofeature")
    } else if(wanted_fc[i] == "LQHP") {
      wanted_fc1 <- c("noautofeature")
    } else {
      warning(message("Not a usable feature class.\n"))
      return(NULL)
    }
    new_wanted_fc[[i]] <- wanted_fc1
  }

  # next we'll make a full list of the fc and rm combinations
  # j loop for each rm, make a list the length of rm attached to fc[i]
  # so there will be i lists for each fc
  # concatenate the i lists into one list to make the full combined rm and fc list
  fc_perm_list <- vector("list", length = length(new_wanted_fc))
  for(i in 1:length(new_wanted_fc)) {
    rm_perm_list <- vector("list", length = length(wanted_rm))
    for(j in 1:length(wanted_rm)) {
      rm_element <- wanted_rm[j]
      fc_element <- new_wanted_fc[[i]]
      rm_perm_list[[j]] <- list(rm_element, fc_element)
    }
    fc_perm_list[[i]] <- rm_perm_list
  }
  final_perm_call <- vector("list", length = length(wanted_fc))
  for(i in 1:length(wanted_fc)) {
    rm_parts <- vector(length = length(wanted_rm))
    for(j in 1:length(wanted_rm)) {
      rm_parts[j] <- paste0("fc_perm_list[[", i, "]][[", j, "]]")
    }
    final_perm_call[[i]] <- rm_parts
  }
  final_perm_call_real <- unlist(final_perm_call)
  final_perm_list <- vector("list", length = length(final_perm_call_real))
  for(i in 1:length(final_perm_call_real)) {
    final_perm_list[[i]] <- eval(parse(text = final_perm_call_real[i]))
  }

  # if there is no partitioning scheme, a model will be run that is trained
  # on all of the data. These models will also be returned as the models
  # run for a partitioning scheme, but the partitioning scheme run will also
  # return validation statistics. it can only return AUC
  if(is.null(wanted_partition)) {
    model_list <- vector("list", length = length(final_perm_list))
    auc_list <- vector("list", length = length(final_perm_list))
    total_coef_list <- vector(length = length(final_perm_list))
    nonzero_coef_list <- vector(length = length(final_perm_list))
    predictions_list <- vector("list", length = length(final_perm_list))
    AICc_list <- vector(length = length(final_perm_list))
    print("running models")
    for(i in 1:length(final_perm_list)) {
      # create model
      mod1 <- MaxEnt(x = df_for_maxent[,-1], p = df_for_maxent[,1],
                     args = c(paste0("betamultiplier=",
                                     final_perm_list[[i]][[1]]),
                              paste0(final_perm_list[[i]][[2]])))
      # save model
      model_list[[i]] <- mod1
      # retrieve AUC
      auc_list[[i]] <- mod1@results[which(rownames(mod1@results) ==
                                            "Training.AUC"),1]
      # retrieve coefficents
      all_coef <- suppressWarnings(as.numeric(unlist(strsplit(mod1@lambdas, ","))))
      all_coef <- all_coef[-which(is.na(all_coef))]
      total_coef_list[i] <- length(all_coef)
      nonzero_coef <- all_coef[which(all_coef != 0)]
      nonzero_coef <- nonzero_coef[which(nonzero_coef != 0.000000e+00)]
      nonzero_coef_list[i] <- length(nonzero_coef)
      # generate predictions and pull out values at occurrences for AICc calc
      predicted_suit_list <- vector("list", length = length(projection_layers))
      wanted_val_list <- vector("list", length = length(projection_layers))
      for(j in 1:length(projection_layers)) {
        predicted_suit <- predict(mod1, projection_layers[[j]])
        predicted_suit_list[[j]] <- predicted_suit
        standard_suit <- predicted_suit
        values(standard_suit) <- values(standard_suit)/max(values(standard_suit),
                                                           na.rm = T)
        needed_occs <- occ_list %>% filter(depth == depth_list[j])
        if(nrow(needed_occs > 0)) {
          wanted_val_list[[j]] <- terra::extract(standard_suit,
                                                 data.frame(needed_occs$longitude,
                                                            needed_occs$latitude))
        } else {
          wanted_val_list[[j]] <- NA
        }
      }
      wanted_val_final <- do.call(rbind, wanted_val_list)[,1]
      wanted_val_final <- wanted_val_final[which(!(is.na(wanted_val_final)))]
      predictions_list[[i]] <- rast(predicted_suit_list)


      # calculating AICc
      bigk <- length(nonzero_coef)
      likelihood <- sum(log(wanted_val_final))
      littlen <- length(wanted_val_final)
      if(bigk == (littlen - 1)) {
        AICc_list[i] <- NA
      } else {
        AICc <- ((2*bigk) - (2*likelihood)) +
          (((2*bigk)*(bigk + 1))/(littlen - bigk - 1))
        AICc_list[i] <- AICc
      }
    }
    all_auc <- do.call(rbind, auc_list)
    rm <- rep(wanted_rm, times = length(wanted_fc))
    fc_list <- vector("list", length = length(wanted_fc))
    for(i in 1:length(wanted_fc)) {
      fc_list[[i]] <- rep(wanted_fc[i], times = length(wanted_rm))
    }
    fc <- unlist(fc_list)
    delta.AICc <- AICc_list - min(AICc_list, na.rm = T)
    mod_results <- data.frame(fc, rm, all_auc[,1], total_coef_list, nonzero_coef_list,
                              AICc_list, delta.AICc)
    colnames(mod_results) <- c("fc", "rm", "train.AUC", "toal.coef", "nonzero.coef",
                               "AICc", "delta.AICc")
    final_output <- list(models = model_list, predictions = predictions_list,
                         results = mod_results)

  # generating models for a given partition scheme
  } else {

    # partition warnings
    if(!(is.list(wanted_partition)) & !("occ_partitions" %in% names(wanted_partition))
         & !("bg_partitions" %in% names(wanted_partition))) {
      warning(message("Argument 'wanted_partition' should be a list containing the
                      elements '$occ_partitions' and 'bg_partitions', as output
                      by partition_3D.\n"))
      return(NULL)
    }

    if(length(wanted_partition$occ_partitions) != nrow(maxent_df_present)) {
      warning(message("Number of occurrences in partition does not match number of
                      occurrences in 'maxent_df'.\n"))
      return(NULL)
    }

    # full models for model part of output
    model_list <- vector("list", length = length(final_perm_list))
    print("running models")
    for(i in 1:length(final_perm_list)) {
      # create model
      mod1 <- MaxEnt(x = df_for_maxent[,-1], p = df_for_maxent[,1],
                     args = c(paste0("betamultiplier=",
                                     final_perm_list[[i]][[1]]),
                              paste0(final_perm_list[[i]][[2]])))
      # save model
      model_list[[i]] <- mod1
    }

    num_of_partitions <- unique(wanted_partition$bg_partitions)
    # generating training and testing models for each partition

    part_model_list <- vector("list", length = length(num_of_partitions))
    part_eval_list <- vector("list", length = length(num_of_partitions))
    part_predictions_list <- vector("list", length = length(num_of_partitions))
    present_only <- df_for_maxent[which(df_for_maxent$p == 1),]
    absent_only <- df_for_maxent[which(df_for_maxent$p == 0),]
    for (i in (1:length(num_of_partitions))) {
      test_p <- present_only[which(wanted_partition$occ_partitions ==
                                     num_of_partitions[i]),]
      train_p <- present_only[which(wanted_partition$occ_partitions !=
                                      num_of_partitions[i]),]
      test_a <- absent_only[which(wanted_partition$bg_partitions ==
                                    num_of_partitions[i]),]
      train_a <- absent_only[which(wanted_partition$bg_partitions !=
                                     num_of_partitions[i]),]
      test_full <- rbind(test_p, test_a)
      train_full <- rbind(train_p, train_a)
      mod_per_param_list <- vector("list", length = length(final_perm_list))
      auc_val_per_param_list <- vector(length = length(final_perm_list))
      auc_train_per_param_list <- vector("list", length = length(final_perm_list))
      total_coef_list <- vector(length = length(final_perm_list))
      nonzero_coef_list <- vector(length = length(final_perm_list))
      predictions_list <- vector("list", length = length(final_perm_list))
      print(paste0("training and testing models for partition ", i))
      for(j in 1:length(final_perm_list)) {
        # generating each model combination for partition i
        train_model <- MaxEnt(x = train_full[,-1], p = train_full[,1],
                              args = c(paste0("betamultiplier=",
                                              final_perm_list[[j]][[1]]),
                                       paste0(final_perm_list[[j]][[2]])))
        # saving model
        mod_per_param_list[[j]] <- train_model
        # retrieving validation auc
        test_full_present <- test_full %>% filter(p == 1)
        test_full_absent <- test_full %>% filter(p == 0)
        ev <- predicts::pa_evaluate(p = test_full_present[,-1],
                                    a = test_full_absent[,-1],
                                    model = train_model)
        auc_val_per_param_list[j] <- ev@stats$auc
        # retrieving training AUC
        auc_train_per_param_list[[j]] <-
          train_model@results[which(rownames(train_model@results) == "Training.AUC"),1]
        # retrieve coefficents
        all_coef <- suppressWarnings(as.numeric(unlist(strsplit(train_model@lambdas, ","))))
        all_coef <- all_coef[-which(is.na(all_coef))]
        total_coef_list[j] <- length(all_coef)
        nonzero_coef <- all_coef[which(all_coef != 0)]
        nonzero_coef <- nonzero_coef[which(nonzero_coef != 0.000000e+00)]
        nonzero_coef_list[j] <- length(nonzero_coef)
        # generate predictions; AICc will be calculated after model averaging across
        # partitions
        predicted_suit_list <- vector("list", length = length(projection_layers))
        wanted_val_list <- vector("list", length = length(projection_layers))
        for(k in 1:length(projection_layers)) {
          predicted_suit <- predict(train_model, projection_layers[[k]])
          predicted_suit_list[[k]] <- predicted_suit
        }
        predictions_list[[j]] <- predicted_suit_list
      }
      part_model_list[[i]] <- mod_per_param_list
      all_auc_train <- do.call(rbind, auc_train_per_param_list)
      rm <- rep(wanted_rm, times = length(wanted_fc))
      fc_list <- vector("list", length = length(wanted_fc))
      for(j in 1:length(wanted_fc)) {
        fc_list[[j]] <- rep(wanted_fc[j], times = length(wanted_rm))
      }
      fc <- unlist(fc_list)
      mod_results <- data.frame(fc, rm, all_auc_train, auc_val_per_param_list,
                                total_coef_list,
                                nonzero_coef_list)
      colnames(mod_results) <- c("fc", "rm", "train.AUC", "val.AUC", "total.coef",
                                 "nonzero.coef")
      part_eval_list[[i]] <- mod_results
      part_predictions_list[[i]] <- predictions_list
    }

    # creating a mod results dataframe averaged across all partitions
    wantdf <- part_eval_list[[1]][,-c(1:2)]
    cell_num <- (nrow(wantdf)*ncol(wantdf))
    avg_mod_result <- matrix(nrow = nrow(wantdf),
                             ncol = ncol(wantdf))
    for(i in 1:cell_num) {
      avg_vec <- vector(length = length(num_of_partitions))
      for(j in 1:length(num_of_partitions)) {
        wantdf <- part_eval_list[[j]][,-c(1:2)]
        avg_vec[j] <- as.matrix(wantdf)[i]
      }
      avg_mod_result[i] <- mean(avg_vec)
    }
    avg_mod_result <- data.frame(avg_mod_result)
    colnames(avg_mod_result) <- c("avg.train.AUC", "avg.val.AUC", "avg.total.coef",
                                  "avg.nonzero.coef")
    avg_mod_result <- cbind(part_eval_list[[1]][,1:2], avg_mod_result)

    # creating an average suitability list across partitions for final predictions
    # and average AICc
    avg_predictions_list <- vector("list", length = length(final_perm_list))
    for(i in 1:length(final_perm_list)) {
      avg_suit_list <- vector("list", length = length(projection_layers))
      for(j in 1:length(projection_layers)) {
        partition_layers <- vector("list", length = length(num_of_partitions))
        for(k in 1:length(num_of_partitions)) {
          partition_layers[[k]] <- part_predictions_list[[k]][[i]][[j]]
        }
        avg_across_depth_for_model_i <- terra::app(rast(partition_layers), mean)
        avg_suit_list[[j]] <- avg_across_depth_for_model_i
      }
      avg_predictions_list[[i]] <- rast(avg_suit_list)
    }

    # calculating average AICc for each model type
    avg.AICc <- vector(length = length(final_perm_list))
    for(i in 1:length(final_perm_list)) {
      wanted_val_list <- vector("list", length = length(projection_layers))
      for(j in 1:length(projection_layers)) {
        standard_suit <- avg_predictions_list[[i]][[j]]
        values(standard_suit) <- values(standard_suit)/max(values(standard_suit),
                                                           na.rm = T)
        needed_occs <- occ_list %>% filter(depth == depth_list[j])
        if(nrow(needed_occs) > 0) {
          wanted_val_list[[j]] <- terra::extract(standard_suit,
                                    data.frame(needed_occs$longitude, needed_occs$latitude))
        } else {
          wanted_val_list[[j]] <- NA
        }
      }
      wanted_val_final <- do.call(rbind, wanted_val_list)[,1]
      wanted_val_final <- wanted_val_final[which(!(is.na(wanted_val_final)))]
      # calculating AICc
      bigk <- avg_mod_result$avg.nonzero.coef[i]
      likelihood <- sum(log(wanted_val_final))
      littlen <- length(wanted_val_final)
      if(bigk == (littlen - 1)) {
        avg.AICc[i] <- NA
      } else {
        AICc <- ((2*bigk) - (2*likelihood)) +
          (((2*bigk)*(bigk + 1))/(littlen - bigk - 1))
        avg.AICc[i] <- AICc
      }
    }
    avg.delta.AICc <- avg.AICc-min(avg.AICc)
    avg_mod_result <- cbind(avg_mod_result, avg.AICc, avg.delta.AICc)

    final_output <- list(models = model_list, predictions = avg_predictions_list,
                         results = avg_mod_result, partition_results = part_eval_list)
  }
  return(final_output)
}
