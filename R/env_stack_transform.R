#' @title Coverts a list of 'SpatRaster' stacks where the elements are environmental
#' variables and the layers are depths to a list of SpatRaster stacks where the elements
#' are depths and the layers are environmental variables
#'
#' @description A user will likely already have environmental data organized into a list
#' of 'spatRaster' stacks where each element is a different environmental variable and each
#' layer corresponds to a depth slice for background sampling and extracting. However, the
#' maxent_3D function requires the data be input as a list of 'spatRaster' stacks where
#' each element corresponds to a depth slice and each layer is an environmental variable
#' for the purpose of projecting the model back into geographic space, and this function
#' has been developed for ease of conversion.
#'
#' @param envs_all a list of 'spatRaster' stacks where each element is a different
#' environmental variable and each layer is a depth slice.
#'
#' @param envs_names A 'character' vector of the names of the environmental variables
#' to be applied to the layers of the resulting list of 'spatRaster' stacks
#'
#' @details The extents of each environmental variable layer should match on a depth slice
#' by depth slice basis.
#'
#' @return A list of 'spatRaster' stacks where each list element is a depth slice and
#' each layer is an environmental variable.
#'
#' @examples
#' library(terra)
#'
#' # creating a list of spatRaster stacks where each element is an environmental variable
#' and each layer is a depth
#'
#' env1_d1 <- rast(ncol = 50, nrow = 50)
#' values(env1_d1) <- sample(c(1:100), size = 2500, replace = T)
#' env2_d1 <- rast(ncol = 50, nrow = 50)
#' values(env2_d1) <- sample(c(1:100), size = 2500, replace = T)
#' env1_d2 <- rast(ncol = 50, nrow = 50)
#' values(env1_d2) <- sample(c(1:100), size = 2500, replace = T)
#' env2_d2 <- rast(ncol = 50, nrow = 50)
#' values(env2_d2) <- sample(c(1:100), size = 2500, replace = T)
#'
#' env1 <- c(env1_d1, env1_d2)
#' env2 <- c(env2_d1, env2_d2)
#' envs <- list(env1, env2)
#' envnames <- c("env1", "env2")
#'
#' # Here's the function
#' result <- env_stack_transform(envs_all = envs, envs_names = envnames)
#'
#' @import terra
#'
#' @keywords transform list SpatRaster stack
#'
#' @export


env_stack_transform <- function(envs_all, envs_names) {
  env_call <- vector(length = length(envs_all))
  for (i in 1:length(envs_all)) {
    place <- paste0("envs_all[[", i, "]][[i]]")
    env_call[i] <- place
  }

  stack_list_all <- vector("list", length = dim(envs_all[[1]])[3])
  for (i in 1:dim(envs_all[[1]])[3]) {
    j_list <- vector("list", length = length(env_call))
    for (j in 1:length(env_call)) {
      j_list[[j]] <- eval(parse(text = env_call[j]))
    }
    st <- rast(j_list)
    names(st) <- envs_names
    stack_list_all[[i]] <- st
  }
  return(stack_list_all)
}
