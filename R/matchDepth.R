#' @title Match the depth values given in an occurrence dataset to the depth slice
#' values of a user provided spatRaster stack
#'
#' @description assigns coordinates to depth slices given a user provided raster template.
#' if any points are deeper than the lowest depth slice, they are assigned to the
#' lowest depth slice.
#'
#' @param occs dataframe of occurrence records with colnames "longitude," "latitude,"
#' and "depth." Depth values should be positive.
#' @param rasterTemplate a spatRaster stack where the layer names correspond to the depth value,
#' as a positive number
#'
#' @details rasterTemplate names and values in the depth column of occs should be positive.
#' Points lower than the deepest depth slice are assigned to the deepest depth slice
#'
#' @return object of class dataframe, the same occs fed into the function but with the
#' depth values adjusted to match the spatRaster stack depth slices
#'
#' @examples
#' # Create test raster brick
#' r1 <- rast(ncol = 100, nrow = 100)
#' r2 <- rast(ncol = 100, nrow = 100)
#' r3 <- rast(ncol = 100, nrow = 100)
#' rbrick <- c(r1, r2, r3)
#' names(rbrick) <- c(1, 40, 90)
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
#' # Here's the function
#' result <- depthMatch(occs = occs, rasterTemplate = rbrick)
#'
#' @keywords occurrence cleaning
#'
#' @export

depthMatch <- function(occs, rasterTemplate) {
  wantdepths <- names(rasterTemplate)
  for(k in 1:length(wantdepths)) {
    if(k < length(wantdepths)) {
      depthint <- as.numeric(c(wantdepths[k], wantdepths[k+1]))
      meandepth <- mean(depthint)
      fixdepths1 <- which(occs$depth > depthint[1] & occs$depth <= meandepth)
      fixdepths2 <- which(occs$depth > meandepth & occs$depth < depthint[2])
      if(length(fixdepths1) > 0) {
        occs$depth[fixdepths1] <- depthint[1]
      }
      if(length(fixdepths2) > 0)
        occs$depth[fixdepths2] <- depthint[2]
    } else {
      fixdepths <- which(occs$depth > as.numeric(wantdepths[k]))
      if(length(fixdepths) > 0) {
        occs$depth[fixdepths] <- as.numeric(wantdepths[k])
      }
    }
  }
  return(occs)
}
