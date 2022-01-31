#' @title Calculate MESS
#'
#' @description Calculates multivariate environmental similarity
#' surface based on model calibration and projection data
#'
#' @param calibration A `data.frame` of environmental variables
#' used to calibrate an ecological niche model, one row for
#' measurements from each voxel included in the data used to
#' calibrate the model. Columns with names not corresponding to
#' `projection` `list` items are ignored.
#'
#' @param projection A named `list` of `RasterBrick` objects for
#' projection; names correspond to `calibration` column names.
#' Each `RasterBrick` should have the same number of layers,
#' corresponding to vertical depth slices.
#'
#' @details `MESS3D` is a wrapper around `mess` from the `dismo`
#' package. It calculates MESS for each depth layer. Negative values
#' indicate areas of extrapolation which should be interpreted with
#' caution (see Elith *et al*, 2010 in *MEE*).
#'
#' @note The calibration dataset should include both presences
#' and background/pseudoabsence points used to calibrate an
#' ecological niche model.
#'
#' @references Elith J, Kearney M, and Phillips S. 2010.
#' The art of modelling range-shifting species.
#' *Methods in Ecology and Evolution*, 1, 330-342.
#'
#' @return A `RasterBrick` of mess scores in each voxel;
#' layer names correspond to layer names of first
#' `RasterBrick` in `projection` `list`.
#'
#' @examples
#' library(raster)
#' library(dplyr)
#'
#' # Create sample rasterBricks
#' r1 <- raster(ncol=10, nrow=10)
#' values(r1) <- 1:100
#' r2 <- raster(ncol=10, nrow=10)
#' values(r2) <- c(rep(20, times = 50), rep(60, times = 50))
#' r3 <- raster(ncol=10, nrow=10)
#' values(r3) <- 8
#' envBrick1 <- brick(r1, r2, r3)
#' names(envBrick1) <- c(0, 10, 30)
#'
#' r1 <- raster(ncol=10, nrow=10)
#' values(r1) <- 100:1
#' r2 <- raster(ncol=10, nrow=10)
#' values(r2) <- c(rep(10, times = 50), rep(20, times = 50))
#' r3 <- raster(ncol=10, nrow=10)
#' values(r3) <- c(10,20,30)
#' envBrick2 <- brick(r1, r2, r3)
#' names(envBrick2) <- c(0, 10, 30)
#'
#' rastList <- list("temperature" = envBrick1, "salinity" = envBrick2)
#'
#' # Create test reference set
#' set.seed(0)
#' longitude <- sample(extent(envBrick1)[1]:extent(envBrick1)[2],
#'                     size = 10, replace = FALSE)
#' set.seed(0)
#' latitude <- sample(extent(envBrick1)[3]:extent(envBrick1)[4],
#'                    size = 10, replace = FALSE)
#' set.seed(0)
#' depth <- sample(0:35, size = 10, replace = TRUE)
#' occurrences <- as.data.frame(cbind(longitude,latitude,depth))
#'
#' # Calibration
#' calibration <- lapply(rastList, FUN = function(x) xyzSample(occurrences, x)) %>% bind_rows
#'
#' # Run the function
#' messStack <- MESS3D(calibration = calibration, projection = rastList)
#' plot(messStack)
#'
#' @importFrom dismo mess
#' @importFrom methods is
#'
#' @seealso \code{\link[dismo]{mess}}
#'
#' @keywords modelDiagnostics
#' @export

MESS3D <- function (calibration, projection) {
  if (!is.data.frame(calibration)) {
    warning(paste0("'calibration' must be an object of class 'data.frame'.\n"))
    return(NULL)
  }
  if (!is.list(projection) || !all(unlist(lapply(projection, FUN = function(x) is(x, "RasterBrick"))))) {
    warning(paste0("'projection' must be a list of 'RasterBrick' objects.\n"))
    return(NULL)
  }
  if(!all(names(projection) %in% colnames(calibration))){
    warning(paste0("'projection' names must correspond with column names in `calibration`.\n"))
    return(NULL)
  }

  # Go through the environmental variables and calculate MESS for each layer
  cal <- as.data.frame(calibration[,names(projection)])
  messStack <- NULL
  for (i in 1:nlayers(projection[[1]])){
    proj <- stack(lapply(projection, FUN = function(x) x[[i]]))
    messLayer <- mess(x = proj, v = cal, full = F)
    messStack <- stack(c(messStack, messLayer))
  }
  names(messStack) <- names(projection[[1]])
  return(messStack)
}
