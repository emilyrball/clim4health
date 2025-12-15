#' Collapse one dimension of a s2dv_cube object
#'
#' @description 
#' `c4h_collapse()` works differently depending on the dimension being collapsed:
#' * Use dim = "ensemble" to compute a summary statistic across ensemble members in 
#' forecast and hindcast datasets.
#' * Use dim = "time" to compute climatologies with observational data (i.e. the 
#' length of the 'sdate' dimension must be one). In this case the first "time" 
#' of the dataset is assigned as the new s2dv time coordinate.
#' 
#' @param data *s2dv_cube* object with the climate data.
#' @param dim Dimension to collapse. Options include 'ensemble' and 'time'.
#' @param fun Function to perform the aggregation, defaults to 'mean'. Other options
#' include 'median', 'min' and 'max'.
#' @param na.rm Logical, indicating whether NA values should be ignored when 
#' applying the collapsing function. Defaults to TRUE.
#'
#' @returns The collapsed s2dv_cube. 
#' @export
#'
#' @examples
#' # Load a forecast
#' fcst <- c4h_load(system.file("extdata", "forecast", package = "clim4health"),
#'                  var = "t2m",
#'                  year = 2025,
#'                  month = 1,
#'                  ext = 'nc')
#' fcst$dims
#' 
#' # Ensemble median of a forecast
#' fcst_coll <- c4h_collapse(fcst, dim = "ensemble", fun = "median") 
#' fcst_coll$dims
#' 
#' # Load an observational dataset
#' stations_path <- system.file("extdata", "stations", package = "clim4health")
#' stations <- c4h_load(stations_path, var = "temp_mean", ext = "csv")
#' stations$dims
#' 
#' # Compute climatologies (average temp conditions) 
#' stations_coll <- c4h_collapse(stations, dim = "time") 
#' stations_coll$dims
c4h_collapse <- function(data,
                         dim,
                         fun = "mean",
                         na.rm = TRUE) {
  
  # Input cheks ----
  
  # Check class type
  if (!inherits(data, "s2dv_cube")) {
    stop("'ch4_collapse' only accepts s2dv_cube objects.")
  }
  
  
  # Check collapsing dimension
  if (!dim %in% c("ensemble", "time")) {
    stop("c4h_collapse can only collapse the 'ensemble' and 'time' dimensions.")
  } else if (!dim %in% names(data$dims)) {
    stop(paste0("The data doesn't have a ", dim, " dimension."))
  } else if (data$dims[dim]<=1) {
    stop(paste0("The size of the dimension ", dim, " must be larger than one."))
  } 
  
  # Check valid fun
  if (!fun %in% c("mean", "median", "min", "max")) {
    stop("Valid 'fun' include 'mean', 'median', 'min' and 'max'.")
  }
  
  # Aggregation ----
  
  if (dim == "ensemble") {
    # Compute aggr
    keep_margins <- which(names(data$dims) != "ensemble")
    data_aggr <- .apply_keepdim(data$data, keep_margins, fun, na.rm = na.rm)
    # Metadata
    coords_aggr <- data$coords
    coords_aggr$ensemble <- 1
    attributes(coords_aggr$ensemble) <- attributes(data$coords$ensemble)
    dims_aggr <- data$dims
    dims_aggr["ensemble"] <- 1
    attr_agg <- data$attrs
    
  } else if (dim == "time") {
    
    # Check that dim "sdate" is of length 1
    if (data$dims["sdate"] > 1) {
      stop("To collapse the 'time' dimension, the length of the 'sdate' dimension",
           " must be one (i.e. observational data).")
    }
    # Compute aggr
    keep_margins <- which(names(data$dims) != "time")
    data_aggr <- .apply_keepdim(data$data, keep_margins, fun, na.rm = na.rm)
    # Metadata
    coords_aggr <- data$coords
    coords_aggr$time <- 1
    attributes(coords_aggr$time) <- attributes(data$coords$time)
    dims_aggr <- data$dims
    dims_aggr["time"] <- 1
    attr_agg <- data$attrs
    attr_agg$Dates <- data$attrs$Dates[1,1]
    dim(attr_agg$Dates) <- c(sdate = 1, time = 1)
  }
  
  
  # Create results s2dv
  out <- list(data = data_aggr,
              coords = coords_aggr,
              dims = dims_aggr,
              attrs = attr_agg)
  attr(out, "class") <- "s2dv_cube"
  
  return(out)
}


#' Aggregate a multidimensional array while keeping the aggregate dimension
#'
#' @param data s2dv object to be aggregated.
#' @param margins dimensions to keep.
#' @param fun function to apply.
#' @param na.rm logical, indicating whether NA values should be ignored.
#'
#' @returns The collapsed array with the same number of dimensions as the input.
#' @noRd
.apply_keepdim <- function(data,
                           margins,
                           fun,
                           na.rm) {
  
  out <- apply(data, margins, fun, na.rm = na.rm)
  d <- dim(data)
  newdim <- rep(1, length(d))
  names(newdim) <- names(d)
  newdim[margins] <- dim(out)
  array(out, dim = newdim)
}
