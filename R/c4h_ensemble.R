#' Aggregate climate ensembles 
#'
#' @param data *s2dv_cube* object with the climate data to be aggregated.
#' @param fun Function to perform the aggregation, defaults to mean. Other options
#' include the median.
#'
#' @returns The *s2dv_cube* with the ensemble mean/median. 
#' @export
#'
#' @examples
#' # Forecast
#' fcst_path <- paste0(getwd(), "/inst/extdata/forecast/")
#' fcst <- c4h_load(fcst_path,
#'                          var = "t2m",
#'                          data_type = "fcst",
#'                          date = "20250101",
#'                          ext = 'nc')
#' fcst$dims
#' 
#' # Compute ensemble mean
#' fcst_aggr <- c4h_ensemble(fcst) 
#' fcst_aggr$dims
c4h_ensemble <- function(data,
                         fun = "mean"){
  
  # Check class type
  if(!inherits(data, "s2dv_cube")){
    stop("'c4h_ensemble' only accepts s2dv_cube objects.")
  }
  
  # Check existing relevant ensemble dimension
  if(!"ensemble" %in% names(data$dims)){
    stop("The data doesn't have an 'ensemble' dimension.")
  }else if(data$dims["ensemble"]<=1){
    stop("The data must have more than two ensemble to perform an aggregation.")
  }
  
  # Check valid fun
  if(!fun %in% c("mean", "median")){
    stop("Valid 'fun' include 'mean' and 'median'.")
  }
  
  # Aggregate it
  keep_margins <- which(names(data$dims) != "ensemble")
  data_aggr <- apply(data$data, MARGIN = keep_margins, FUN = fun)
  
  # Metadata
  coords_aggr <- data$coords
  coords_aggr$ensemble <- 1
  attributes(coords_aggr$ensemble) <- attributes(coords_aggr$ensemble)
  dims_aggr <- data$dims
  dims_aggr["ensemble"] <- 1

  # Create results s2dv
  out <- list(data = data_aggr,
               coords = coords_aggr,
               dims = dims_aggr,
               attrs = data$attrs)
  attr(out, "class") <- "s2dv_cube"

  return(out)
}