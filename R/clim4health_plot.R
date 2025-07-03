#' Plot *s2dv* climate data
#' 
#' @description PENDING
#'
#' @param data *s2dv_cube* object with the climate data to be plotted.
#' @param sdate Numerical index of the forecast starting time to be plotted.
#' @param time Numerical index of the time to be plotted.
#' @param ensemble Numerical index of the ensemble member to be plotted. If
#' set to TRUE (default), it aggregates all the ensemble members
#' @param ensemble_fun Aggregation function when ensemble=TRUE. Defaults to the
#' mean.
#' @param save_path Optional path where the plot should be saved. Extensions 
#' allowed: eps/ps, jpeg, png, pdf, bmp and tiff.
#' @returns A plot
#' @export
#'
#' @examples 
#' # Forecast
#' fcst_path <- paste0(getwd(), "/inst/extdata/forecast/")
#' fcst <- clim4health_load(fcst_path,
#'                          var = "t2m",
#'                          data_type = "fcst",
#'                          date = "20250101",
#'                          ext = 'nc')
#' clim4health_plot(fcst, time = 1) 
clim4health_plot <- function(data,
                             sdate = NULL,
                             time = NULL,
                             ensemble = TRUE,
                             ensemble_fun = mean,
                             save_path = NULL){
  
  # 1. Data checks ----
  
  # Get array info
  dims_data <- dim(data$data)
  dates_data <- data$attrs$Dates
  data_plot <- data$data
  
  # Requirements: sdate
  if(is.null(sdate) & dim(data_plot)["sdate"]>1){
    stop("More than one 'sdate' is available, please select one.")
  }else if(!is.null(sdate) & length(sdate)>1){
    stop("Only one 'sdate' is possible for now.")
  }
  
  # Requirements: time
  if(is.null(time) & dim(data_plot)["time"]>1){
    stop("More than one 'time' is available, please select one.")
  }else if(!is.null(time) & length(time)>1){
    stop("Only one 'time' is possible for now.")
  }
  
  # Requirements: ensemble
  if(dim(data_plot)["sdate"]>1){
    stop("Only one 'ensemble' is possible for now.")
  }
  
  # 2. Data preparation ----

  # Select 'sdate' if needed
  if(!is.null(time)){
    data_plot <- .subset_array_named(data_plot, time = time)
  }
  
  # Select 'time' if needed
  if(!is.null(time)){
    data_plot <- .subset_array_named(data_plot, time = time)
  }

  # Aggregate ensembles if needed
  if((dims_data["ensemble"] > 1) & isTRUE(ensemble)){
    keep_margins <- which(names(dims_data) != "ensemble")
    data_plot <- apply(data_plot, MARGIN = keep_margins, FUN = ensemble_fun)
  }
  
  # 3. Plot ---
  
  # Save file of needed
  if(!is.null(save_path)){
    s2dv::PlotEquiMap(var = data_plot,
                      lon = c(data$coords$longitude), 
                      lat = c(data$coords$latitude),
                      filled.continents = FALSE,
                      fileout = save_path)
  }
  
  # Plot
  out_plot <- s2dv::PlotEquiMap(var = data_plot,
                                lon = c(data$coords$longitude), 
                                lat = c(data$coords$latitude),
                                filled.continents = FALSE)
  
  # Return
  return(invisible(out_plot))
}

