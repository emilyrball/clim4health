#' Plot *s2dv* climate data
#' 
#' @description PENDING
#'
#' @param data *s2dv_cube* object with the climate data to be plotted.
#' @param var Numerical index of the variable to be plotted. 
#' Only required if more than one variable is available.
#' @param sdate Numerical index of the forecast starting time to be plotted. 
#' Only required if more than one 'sdate' is available.
#' @param time Numerical index of the time to be plotted.
#' Only required if more than one 'time' is available.
#' @param ensemble Numerical index of the ensemble member to be plotted. If
#' set to TRUE (default), it aggregates all the ensemble members.
#' Only required if more than one 'ensemble' member is available.
#' @param ensemble_fun Aggregation function used when ensemble = TRUE.
#' Defaults to the mean.
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
#' 
#' # Reanalysis
#' rean_path <- paste0(getwd(), "/inst/extdata/reanalysis/")
#' rean <- clim4health_load(rean_path,
#'                          var = "t2m",
#'                          data_type = "obs",
#'                          date = c("201001", "201002", "201003"),
#'                          ext = 'nc')
#' clim4health_plot(rean, time = 1) 
#' 
#' # Hindcast
#' hind_path <- paste0(getwd(), "/inst/extdata/hindcast/")
#' hind <- clim4health_load(hind_path,
#'                          var = "t2m",
#'                          data_type = "hcst",
#'                          date = c("20100101", "20110101", "20120101"),
#'                          ext = 'nc')
#' clim4health_plot(rean, time = 1, sdate = 1) 
clim4health_plot <- function(data,
                             sdate = NULL,
                             time = NULL,
                             ensemble = TRUE,
                             ensemble_fun = mean,
                             save_path = NULL,
                             legend = NULL){
  
  # 1. Data checks ----
  
  # Get array info
  dims_data <- data$dims
  dates_data <- data$attrs$Dates
  plot_data <- data$data

  # Requirements: var
  if(is.null(var) & dims_data["var"]>1){
    stop("More than one 'var' is available, please select one.")
  }else if(!is.null(var) & length(var)>1){
    stop("Only one 'var' is possible.")
  }
  
  # Requirements: sdate
  if(is.null(sdate) & dims_data["sdate"]>1){
    stop("More than one 'sdate' is available, please select one.")
  }else if(!is.null(sdate) & length(sdate)>1){
    stop("Only one 'sdate' is possible for now.")
  }
  
  # Requirements: time
  if(is.null(time) & dims_data["time"]>1){
    stop("More than one 'time' is available, please select one.")
  }else if(!is.null(time) & length(time)>1){
    stop("Only one 'time' is possible for now.")
  }
  
  # Requirements: ensemble
  if(length(ensemble)>1 & dims_data["ensemble"]>1){
    stop("More than one 'ensemble' member is available, please select one or
         set ensemble=TRUE to compute an aggregation.")
  }
  
  # 2. Data preparation ----

  # Select 'sdate' if needed
  if(!is.null(sdate)){
    plot_data <- .subset_array_named(plot_data, sdate = sdate)
  }
  
  # Select 'time' if needed
  if(!is.null(time)){
    plot_data <- .subset_array_named(plot_data, time = time)
  }

  # Aggregate ensemble members if needed
  if((dims_data["ensemble"] > 1) & isTRUE(ensemble)){
    message(paste0("Aggregating ensemble members with the function: ", 
                   deparse(substitute(ensemble_fun))))
    keep_margins <- which(names(dims_data) != "ensemble")
    plot_data <- apply(plot_data, MARGIN = keep_margins, FUN = ensemble_fun)
    
    # Select ensemble member
  }else if(is.numeric(ensemble)){
    plot_data <- .subset_array_named(plot_data, ensemble = ensemble)
  }
  
  # 3. Plot ---
  print(dim(plot_data))
  
  # Save file if needed
  if(!is.null(save_path)){
    s2dv::PlotEquiMap(var = plot_data,
                      lon = c(data$coords$longitude), 
                      lat = c(data$coords$latitude),
                      filled.continents = FALSE,
                      country.borders = TRUE,
                      fileout = save_path,
                      colNA = "grey90")
  }
  
  # Plot
  out_plot <- s2dv::PlotEquiMap(var = plot_data,
                                lon = c(data$coords$longitude), 
                                lat = c(data$coords$latitude),
                                filled.continents = FALSE,
                                country.borders = TRUE,
                                colNA = "grey90")
  
  # Return
  return(invisible(out_plot))
}

