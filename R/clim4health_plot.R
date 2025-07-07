#' Plot *s2dv* climate data
#' 
#' @description ONLY WORKS WITH GRID DATA COMING FROM clim4health_load
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
#' @param palette GHR, RColorBrewer or colorspace palette. Use "-" before the 
#' palette name (e.g., "-Reds") to reverse it. Alternatively, a vector of
#' hex codes to create a continuous interpolated palette.
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
#' clim4health_plot(fcst) 
#' 
#' # Reanalysis
#' rean_path <- paste0(getwd(), "/inst/extdata/reanalysis/")
#' rean <- clim4health_load(rean_path,
#'                          var = "t2m",
#'                          data_type = "obs",
#'                          date = c("201001", "201002", "201003"),
#'                          ext = 'nc')
#' clim4health_plot(rean) 
#' 
#' # Hindcast
#' hind_path <- paste0(getwd(), "/inst/extdata/hindcast/")
#' hind <- clim4health_load(hind_path,
#'                          var = "t2m",
#'                          data_type = "hcst",
#'                          date = c("20100101", "20110101", "20120101"),
#'                          ext = 'nc')
#' clim4health_plot(hind) 
clim4health_plot <- function(data,
                             var = NULL,
                             sdate = NULL,
                             time = NULL,
                             ensemble = TRUE,
                             ensemble_fun = mean,
                             palette = "-RdBu"){
  
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
  if(!is.null(sdate) & !is.numeric(sdate)){
    stop("'sdate' must be of type numeric.")
  }else if(!is.null(sdate) & any(sdate > dims_data["sdate"])){
    stop(paste0("A wrong 'sdate' has been selected (possible range: 1 - ",
                dims_data["sdate"], ")."))
  }else if(is.null(sdate)){
    sdate <- 1:dims_data["sdate"]
  }
  
  # Requirements: time
  if(!is.null(time) & !is.numeric(time)){
    stop("'time' must be of type numeric.")
  }else if(!is.null(time) & any(time > dims_data["time"])){
    stop(paste0("A wrong 'time' has been selected (possible range: 1 - ",
                dims_data["time"], ")."))
  }else if(is.null(time)){
    time <- 1:dims_data["time"]
  }
  
  # Requirements: ensemble
  if(!is.null(ensemble) & !isTRUE(ensemble) & !is.numeric(ensemble)){
    stop("'ensemble' must be either TRUE, NULL, or of type numeric.")
  }else if(!is.null(ensemble) & !isTRUE(ensemble) & any(ensemble > dims_data["ensemble"])){
    stop(paste0("A wrong 'ensemble' has been selected (possible range: 1 - ",
                dims_data["time"], ")."))
  }else if(is.null(time)){
    ensemble <- 1:dims_data["ensemble"]
  }
  
  # Only two dimensions can have length > 1
  if(length(sdate)>1 & length(time)>1 & length(ensemble)>1){
    stop(paste0("Only two out of 'sdate', 'time' and 'ensemble' can have a ",
                "length larger than one."))
  }else{
    
    # Faceting requirements
    facet_vars <- list(sdate = sdate, time = time, ensemble = ensemble)
    facet_vars <- names(facet_vars)[sapply(facet_vars, function(x) length(x) > 1)]
    f1 <- if (length(facet_vars) >= 1) facet_vars[1] else NULL
    f2 <- if (length(facet_vars) >= 2) facet_vars[2] else NULL
  }
  
  
  # 2. Data preparation ----

  # Select 'sdate' if needed
  if(all(sdate %in% 1:dims_data["sdate"])){
    plot_data <- .subset_array_named(plot_data, sdate = sdate)
  }
  
  # Select 'time' if needed
  if(all(time %in% 1:dims_data["time"])){
    plot_data <- .subset_array_named(plot_data, time = time)
  }

  # Aggregate ensemble members if needed
  if((dims_data["ensemble"] > 1) & isTRUE(ensemble)){
    message(paste0("Aggregating ensemble members with the function: ", 
                   deparse(substitute(ensemble_fun))))
    keep_margins <- which(names(dims_data) != "ensemble")
    plot_data <- apply(plot_data, MARGIN = keep_margins, FUN = ensemble_fun)
    dim(plot_data) <- c(dim(plot_data), "ensemble"=1)
    ensemble <- 1
    
    # Select ensemble member
  }else if(all(ensemble %in% 1:dims_data["ensemble"])){
    plot_data <- .subset_array_named(plot_data, ensemble = ensemble)
  }
  
  
  # 3. Conversion to stars ----
  spatial_dims <- c(which(names(dim(plot_data)) == "longitude"),
                    which(names(dim(plot_data)) == "latitude"))
  plot_stars <- aperm(plot_data, c(spatial_dims, setdiff(1:length(dims_data), spatial_dims)))
  plot_stars <- stars::st_as_stars(plot_stars)
  names(plot_stars) <- data$attrs$Variable$varName
  plot_stars <- stars::st_set_dimensions(plot_stars, "longitude",
                                         values = c(data$coords$longitude))
  plot_stars <- stars::st_set_dimensions(plot_stars, "latitude",
                                         values = c(data$coords$latitude))
  plot_stars <- stars::st_set_dimensions(plot_stars, "sdate",
                                         values = c(data$coords$sdate)[sdate])
  plot_stars <- stars::st_set_dimensions(plot_stars, "time",
                                         values = c(data$coords$time)[time])
  plot_stars <- stars::st_set_dimensions(plot_stars, "ensemble",
                                         values = c(data$coords$ensemble)[ensemble])
  sf::st_crs(plot_stars) <- 4326
  
  # 4. Plot ----
  
  # Create base figure
  if(is.null(f1) & is.null(f2)){ # No faceting
    p <- ggplot2::ggplot() +
      stars::geom_stars(data = plot_stars)
  }else if(!is.null(f1) & is.null(f2)){ # Wrap
    p <- ggplot2::ggplot() +
      stars::geom_stars(data = plot_stars) +
      ggplot2::facet_wrap(as.formula(paste("~", f1)),
                          labeller = "label_both")
  }else if(!is.null(f1) & !is.null(f2)){ # Wrap
    p <- ggplot2::ggplot() +
      stars::geom_stars(data = plot_stars) +
      ggplot2::facet_grid(as.formula(paste(f1, "~", f2)),
                          labeller = "label_both", switch = "y")
  }
  
  # Palette
  my_palette <- GHRexplore::GHR_palette(palette)(n = 30)
  p <- p + ggplot2::scale_fill_gradientn(colors = my_palette)

  # Theme specs
  p <- p +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::theme_bw()$plot.margin,
      strip.text.y.left = ggplot2::element_text(angle = 90),
      legend.key.height = ggplot2::unit(1, "cm")
    )
    
  # 5. Return ----
  return(p)
}