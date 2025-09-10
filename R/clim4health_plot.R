#' Plot *s2dv* climate data
#' 
#' @description ONLY WORKS WITH GRID DATA COMING FROM clim4health_load
#'
#' @param data *s2dv_cube* object with the climate data to be plotted.
#' @param var Numerical index of the variable to be plotted. 
#' Only required if more than one variable is available.
#' @param sdate Numerical index of the forecast starting time to be plotted. Defaults 
#' to NULL (all starting dates).
#' @param time Numerical index of the time to be plotted. Defaults to NULL (all 
#' times).
#' @param ensemble Numerical index of the ensemble member to be plotted. If
#' set to TRUE, it aggregates all the ensemble members. Defaults to NULL (all 
#' times).
#' @param boundaries Polygon boundaries to add to the map. Defaults to "countries"
#' for country boundaries. Set to NULL for no boundaries, otherwise supply
#' a polygon `sf` object.
#' @param mask_boundaries If TRUE (default), mask the data to the provided 
#' boundaries.
#' @param ensemble_fun Aggregation function used when ensemble = TRUE.
#' Defaults to the mean.
#' @param title Optional plot title.
#' @param legend Optional legend title. If NULL (default), it uses the variable
#' names.
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
#'                          variable = "t2m",
#'                          ext = 'nc')
#' clim4health_plot(fcst, title = "Forecast") 
#' 
#' # Reanalysis
#' rean_path <- paste0(getwd(), "/inst/extdata/reanalysis/")
#' rean <- clim4health_load(rean_path,
#'                          variable = "t2m",
#'                          ext = 'nc')
#' clim4health_plot(rean, title = "Reanalysis") 
#' 
#' # Hindcast
#' hind_path <- paste0(getwd(), "/inst/extdata/hindcast/")
#' hind <- clim4health_load(hind_path,
#'                          variable = "t2m",
#'                          ext = 'nc')
#' clim4health_plot(hind, title = "Hindcast", ensemble = TRUE) 
clim4health_plot <- function(data,
                             var = NULL,
                             sdate = NULL,
                             time = NULL,
                             ensemble = NULL,
                             ensemble_fun = mean,
                             boundaries = NULL,
                             mask_boundaries = FALSE,
                             title = NULL,
                             legend = NULL,
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
  }else if(is.null(ensemble)){
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
  
  # Requirements: boundaries 
  if(!is.null(boundaries)){
    if(boundaries != "countries" & !inherits(boundaries, "sf")){
      stop("'boundaries' must be set to either 'countries' or be a polygon 'sf' object.")
    }
  }
  
  # Number of panels
  if(length(time) * length(sdate) * length(ensemble) > 30){
    warning(paste0("The number of panels is very large, consider selecting a smaller",
                   "set of times/ensemble members/dates."))
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
  
  # Select boundaries if needed
  if(!is.null(boundaries)){
    if(boundaries == "countries"){
      data(world, package = "spData")
      bound_sf <- world
    }else if(inherits(boundaries, "sf")){
      if(sf::st_crs(boundaries)$epsg != 4326){
        message("Transforming 'boundaries' to CRS EPSG 4326")
        bound_sf <- sf::st_transform(boundaries, crs = 4326)
      } else{
        bound_sf <- boundaries
      }
    }
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
  plot_stars <- stars::st_set_dimensions(plot_stars, "ensemble",
                                         values = c(data$coords$ensemble)[ensemble])
  plot_stars <- stars::st_set_dimensions(plot_stars, "time",
                                         values = c(data$coords$time)[time])
  sf::st_crs(plot_stars) <- 4326
  
  # Mask if needed
  if(isTRUE(mask_boundaries) & !is.null(boundaries)){
    sf::sf_use_s2(FALSE) 
    suppressMessages(plot_stars <- plot_stars[bound_sf])
    sf::sf_use_s2(FALSE) 
  }
  
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
  p <- p + ggplot2::scale_fill_gradientn(colors = my_palette, na.value = "grey70")  
  
  # Boundaries
  if(!is.null(boundaries)){
    p <- p +
      ggplot2::geom_sf(data = bound_sf, alpha = 0, 
                       col = "black", linewidth = 0.4) +
      suppressMessages(ggplot2::coord_sf(
        xlim = sf::st_bbox(plot_stars)[c("xmin", "xmax")],
        ylim = sf::st_bbox(plot_stars)[c("ymin", "ymax")],
        expand = FALSE))
    
  }
  
  # NA box
  if(any(is.na(plot_stars[[1]]))){
    
    p <- p +
      ggplot2::geom_point(data = data.frame(lab = "NA"), x = NA, y = NA,
                          fill = NA, size = 7, shape = 15, na.rm = TRUE,
                          ggplot2::aes(color = .data$lab), show.legend = TRUE) +
      
      ggplot2::scale_color_manual(values = c("NA" = "grey70"),
                                  guide = ggplot2::guide_legend(
                                    override.aes = list(fill = "grey70",
                                                        color = "grey20"))) +
      ggplot2::labs(color = "") +
      ggplot2::guides(fill = ggplot2::guide_colorbar(order = 1),
                      color = ggplot2::guide_legend(order = 2)) +
      ggplot2::theme(legend.key = ggplot2::element_blank())
  }

  # Theme
  p <- p +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::theme_bw()$plot.margin,
      strip.text.y.left = ggplot2::element_text(angle = 90),
      legend.key.height = ggplot2::unit(1, "cm"),
      plot.title = ggplot2::element_text(hjust = 0.5, 
                                         margin = ggplot2::margin(b = 5)))
  
  # Labels
  if(!is.null(title)){
    p <- p + ggplot2::ggtitle(title)
  }
  if(!is.null(legend)){
    p <- p + ggplot2::labs(fill = legend)
  }
  if((!"sdate" %in% c(f1, f2)) & ("time" %in% c(f1, f2))) {
    p <- p + 
      ggplot2::labs(subtitle =  paste0(
        "Start date: ",
        stars::st_get_dimension_values(plot_stars, "sdate")))
  }
    
  return(p)
}