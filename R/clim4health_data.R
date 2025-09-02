#' Example datasets included in *clim4health*
#'
#' These datasets are stored in `inst/extdata/` and are accessed
#' through functions such as [clim4health_load()] or via
#' `system.file("extdata", "file.csv", package = "clim4health")`.
#'
#' @section Station and administrative data:
#' - `stations/temp_vallecauca.csv`: Table data containing daily temperature 
#' station observations for the "Valle del Cauca" department in Colombia (2010-2012).
#' The raw hourly data have been aggregated to daily measurements whenever at least a 70%
#' of the hourly observations were available for a given day. Source: IDEAM (2025).
#' - `areas/munip_vallecauca.gpkg`: Geopackage containing the municipality 
#' boundaries of the "Valle del Cauca" department in Colombia. Source: IGAC (2025).
#'
#' @section Climate forecasts:
#' - TBC
#' 
#' @section Climate hindcasts:
#' - TBC
#'  
#' @section Climate reanalysis:
#' - TBC
#' 
#' @seealso [clim4health_load()]
#' @name clim4health_data
NULL