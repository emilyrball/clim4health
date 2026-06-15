#' Example datasets included in *clim4health*
#'
#' These datasets are stored in `inst/extdata/` and are accessed
#' through functions such as [c4h_load] or via
#' `system.file("extdata", "file.csv", package = "clim4health")`.
#'
#' @section Station and administrative data:
#' - `stations/temp_vallecauca.csv`: Table data containing daily temperature
#' station observations for the "Valle del Cauca" department in Colombia
#' (2010-2012). The raw hourly data have been aggregated to daily measurements
#' whenever at least a 70% of the hourly observations were available for a
#' given day. Source: IDEAM (2025).
#' - `areas/munip_vallecauca.gpkg`: Geopackage containing the municipality
#' boundaries of the "Valle del Cauca" department in Colombia. Source: IGAC
#' (2025).
#'
#' @section Climate forecasts:
#' - `forecast/t2m_20250101.nc`: 3-month monthly-averaged seasonal forecast
#' (ECMWF SEAS5.1 seasonal forecast) including 2-metre temperature (K, 51
#' ensemble members, 1º spatial resolution) issued in January 2025.
#' The spatial extent covers the "Valle del Cauca" department in Colombia.
#'
#' @section Climate hindcasts:
#' - `hindcast/t2m_*.nc`: 3-month monthly-averaged seasonal hindcast
#' (ECMWF SEAS5.1 seasonal forecast) including 2-metre temperature (K, 25
#' ensemble members, 1º spatial resolution)) issued in January 2010-2012.
#' The spatial extent covers the "Valle del Cauca" department in Colombia.
#'
#' @section Climate reanalysis:
#' - `reanalysis/t2m_*.nc`: monthly-averaged ERA5-Land reanalysis product
#' including 2-metre temperature (K, 0.1º spatial resolution) for the months of
#' January, February and March 2010-2012.
#' The spatial extent covers the "Valle del Cauca" department in Colombia.
#'
#' @seealso [c4h_load]
#' @name c4h_data
NULL
