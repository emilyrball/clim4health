#' c4h_convert
#'
#' Convert *s2dv_cube* objects to *terra*, *data.frame*, and *sf*.
#'
#' @param data The object to transform.
#' @param output_format A string specifying the desired output format.
#' Valid options are 'terra' (SpatRaster), 'data.frame' or 'sf' (polygons).
#' @param crs Coordinate reference system to use for output_formats 'terra' and
#' 'sf'. The default is NULL, in which case the CRS will be taken
#' from  the input data if available, or assumed to be WGS 84 (EPSG:4326) for
#' gridded data.
#' @param drop Logical. If TRUE, dimensions with only one value will be dropped
#' when converting to the desired format. Default is FALSE.
#'
#' @returns An R object of type terra `SpatRaster`, data.frame or `sf` polygons.
#'
#' @details The function only allows to convert into the following classes
#' depending on the data format.
#'   \describe{
#'     \item{Gridded data}{'terra' SpatRaster, 'sf' polygons, or 'data.frame'.}
#'     \item{Areal data}{'sf' polygons or 'data.frame'.}
#'     \item{Point data}{'data.frame'.}
#'   }
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Load example forecast data
#' fcst_path <- system.file("extdata", "forecast", package = "clim4health")
#' fcst <- c4h_load(fcst_path, variable = "t2m", leadtime_month = 1, ext = 'nc')
#'
#' ############################
#' ### Convert gridded data
#' ## to terra
#' out <- c4h_convert(fcst, "terra", crs = "EPSG:4326")
#' ## to data.frame
#' out <- c4h_convert(fcst, "data.frame")
#' ## to sf
#' out <- c4h_convert(fcst, "sf", crs = "EPSG:4326")
#'
#' #############################
#' ##### load polygon data #####
#' library(sf)
#' # Region municipalities
#' munip_path <- system.file("extdata", "areas", "munip_vallecauca.gpkg",
#'                           package = "clim4health")
#' munip <- read_sf(munip_path)
#' munip <- munip[1:2, ]
#' fcst_aggr <- c4h_space(fcst, munip)
#'
#' ### Convert polygon data
#' ## to data.frame
#' out <- c4h_convert(fcst_aggr, "data.frame")
#' ## to sf
#' out <- c4h_convert(fcst_aggr, "sf", crs = "EPSG:4326")
#'
#' #############################
#' ##### load station data #####
#' station_path <- system.file("extdata", "stations", "temp_vallecauca.csv",
#'                             package = "clim4health")
#' stations <- c4h_load(station_path, variable = "temp_mean", ext = 'csv')
#'
#' ### Convert location data
#' ## to data.frame
#' out <- c4h_convert(stations, "data.frame")
#' }
c4h_convert <- function(data, output_format, crs = NULL, drop = FALSE) {
  if (!output_format %in% c("terra", "data.frame", "sf")) {
    stop("Invalid output format. Choose 'terra', 'data.frame' or 'sf'.")
  }

  if (!inherits(drop, "logical") || length(drop) != 1) {
    stop("'drop' must be a single logical value.")
  }

  if (inherits(data, "s2dv_cube")) {
    # Spatial coords
    dim_names <- names(data$dims)
    has_latlon <- all(c("latitude", "longitude") %in% dim_names)
    has_area <- "area" %in% dim_names
  } else {
    stop("'data' must be an s2dv_cube.")
  }

  if (!has_latlon && output_format == "terra") {
    stop("Gridded data is required for 'terra' output format.")
  }

  if (
    !(has_latlon || has_area) &&
      output_format == "sf"
  ) {
    stop("Gridded or polygon data is required for 'sf' output format.")
  }

  if (is.null(crs) && output_format %in% c("terra", "sf")) {
    if (has_area) {
      message(paste0(
        "'crs' is not provided. Taking CRS from",
        "the input data if available."
      ))
      crs <- sf::st_crs(data$attrs$area$geom)
    } else if (has_latlon) {
      message(paste0(
        "'crs' is not provided. Assuming WGS 84 ",
        "(EPSG:4326) for gridded data."
      ))
      crs <- "EPSG:4326"
    }
  }

  if (output_format == "terra") {
    .s2dv_to_terra(data, crs = crs, drop = drop)
  } else if (output_format == "data.frame") {
    .s2dv_to_df(data, drop = drop)
  } else if (output_format == "sf") {
    .s2dv_to_sf(data, crs = crs, drop = drop)
  }
}
