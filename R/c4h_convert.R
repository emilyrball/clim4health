#' c4h_convert
#'
#' Convert objects between formats including s2dv, terra, csv, polygons.
#'
#' @param data The object to transform
#' @param output_format A string specifying the desired output format.
#' Valid options are 'terra', 'csv' or 'polygons'
#' @param crs Coordinate reference system to use for output_formats 'terra' and
#' 'polygons'. The default is NULL, in which case the CRS will be taken from
#' the input data if available, or assumed to be WGS 84 (EPSG:4326) for gridded
#' data.
#'
#' @returns An R object of type terra SpatRaster, data.frame or sf polygons.
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
#' out <- c4h_convert(fcst,
#'                    "terra", crs = "EPSG:4326")
#' ## to csv
#' out <- c4h_convert(fcst,
#'                    "csv")
#' ## to polygons
#' out <- c4h_convert(fcst,
#'                    "polygons", crs = "EPSG:4326")
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
#' ## to csv
#' out <- c4h_convert(fcst_aggr,
#'                    "csv")
#' ## to polygons
#' out <- c4h_convert(fcst_aggr, "polygons", crs = "EPSG:4326")
#'
#' #############################
#' ##### load station data #####
#' station_path <- system.file("extdata", "stations", "temp_vallecauca.csv",
#'                             package = "clim4health")
#' stations <- c4h_load(station_path, variable = "temp_mean", ext = 'csv')
#'
#' ### Convert location data
#' ## to csv
#' out <- c4h_convert(stations, "csv")
#' }
c4h_convert <- function(data,
                        output_format,
                        crs = NULL) {
  if (!output_format %in% c("terra", "csv", "polygons")) {
    stop("Invalid output format. Choose 'terra', 'csv' or 'polygons'.")
  }

  if (inherits(data, "s2dv_cube")) {
    # Spatial coords
    dim_names <- names(data$dims)
    has_latlon  <- all(c("latitude", "longitude") %in% dim_names)
    has_location <- "location" %in% dim_names
    has_area <- "area" %in% dim_names
  } else {
    stop("'data' must be an s2dv_cube.")
  }

  if (!has_latlon && output_format == "terra") {
    stop("Gridded data is required for 'terra' output format.")
  }

  if (!(has_latlon || has_area) &&
        output_format == "polygons") {
    stop("Gridded or polygon data is required for 'polygons' output format.")
  }

  if (is.null(crs) && output_format %in% c("terra", "polygons")) {
    if (has_area) {
      message(paste0("'crs' is not provided. Taking CRS from",
                     "the input data if available."))
      crs <- sf::st_crs(data$attrs$area$geom)
    } else if (has_latlon) {
      message(paste0("'crs' is not provided. Assuming WGS 84 ",
                     "(EPSG:4326) for gridded data."))
      crs <- "EPSG:4326"
    }
  }

  if (output_format == "terra") {
    .s2dv_to_terra(data, crs = crs)

  } else if (output_format == "csv") {
    .s2dv_to_csv(data)

  } else if (output_format == "polygons") {
    .s2dv_to_polygons(data, crs = crs)
  }
}