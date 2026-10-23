#' Convert s2dv_cube objects to other data formats.
#'
#' @description
#' Convert *s2dv_cube* objects to *terra*, *stars*, *data.frame*, and *sf*.
#'
#' @param data The s2dv_cube object to transform.
#' @param output_format A string specifying the desired output format.
#' Valid options are 'terra' (SpatRaster), 'stars', 'data.frame' or
#' 'sf' (polygons).
#' @param crs Coordinate reference system to use for output_formats 'terra',
#' 'sf', and 'stars'. The default is NULL, in which case the CRS will be taken
#' from the area geometries if they have one, or assumed to be WGS 84
#' (EPSG:4326) otherwise (e.g. for gridded data). It is ignored for the
#' 'data.frame' format.
#' @param drop Logical. If TRUE, singleton non-spatial dimensions with only
#' one value will be dropped when converting to the desired format.
#' Default is FALSE.
#'
#' @returns An R object of type terra `SpatRaster`, `stars`, data.frame or
#' `sf` polygons.
#'
#' @details
#' The input must be an `s2dv_cube`. The type of spatial data is detected from
#' the names of its dimensions, and determines which output formats are
#' available:
#'   \describe{
#'     \item{Gridded data}{Has `latitude` and `longitude` dimensions. Can be
#'     converted to 'terra' (SpatRaster), 'stars', 'sf' (polygons, one per grid
#'     cell) or 'data.frame'.}
#'     \item{Areal data}{Has an `area` dimension, with the polygon geometries
#'     stored in `attrs$area$geom`. Can be converted to 'sf' (polygons) or
#'     'data.frame'.}
#'     \item{Point data}{Has a `location` dimension, with coordinates stored
#'     in `attrs$location`. Can only be converted to 'data.frame'.}
#'   }
#' Requesting a format that is not available for the data type results in an
#' error.
#'
#' The structure of the output depends on the format:
#'   \describe{
#'     \item{terra}{A `SpatRaster` with one layer per combination of
#'     `dataset`, `var`, `sdate`, `time` and `ensemble`. Layers are named by
#'     joining the labels of these dimensions (e.g.
#'     `dataset_1_var_t2m_sdate_20250101_time_20250101_ensemble_0`). Dates are
#'     used for the `sdate` and `time` labels when available. Latitudes stored
#'     from north to south are reversed.}
#'     \item{stars}{A `stars` object with `longitude` and `latitude` as the
#'     first two dimensions, followed by the remaining dimensions of the cube.
#'     The attribute is named after the variable of the cube.}
#'     \item{sf}{An `sf` object with one polygon per grid cell or area and one
#'     column per layer, named as for 'terra'.}
#'     \item{data.frame}{A long-format table with one row per value. It
#'     contains one column per dimension (as indices), a `value` column and,
#'     when available, `date`, `var_name` and `ensemble_value` columns. Point
#'     data also include `latitude` and `longitude` columns, and areal data
#'     the area names.}
#'   }
#'
#' If `drop = TRUE`, singleton `dataset`, `var` and `ensemble` dimensions are
#' removed from the layer names ('terra', 'sf') or from the columns
#' ('data.frame'), and singleton dimensions are dropped for 'stars'.
#'
#' @seealso [c4h_save] to save s2dv_cube objects, possibly to other formats
#' using [c4h_convert].
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(sf)
#'
#' # Load example forecast data
#' fcst_path <- system.file("extdata", "forecast", package = "clim4health")
#' fcst <- c4h_load(fcst_path, variable = "t2m", leadtime_month = 1, ext = "nc")
#'
#' # load polygon data and aggregate forecast
#' munip_path <- system.file("extdata", "areas", "munip_vallecauca.gpkg",
#'                           package = "clim4health")
#' munip <- read_sf(munip_path)
#' munip <- munip[1:2, ]
#' fcst_aggr <- c4h_space(fcst, munip)
#'
#' # load station data
#' station_path <- system.file("extdata", "stations", "temp_vallecauca.csv",
#'                             package = "clim4health")
#' stations <- c4h_load(station_path, variable = "temp_mean", ext = "csv")
#'
#'
#' ### Convert gridded data
#' ## to terra
#' out <- c4h_convert(fcst, "terra")
#' ## to data.frame
#' out <- c4h_convert(fcst, "data.frame")
#' ## to sf
#' out <- c4h_convert(fcst, "sf")
#'
#'
#' ### Convert polygon data
#' ## to data.frame
#' out <- c4h_convert(fcst_aggr, "data.frame")
#' ## to sf
#' out <- c4h_convert(fcst_aggr, "sf")
#'
#' ### Convert point data
#' ## to data.frame
#' out <- c4h_convert(stations, "data.frame")
#' }
c4h_convert <- function(data, output_format, crs = NULL, drop = FALSE) {
  # 1. Input checks ----
  if (!inherits(data, "s2dv_cube")) {
    stop("'data' must be an s2dv_cube.")
  }
  .validate_s2dv_cube(data)
  spatial_type <- .s2dv_spatial_type(data)

  if (
    !is.character(output_format) ||
      length(output_format) != 1 ||
      !output_format %in% c("terra", "stars", "data.frame", "sf")
  ) {
    stop(
      "Invalid output format. Choose 'terra', 'stars', 'data.frame' or 'sf'."
    )
  }

  if (!is.logical(drop) || length(drop) != 1 || is.na(drop)) {
    stop("'drop' must be a single logical value.")
  }

  if (spatial_type != "gridded" && output_format %in% c("terra", "stars")) {
    stop("Gridded data is required for 'terra' and 'stars' formats.")
  }

  if (!spatial_type %in% c("gridded", "area") && output_format == "sf") {
    stop("Gridded or polygon data is required for 'sf' output format.")
  }

  crs <- .resolve_crs(data, crs, output_format, spatial_type)

  # 2. Conversion ----
  switch(
    output_format,
    terra = .s2dv_to_terra(data, crs = crs, drop = drop),
    stars = .s2dv_to_stars(data, crs = crs, drop = drop),
    data.frame = .s2dv_to_df(data, drop = drop),
    sf = .s2dv_to_sf(data, crs = crs, drop = drop),
    stop("Invalid output format.")
  )
}
