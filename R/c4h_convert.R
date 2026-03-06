#' c4h_convert
#'
#' Convert objects between formats including s2dv, terra, csv, polygons.
#'
#' @param data The object to transform
#' @param output_format A string specifying the desired output format.
#' Valid options are 's2dv', 'terra', 'csv' or 'polygons'
#'
#' @returns An R object of type terra SpatRaster, data.frame or sf polygons.
#' @export
#'
#' @examples
#' # Load example forecast data
#' fcst_path <- system.file("extdata", "forecast", package = "clim4health")
#' fcst <- c4h_load(fcst_path, variable = "t2m", ext = 'nc')
#'
#' ### Convert s2dv into terra
#' # out <- c4h_convert(fcst,
#' #                    "terra")
#' ### Convert s2dv into csv
#' # out <- c4h_convert(fcst,
#' #                    "csv")
#' ## REVIEW - more examples
#' # s2dv gridded to terra - done
#' # s2dv gridded to csv - done
#' # s2dv point to csv
#'
#' ### Polygon examples
#' # library(sf)
#' #
#' # # Region municipalities
#' # munip_path <- system.file("extdata", "areas", "munip_vallecauca.gpkg",
#' #                         package = "clim4health")
#' # munip <- read_sf(munip_path)
#' # fcst_aggr <- c4h_space(fcst, munip)
#'
#' # # s2dv polygon to csv
#' # out <- c4h_convert(fcst_aggr,
#' #                    "csv")
#' # s2dv polygon to polygons
#'
#' ### Station data example
#' # station_path <- system.file("extdata", "stations", "temp_vallecauca.csv",
#' #                        package = "clim4health")
#' # stations <- c4h_load(station_path, variable = "temp_mean", ext = 'csv')
#' # out <- c4h_convert(stations,
#' #                    "csv")
c4h_convert <- function(data,
                        output_format) {
  if (output_format == "terra") {
    .s2dv_to_terra(data) ### REVIEW crs argument unused

  } else if (output_format == "csv") {
    .s2dv_to_csv(data) ### REVIEW file argument unused

  } else if (output_format == "polygons") {
    .s2dv_to_polygons(data) ### REVIEW crs argument unused
  }
}