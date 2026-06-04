#' c4h_save
#'
#' Save objects manipulated using clim4health functions
#' in different formats: s2dv, terra, csv, polygons.
#'
#' @param data The object you want to save, which could be an s2dv_cube,
#' terra, data.frame or sf object.
#' @param format A string indicating the format of the object you want to save.
#' Options are: "s2dv", "terra", "csv" or "polygons"
#' @param path A string indicating the path to save the file.
#' @param drop Logical. Only relevant for "s2dv" format. For other data formats,
#' please use the drop parameter in c4h_convert. If TRUE, dimensions
#' with only one value will be dropped when saving. If FALSE, they will be
#' retained. Default is FALSE.
#'
#' @returns None
#' @export
#'
#' @examples
#' \donttest{
#' fcst_path <- system.file("extdata", "forecast", package = "clim4health")
#' fcst <- c4h_load(fcst_path, variable = "t2m", ext = 'nc')
#'
#' c4h_save(fcst, "s2dv", "out.nc")
#' file.remove("out.nc")
#' }
c4h_save <- function(data, format, path, drop = FALSE) {
  # checks
  if (!inherits(data, "s2dv_cube") && format == "s2dv") {
    stop("Data must be an s2dv_cube for format 's2dv'.")
  } else if (!inherits(data, "SpatRaster") && format == "terra") {
    stop("Data must be a SpatRaster for format 'terra'.")
  } else if (!inherits(data, "sf") && format == "polygons") {
    stop("Data must be an sf object for format 'polygons'.")
  } else if (!inherits(data, "data.frame") && format == "csv") {
    stop("Data must be a data.frame for format 'csv'.")
  }
  if (
    format == "s2dv" &&
      !(all(
        c("longitude", "latitude") %in%
          names(data$dims)
      ))
  ) {
    stop(paste0(
      "s2dv_cube must have 'longitude' and 'latitude' dimensions to ",
      "be saved in 's2dv' format. Consider converting to 'terra', ",
      "'csv', or 'polygons' format using c4h_convert()."
    ))
  }

  if (format == "s2dv") {
    .save_s2dv(data, path, drop = drop)
  } else if (format == "terra") {
    .save_terra(data, path)
  } else if (format == "csv") {
    .save_csv(data, path)
  } else if (format == "polygons") {
    .save_polygons(data, path)
  }
}
