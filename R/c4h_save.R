#' c4h_save
#'
#' Save *s2dv_cube* objects used in `clim4health` functions to disk.
#'
#' @param data The *s2dv_cube* object to be saved.
#' @param path A string indicating the path to save the file. The driver to
#' save the file will be deduced based on the file extension. See details for
#' a list of available extensions for each data type.
#' @param drop Logical. If TRUE, dimensions  with only one value will
#' be dropped when saving. If FALSE (defaults), they will be retained.
#'
#' @returns None.
#' @details The list of available extensions is the following:
#'   \describe{
#'     \item{Gridded data}{'nc', 'tif', 'csv'.}
#'     \item{Areal data}{'gpkg', 'shp'.}
#'     \item{Point data}{'csv'.}
#'   }
#' Note that when saving s2dv data to formats other than 'nc', an
#' internal conversion using [c4h_convert] is applied.
#'
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
c4h_save <- function(data, path, drop = FALSE) {
  # s2dv checks
  if (!inherits(data, "s2dv_cube")) {
    stop("Data must be an s2dv_cube.")
  }
  dim_names <- names(data$dims)
  has_latlon <- all(c("latitude", "longitude") %in% dim_names)
  has_area <- "area" %in% dim_names
  has_loc <- "location" %in% dim_names
  if ((has_latlon + has_area + has_loc) != 1L) {
    stop("Invalid s2dv_cube object")
  }

  # get and check file extension
  extpath <- tools::file_ext(path)
  if (!extpath %in% c("nc", "tif", "csv", "gpkg", "shp")) {
    stop(
      "Invalid file extension. Please check available extensions ",
      "in the documentation."
    )
  }

  # Gridded data
  if (has_latlon) {
    if (extpath == "nc") {
      .save_s2dv(data, path, drop = drop)
    } else if (extpath == "tif") {
      data_conv <- c4h_convert(data, "terra", drop = drop)
      terra::writeRaster(data_conv, filename = path, overwrite = TRUE)
    } else if (extpath == "csv") {
      data_conv <- c4h_convert(data, "data.frame", drop = drop)
      utils::write.csv(data_conv, file = path, row.names = FALSE)
    } else {
      stop("Invalid extension for gridded data.")
    }
  }

  # Areal data
  if (has_area) {
    if (extpath %in% c("gpkg", "shp")) {
      data_conv <- c4h_convert(data, "sf", drop = drop)
      sf::st_write(data_conv, dsn = path, append = FALSE, quiet = TRUE)
    } else {
      stop("Invalid extension for areal data.")
    }
  }

  # Point data
  if (has_loc) {
    if (extpath %in% c("csv")) {
      data_conv <- c4h_convert(data, "data.frame", drop = drop)
      utils::write.csv(data_conv, file = path, row.names = FALSE)
    } else {
      stop("Invalid extension for point data.")
    }
  }
}
