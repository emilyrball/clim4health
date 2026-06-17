#' .save_s2dv
#'
#' Save an object of class s2dv_cube to a NetCDF file.
#'
#' @param cube The s2dv_cube object to be saved.
#' @param path A string indicating the path to save the NetCDF file.
#' @param drop Logical. If TRUE, dimensions with only one value will be dropped
#' when converting to the desired format. If FALSE, they will be retained.
#' Default is FALSE.
#'
#' @returns None
#' @noRd
.save_s2dv <- function(cube, path, drop = FALSE) {
  # if (isTRUE(drop)) {
  #   drop_dims <- names(cube$dims)[which(cube$dims == 1)]
  # } else {
  #   drop_dims <- NULL
  # }
  # plot_data <- stars::st_as_stars(
  #   plot_data,
  #   dimensions = stars::st_dimensions(
  #     dataset = c(data$coords$dataset),
  #     var = c(data$coords$var),
  #     sdate = c(data$coords$sdate)[sdate],
  #     time = c(data$coords$time)[time],
  #     ensemble = c(data$coords$ensemble)[ensemble],
  #     location = point_geom
  #   )

  return(cube)
}
