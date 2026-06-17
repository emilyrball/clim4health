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
  if (isTRUE(drop)) {
    message(
      "Saving as a netCDF doesn't support 'drop'. A ",
      "file with all dimensions will be generated."
    )
  }

  # Convert to stars
  x <- c4h_convert(cube, "stars", drop = FALSE)

  # Prepare coordinates
  dataarray <- x[[1]]
  longitude <- stars::st_get_dimension_values(x, "longitude")
  latitude <- stars::st_get_dimension_values(x, "latitude")
  dataset <- stars::st_get_dimension_values(x, "dataset")
  var <- stars::st_get_dimension_values(x, "var")
  sdate <- stars::st_get_dimension_values(x, "sdate")
  time <- stars::st_get_dimension_values(x, "time")
  ensemble <- stars::st_get_dimension_values(x, "ensemble")

  # Prepare dimensions
  longitude <- ncdf4::ncdim_def("longitude", "degrees_east", longitude)
  latitude <- ncdf4::ncdim_def("latitude", "degrees_north", latitude)
  dataset <- ncdf4::ncdim_def("dataset", "", dataset)
  var <- ncdf4::ncdim_def("var", "", var)
  sdate <- ncdf4::ncdim_def("sdate", "", sdate)
  time <- ncdf4::ncdim_def("time", "", time)
  ensemble <- ncdf4::ncdim_def("ensemble", "", ensemble)

  var_ncdf4 <- ncdf4::ncvar_def(
    name = cube$attrs$Variable$varName,
    units = cube$attrs$Variable$metadata$units,
    dim = list(longitude, latitude, dataset, var, sdate, time, ensemble),
    missval = NA_real_,
    prec = "float"
  )

  # Define object and attributes
  crs_var <- ncdf4::ncvar_def("crs", "", list(), prec = "char")
  nc <- ncdf4::nc_create(path, list(var_ncdf4, crs_var))
  ncdf4::ncatt_put(nc, "longitude", "standard_name", "longitude")
  ncdf4::ncatt_put(nc, "longitude", "units", "degrees_east")
  ncdf4::ncatt_put(nc, "latitude", "standard_name", "latitude")
  ncdf4::ncatt_put(nc, "latitude", "units", "degrees_north")
  ncdf4::ncatt_put(nc, "crs", "grid_mapping_name", "latitude_longitude")
  ncdf4::ncatt_put(nc, "crs", "epsg_code", 4326)
  ncdf4::ncvar_put(nc, var_ncdf4, dataarray)
  ncdf4::nc_close(nc)
}
