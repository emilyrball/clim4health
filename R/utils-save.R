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
  # Detect longitude / latitude dims
  lon <- as.vector(cube$coords$longitude)
  lat <- as.vector(cube$coords$latitude)

  # Drop non-spatial singleton dims (dataset, var, sdate, etc.)
  if (drop) {
    arr <- drop(cube$data)
  } else {
    arr <- cube$data
  }

  # Collapse everything except lon/lat into "band"
  dim_arr <- dim(arr)
  nlon <- length(lon)
  nlat <- length(lat)
  nband <- prod(dim_arr) / (nlon * nlat)
  arr3 <- array(arr, dim = c(nlon, nlat, nband)) # [lon, lat, band]

  # Build stars object
  dims <- stars::st_dimensions(
    x = lon,
    y = lat,
    band = seq_len(nband)
  )

  st <- stars::st_as_stars(arr3, dimensions = dims)
  names(st) <- cube$attrs$varName %||% "var"

  # Write NetCDF file
  stars::write_stars(st, path)
  message("Saved NetCDF: ", path)
}
