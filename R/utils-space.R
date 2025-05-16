#' .make_raster
#'
#' TBC
#'
#' @param index_list TBC
#' @param data TBC
#' @param lat TBC
#' @param lon TBC
#' @param n_spatial TBC
#' @param crs_code TBC
#'
#' @returns A vector of concatenated hours and minutes.
#'
.make_raster <- function(index_list, data, lat, lon, n_spatial, crs_code) {
  full_idx <- c(index_list, rep(list(quote(expr=)), n_spatial))
  slice <- do.call(`[`, c(list(data), full_idx, drop = FALSE))
  slice <- drop(slice)

  r <- terra::rast(slice)
  terra::ext(r) <- terra::ext(c(min(lon), max(lon), min(lat), max(lat)))
  terra::crs(r) <- paste0("EPSG:", crs_code)
  return(r)
}