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
#' @returns TBC
#'
.make_raster <- function(index_list, data, lat, lon, n_spatial, crs_code) {
  dx <- abs(lon[2] - lon[1])
  dy <- abs(lat[2] - lat[1])
  
  full_idx <- c(index_list, rep(list(quote(expr=)), n_spatial))
  slice <- do.call(`[`, c(list(data), full_idx, drop = FALSE))
  slice <- drop(slice)

  r <- terra::rast(slice)
  terra::ext(r) <- terra::ext(c(min(lon) - dx/2,
                                max(lon) + dx/2,
                                min(lat) - dy/2,
                                max(lat) + dy/2))
  terra::crs(r) <- paste0("EPSG:", crs_code)
  return(r)
}