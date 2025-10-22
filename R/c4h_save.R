#' c4h_download
#'
#' download the objects in different formats, s2dv, terra, csv, polygons.
#'
#'
#' @param data The object you want to transform
#' @param format Format of the object you want to save 
#' could be: s2dv terra, csv or polygons
#' @param path the path where you want to save the file saved
#'
#' @returns None
#' @export
#'
#' @examples
#' fcst_path <- "inst/extdata/forecast/"
#' fcst_path <- paste0(fcst_path)
#' 
#' fcst <- clim4health::c4h_load(fcst_path,
#'                             variable = "t2m",
#'                             year = 2025,
#'                             month = 1,
#'                             ext = 'nc'
#'                            )
#' save_s2dv(fcst, 's2dv', 'out.nc')


c4h_download <- function (data, format, path) {
  if (format == 's2dv') {
    .save_s2dv(data, path)
  } else if (format == 'terra') {
    .save_terra(data, path)
  } else if (format == 'csv') {
    .save_csv(data, path)
  }else if (format == 'polygons'){
    .save_polygons(data, path)
  }
}


.save_s2dv <- function(cube, path = "out.nc") {
  
  # Detect longitude / latitude dims
  lon <- as.vector(cube$coords$longitude)
  lat <- as.vector(cube$coords$latitude)
  
  # Drop non-spatial singleton dims (dataset, var, sdate, etc.)
  arr <- drop(cube$data)
  
  # Collapse everything except lon/lat into "band"
  dim_arr <- dim(arr)
  nlon <- length(lon)
  nlat <- length(lat)
  nband <- prod(dim_arr) / (nlon * nlat)
  arr3 <- array(arr, dim = c(nlon, nlat, nband))  # [lon, lat, band]
  
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

# Save terra::SpatRaster (driver inferred by file extension, e.g., .tif)
.save_terra <- function(r, path, overwrite = TRUE) {
  # Expects a SpatRaster / path = "out.tif"
  stopifnot(inherits(r, "SpatRaster"))
  terra::writeRaster(r, filename = path, overwrite = overwrite)
}

# Save a data.frame to CSV
.save_csv <- function(df, path) {
  # Expects a data.frame; writes a standard CSV without row names.
  utils::write.csv(df, file = path, row.names = FALSE)
}

# Save polygons (sf) to GeoPackage (.gpkg) or other formats (by extension)
.save_polygons <- function(sf_obj, path, layer = "data") {
  # Expects an sf object; path = "polys.gpkg"
  sf::st_write(sf_obj, dsn = path, layer = layer, append = FALSE, quiet = TRUE)
}