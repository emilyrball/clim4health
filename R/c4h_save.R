#' c4h_download
#'
#' Save objects manipulated using clim4health functions
#' in different formats: s2dv, terra, csv, polygons.
#'
#' @param data The object you want to save, which could be an s2dv_cube,
#' terra, data.frame or sf object.
#' @param format A string indicating the format of the object you want to save.
#' Options are: "s2dv", "terra", "csv" or "polygons"
#' @param path A string indicating the path where you want to save the file.
#'
#' @returns None
#' @export
#'
#' @examples
#' fcst_path <- system.file("extdata", "forecast", package = "clim4health")
#' fcst <- c4h_load(fcst_path, variable = "t2m", ext = 'nc')
#'
#' save_s2dv(fcst, "s2dv", "out.nc")
c4h_save <- function(data, format, path) {
  if (format == "s2dv") {
    .save_s2dv(data, path)
  } else if (format == "terra") {
    .save_terra(data, path)
  } else if (format == "csv") {
    .save_csv(data, path)
  }else if (format == "polygons"){
    .save_polygons(data, path)
  }
}

#' .save_s2dv
#'
#' Save an object of class s2dv_cube to a NetCDF file.
#'
#' @param cube The s2dv_cube object to be saved.
#' @param path A string indicating the path where to save the NetCDF file.
#'
#' @returns None
#' @noRd
.save_s2dv <- function(cube, path = "out.nc") {

  # Detect longitude / latitude dims
  ### REVIEW - should not require latitude and longitude dimensions?
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

#' .save_terra
#'
#' Save terra::SpatRaster (driver inferred by file extension, e.g., .tif)
#'
#' @param r The SpatRaster object to be saved.
#' @param path A string indicating the path where to save the NetCDF file.
#' @param overwrite Logical indicating whether to overwrite existing files.
#' The default is TRUE.
#'
#' @returns None
#' @noRd
.save_terra <- function(r, path, overwrite = TRUE) {
  # Expects a SpatRaster / path = "out.tif"
  stopifnot(inherits(r, "SpatRaster"))
  terra::writeRaster(r, filename = path, overwrite = overwrite)
}

#' .save_csv
#'
#' Save a data.frame to CSV
#'
#' @param df The data.frame to be saved.
#' @param path A string indicating the path where to save the CSV file.
#' @returns None
#' @noRd
.save_csv <- function(df, path) {
  # Expects a data.frame; writes a standard CSV without row names.
  utils::write.csv(df, file = path, row.names = FALSE)
}

#' .save_polygons
#'
#' Save polygons (sf) to GeoPackage (.gpkg) or other formats (by extension)
#' using sf::st_write
#' @param sf_obj The sf object to be saved.
#' @param path A string indicating the path where to save the file.
#' @param layer A string indicating the layer name within the file. The default
#' is "data".
#' @returns None
#' @noRd
.save_polygons <- function(sf_obj, path, layer = "data") {
  # Expects an sf object; path = "polys.gpkg"
  sf::st_write(sf_obj, dsn = path, layer = layer, append = FALSE, quiet = TRUE)
}