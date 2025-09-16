.norm_cols_lonlat <- function(df) {
  nms <- names(df)
  if ("x" %in% nms && !("lon" %in% nms)) names(df)[names(df) == "x"] <- "lon"
  if ("y" %in% nms && !("lat" %in% nms)) names(df)[names(df) == "y"] <- "lat"
  if (!all(c("lon", "lat") %in% names(df))) {
    stop("Required columns 'lon' y 'lat'.", call. = FALSE)
  }
  df
}

.read_point_rds <- function(path) {
  df <- readRDS(path)
  if (!is.data.frame(df)) stop("Not valid.", call. = FALSE)
  .norm_cols_lonlat(df)
}
.write_point_rds <- function(df, path) {
  df <- .norm_cols_lonlat(df)
  saveRDS(df, path); invisible(path)
}

.read_csv_df <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  .norm_cols_lonlat(df)
}
.write_csv_df <- function(df, path) {
  df <- .norm_cols_lonlat(df)
  utils::write.csv(df, path, row.names = FALSE); invisible(path)
}

.read_sf <- function(path) {
  sf::st_read(path, quiet = TRUE)
}
.write_sf <- function(obj, path) {
  if (file.exists(path)) try(unlink(path, recursive = TRUE, force = TRUE), silent = TRUE)
  suppressWarnings(sf::st_write(obj, path, quiet = TRUE)); invisible(path)
}

.read_terra <- function(path) {
  v <- try(terra::vect(path), silent = TRUE)
  if (!inherits(v, "try-error")) return(v)
  r <- try(terra::rast(path), silent = TRUE)
  if (!inherits(r, "try-error")) return(r)
  stop("Canot read this file", call. = FALSE)
}
.write_terra_vect <- function(v, path) {
  if (file.exists(path)) try(unlink(path, recursive = TRUE, force = TRUE), silent = TRUE)
  terra::writeVector(v, path, overwrite = TRUE); invisible(path)
}

.sf_to_wgs84 <- function(sfo) {
  crs <- sf::st_crs(sfo)
  if (!is.na(crs)) sfo <- sf::st_transform(sfo, 4326)
  sfo
}
.terra_vect_to_wgs84 <- function(v) {
  crs <- terra::crs(v, proj = TRUE)
  if (!is.na(crs) && nzchar(crs)) v <- terra::project(v, "EPSG:4326")
  v
}
.terra_rast_to_wgs84 <- function(r) {
  crs <- terra::crs(r, proj = TRUE)
  if (!is.na(crs) && nzchar(crs) && !grepl("4326|WGS84", crs, ignore.case = TRUE)) {
    r <- terra::project(r, "EPSG:4326")
  }
  r
}

.sf_to_point_df <- function(sfo) {
  sfo <- .sf_to_wgs84(sfo)
  geom <- sf::st_geometry(sfo)

  if (!inherits(geom, "sfc_POINT")) {
    sfo <- sf::st_point_on_surface(sfo)
  }
  coords <- sf::st_coordinates(sfo)
  df <- sf::st_drop_geometry(sfo)
  df$lon <- coords[, 1]; df$lat <- coords[, 2]
  df[, c("lon", "lat", setdiff(names(df), c("lon", "lat"))), drop = FALSE]
}

.terra_vect_to_point_df <- function(v) {
  v <- .terra_vect_to_wgs84(v)

  if (terra::geomtype(v) != "points") v <- terra::centroids(v)
  xy <- terra::geom(v)[, c("x", "y"), drop = FALSE]
  at <- terra::as.data.frame(v, geom = FALSE)
  data.frame(lon = xy[, "x"], lat = xy[, "y"], at, check.names = FALSE)
}

.rast_to_point_df <- function(r) {
  r <- .terra_rast_to_wgs84(r)

  df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[names(df) == "x"] <- "lon"
  names(df)[names(df) == "y"] <- "lat"
  df
}

.sf_from_df <- function(df) {
  df <- .norm_cols_lonlat(df)
  sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = TRUE)
}
.terra_from_df <- function(df) {
  df <- .norm_cols_lonlat(df)
  terra::vect(df, geom = c("lon", "lat"), crs = "EPSG:4326")
}

.s2dv_write <- function(df, path) {
  df <- .norm_cols_lonlat(df)
  out <- list(lon = df$lon, lat = df$lat, data = df[, setdiff(names(df), c("lon", "lat")), drop = FALSE])
  saveRDS(out, path); invisible(path)
}
.s2dv_read_to_df <- function(path) {
  obj <- readRDS(path)
  if (!is.list(obj) || !all(c("lon", "lat", "data") %in% names(obj))) {
    stop("Not valid.", call. = FALSE)
  }
  if (!is.data.frame(obj$data)) obj$data <- as.data.frame(obj$data, stringsAsFactors = FALSE)
  df <- cbind(data.frame(lon = obj$lon, lat = obj$lat, check.names = FALSE), obj$data)
  .norm_cols_lonlat(df)
}
