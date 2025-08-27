#' Spatial aggregation of climate products into polygon geometries.
#'
#' @param data s2dv_cube object with the climate data to be aggregates, usually 
#' read using `clim4health_load`.
#' @param boundaries Vector da, either an sf object loaded into memory, or path 
#' to a vector file to be read. Contains the polygon geometries for the spatial 
#' aggregation.
#' @param lat_dim_name String. Name of the latitude dimension.
#' @param lon_dim_name String. Name of the longitude dimension.
#' @param agg_fun String. Method to perform the aggregation, defaults to "mean".
#' Other possible options include "median" or "sum".
#' @param region_id String. Name of the region identifier in the boundaries
#'
#' @returns A s2dv_cube object with the aggregated data.
#' @export
#'
#' @examples
#' # library(sf)
#' #
#' # # Load forecast data
#' # fcst_path <- system.file("extdata/forecast/", package = "clim4health")
#' # fcst_path <- paste0(fcst_path, "/")
#' # fcst <- clim4health_load(fcst_path, var = "t2m", data_type = "fcst",
#' #                          sdates = "20240401", ext = 'nc')
#' # dim(fcst$data)
#' #
#' # # Read and plot polygon geometries (municipalities in Colombia)
#' # poly_path <- system.file("extdata/shapefiles/", package = "clim4health")
#' # poly_path <- paste0(poly_path, "/MGN_ANM_MPIOS.shp")
#' # poly_sf <- read_sf(poly_path)
#' # dim(poly_sf) # 1122 municipalities
#' # plot(st_geometry(poly_sf))
#' #
#' # # Spatially aggregate the forecast
#' # result_fcst <- clim4health_space(fcst, poly_sf)
#' # dim(result_obs$data)

clim4health_space <- function(data,
                              boundaries,
                              lat_dim_name = "latitude",
                              lon_dim_name = "longitude",
                              agg_fun = "mean",
                              region_id = NULL) {
  # read boundaries from file:
  if (any(class(boundaries) == "character")) {
    poly <- terra::vect(boundaries)
  } else {
    poly <- boundaries
  }

  if (!is.null(region_id) && !(region_id %in% names(poly))) {
    stop("region_id not found in the boundaries file.")
  }

  # read metadata
  coords <- data$coords
  lat <- coords$latitude
  lon <- coords$longitude

  # transform array to raster stack
  out <- NULL
  out_dt <- NULL
  columns_index <- c()

  poly <- terra::project(poly, "EPSG:4326")
  ref  <- terra::crs(poly, describe = TRUE)$code

  dims <- dim(data$data)
  dim_names <- names(dims)

  idx_lat <- which(dim_names == lat_dim_name)
  idx_lon <- which(dim_names == lon_dim_name)

  spatial_dims <- c(idx_lat, idx_lon)
  non_spatial_dims <- seq_along(dims)[-spatial_dims]

  # create a grid of indices for the non-spatial dimensions
  index_grid <- expand.grid(
    lapply(dims[non_spatial_dims], function(x) seq_len(x)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  out_list <- purrr::pmap(index_grid, function(...) {
    .make_raster(
      index_list = list(...),
      data = data$data,
      lat = lat,
      lon = lon,
      n_spatial = length(spatial_dims), # latitude and longitude
      crs_code = ref
    )
  })

  out <- terra::rast(out_list)

  columns_index <- purrr::pmap_chr(index_grid,
                                   ~ paste0(sprintf("%02d", c(...)),
                                            collapse = "_"))

  # extract region data (for all regions in poly) with terra::extract
  out_dt <- terra::extract(out, poly, fun = agg_fun)

  # transform to array
  final_dims <- c(dims[non_spatial_dims], region = nrow(out_dt))
  result <- array(NA, dim = final_dims)

  index <- strsplit(columns_index, "_")
  for (cc in seq_along(columns_index)) {
    idx <- as.integer(index[[cc]])
    result_idx <- c(idx, list(seq_len(nrow(out_dt))))
    result[do.call(`[<-`, c(list(result), result_idx,
                            list(value = out_dt[, cc])))]
  }

  # format to s2dv_cube
  replacement_dims <- dim(result)
  names(replacement_dims) <- c(non_spatial_dims, "region")
  replacement_attrs <- data$attrs

  replacement_coords <- coords[setdiff(names(coords),
                                       c("latitude", "longitude"))]
  if (is.null(region_id)) {
    region_id <- "region"
    replacement_coords[["region"]] <- seq_len(nrow(poly))
  } else {
    replacement_coords[["region"]] <- as.vector(poly[[region_id]])
    replacement_attrs[["region_id"]] <- region_id
  }

  replacement_attrs[["dtype"]] <- "polygon"
  replacement_attrs[["geometry"]] <- terra::geom(poly, wkt = TRUE)
  replacement_attrs[["crs"]] <- paste0("EPSG:", ref)

  # create final s2dv_cube object
  data <- list(data = result,
               coords = replacement_coords,
               dims = replacement_dims,
               attrs = replacement_attrs)
  attr(data, "class") <- "s2dv_cube"

  return(data)
}