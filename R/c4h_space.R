#' Spatial aggregation of climate products into areas.
#'
#' @param data s2dv_cube object with the climate data to be aggregates, usually 
#' read using `c4h_load`.
#' @param areas Vector data, either a sf or terra object loaded into memory, or path 
#' to a vector file to be read. Contains the areal polygon geometries for the spatial 
#' aggregation.
#' @param fun String. Method to perform the aggregation, defaults to "mean".
#' Other possible options include "median", "sum", "min" and "max".
#' @param weighting String that indicates whether weighted means should be used
#' when `fun = "mean"` at the cost of increased computation. Options are "none" 
#' (a simple mean is applied to cells with centroid within the polygon), 
#' "approx" (for an approximate and fast weighting) and "exact" (for an exact 
#' weighting). See \code{\link[terra]{extract}} for details.
#' @param areas_id String. Name of the area identifier in the `areas` object.
#'
#' @returns A s2dv_cube object with the aggregated data.
#' @export
#'
#' @examples
#' library(sf)
#' 
#' # Region municipalities
#' munip_path <- paste0(getwd(), "/inst/extdata/areas/munip_vallecauca.gpkg")
#' munip <- read_sf(munip_path)
#' 
#' # Forecast
#' fcst_path <- paste0(getwd(), "/inst/extdata/forecast/")
#' fcst <- c4h_load(fcst_path, variable = "t2m", ext = 'nc')
#' 
#' # Spatial aggregation
#' fcst_aggr <- c4h_space(fcst, munip)            
#'                          
c4h_space <- function(data,
                      areas,
                      fun = "mean",
                      weighting = "none",
                      areas_id = NULL) {
  
  # read areas from file or sf object
  if (any(class(areas) == "character") | inherits(areas, "sf")) {
    poly <- terra::vect(areas)
  } else {
    poly <- areas
  }
  
  if (!is.null(areas_id) && !(areas_id %in% names(poly))) {
    stop("'areas_id' not found in the 'areas' object.")
  }
  
  # read metadata
  coords <- data$coords
  lat <- coords$latitude
  lon <- coords$longitude
  
  if (lat[1] > lat[length(lat)]) {
    # if latitude is stored north to south, reverse it
    lat <- rev(lat)
    lat_dim  <- which(names(dim(data$data)) == "latitude")
    idx <- lapply(dim(data$data), seq_len)
    idx[[lat_dim]] <- rev(idx[[lat_dim]])
    data$data <- do.call(`[`, c(list(data$data), idx, drop = FALSE))
  }
  
  # transform array to raster stack
  out <- NULL
  out_dt <- NULL
  columns_index <- c()
  
  # Manage CRS, we aggregate in WGS84
  poly <- terra::project(poly, "EPSG:4326")
  ref  <- terra::crs(poly, describe = TRUE)$code
  
  # Get dimensions and indices of data and coordinates
  dims <- dim(data$data)
  dim_names <- names(dims)
  idx_lat <- which(dim_names == "latitude")
  idx_lon <- which(dim_names == "longitude")
  spatial_dims <- c(idx_lat, idx_lon)
  non_spatial_dims <- seq_along(dims)[-spatial_dims]
  names(non_spatial_dims) <- names(seq_along(dims)[-spatial_dims])
  
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
  
  # extract area data (for all areas in poly) with terra::extract
  if(weighting == "none"| fun != "mean"){
    out_dt <- terra::extract(out, poly, fun = fun, na.rm = TRUE)
    
  }else if(weighting == "approx"){
    out_dt <- terra::extract(out, poly, fun = fun, na.rm = TRUE, weights = TRUE)
    
  }else if(weighting == "exact"){
    out_dt <- terra::extract(out, poly, fun = fun, na.rm = TRUE, exact = TRUE)
    
  }
  
  if ("ID" %in% names(out_dt)) {
    out_dt <- out_dt[ , -which(names(out_dt) == "ID"), drop = FALSE]
  }
  
  # transform to array
  final_dims <- c(dims[non_spatial_dims], areas = nrow(out_dt))
  result <- array(NA_real_, dim = final_dims)
  
  index <- strsplit(columns_index, "_")
  for (cc in seq_along(columns_index)) {
    idx <- as.integer(index[[cc]])
    result_idx <- c(idx, list(seq_len(nrow(out_dt))))
    result <- do.call(`[<-`, c(list(result), result_idx,
                               list(value = out_dt[[cc]])))
  }
  
  # format to s2dv_cube
  replacement_dims <- dim(result)
  names(replacement_dims) <- c(dim_names[non_spatial_dims], "areas")
  replacement_attrs <- data$attrs
  replacement_coords <- coords[setdiff(names(coords), c("latitude", "longitude"))]
  
  # Manage IDs
  if (is.null(areas_id)) {
    areas_id <- "areas"
    replacement_coords[["areas"]] <- seq_len(nrow(poly))
  } else {
    replacement_coords[["areas"]] <- poly[[areas_id]]
    replacement_attrs[["areas_id"]] <- areas_id
  }
  
  # Store geometries
  replacement_attrs[["geom"]] <- list(geom = sf::st_geometry(sf::st_as_sf(poly)))
  
  # create final s2dv_cube object
  data <- list(data = result,
               coords = replacement_coords,
               dims = replacement_dims,
               attrs = replacement_attrs)
  attr(data, "class") <- "s2dv_cube"
  
  return(data)
}


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
#' @noRd
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