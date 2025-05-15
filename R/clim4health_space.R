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
                              agg_fun = "mean") {
  # read boundaries from file:
  if (any(class(boundaries) == "character")) {
    poly <- sf::st_read(boundaries)
  } else {
    poly <- boundaries
  }
  
  # read metadata
  coords <- data$coords
  lat <- coords$latitude
  lon <- coords$longitude
  
  # transform array to raster stack
  out <- NULL
  out_dt <- NULL
  num <- 0
  columns_dates <- c()
  columns_index <- c()
  
  poly <- sf::st_transform(poly, crs = 4326)
  
  ref <- terra::crs(poly, describe = TRUE)$code
  
  for (dat in seq(dim(data$data)["dataset"])) {
    for (vv in seq(dim(data$data)["var"])) {
      for (tt in seq(dim(data$data)["time"])) {
        for (ee in seq(dim(data$data)["ensemble"])) {
          num <- num + 1
          out[[num]] <- raster::raster(data$data[dat, vv, tt, ee, , ],
                                       xmn = min(lon), xmx = max(lon),
                                       ymn = min(lat), ymx = max(lat))
          
          raster::crs(out[[num]]) <- paste0("+init=epsg:", ref)
          
          #columns_dates <- c(columns_dates,
          #  paste0('ensemble_', ifelse(ee<10, '0', ''), ee,
          #  '_date_', substr(dates[yy,tt], 1,10)))

          columns_index <- c(columns_index,
                             paste0(sprintf("%02d", dat), "_",
                                    sprintf("%02d", vv),  "_",
                                    sprintf("%02d", tt),  "_",
                                    sprintf("%02d", ee),  "_"))
        }
      }
    }
  }
  out <- raster::stack(out)
  
  # extract region data (for all regions in poly) with exact_extractr
  out_dt <- exactextractr::exact_extract(out, poly, fun = agg_fun)
  # __if (is.null(dim(out_dt))){out_dt <- data.frame(out_dt)}
  # __names(out_dt) <- columns_dates
  
  # transform to array
  result <- array(NA, dim = c(dim(data$data)["dataset"],
                              dim(data$data)["var"],
                              dim(data$data)["time"],
                              dim(data$data)["ensemble"],
                              region = nrow(out_dt)))
  index <- strsplit(columns_index, "_")
  for (cc in seq_along(columns_index)) {
    dat <- as.integer(index[[cc]][1])
    vv  <- as.integer(index[[cc]][2])
    tt  <- as.integer(index[[cc]][3])
    ee  <- as.integer(index[[cc]][4])
    result[dat, vv, tt, ee, ] <- out_dt[, cc]
  }
  
  # format to s2dv_cube
  replacement_coords <- coords[- which(names(coords) %in%
                                         c("latitude", "longitude"))]
  replacement_coords[["region"]] <- seq_len(nrow(poly))
  
  replacement_dims <- dim(result)
  names(replacement_dims) <- c("dataset", "var", "time", "ensemble", "region")
  
  replacement_attrs <- data$attrs
  
  data <- list(data = result,
               coords = replacement_coords,
               dims = replacement_dims,
               attrs = replacement_attrs)
  attr(data, "class") <- "s2dv_cube"
  
  return(data)
}