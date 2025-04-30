# function to aggregate gridded data to a shapefile

# dependencies
library(sf)
library(exactextractr)

paste_zeros <- function(x) {
  return(paste0(ifelse(x < 10, "0", ""), x))
}

# main function
clim4health_spatial <- function(data, shapefile,
                                lat_dim_name = "latitude",
                                lon_dim_name = "longitude",
                                agg_fun = "mean") {
  # read shapefile from file:
  if (class(shapefile) == "character") {
    shp <- sf::st_read(shapefile)
  } else {
    shp <- shapefile
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

  shp <- sf::st_transform(shp, crs = 4326)

  ref <- terra::crs(shp, describe = TRUE)$code

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
                             paste0(paste_zeros(dat), "_",
                                    paste_zeros(vv),  "_",
                                    paste_zeros(tt),  "_",
                                    paste_zeros(ee),  "_"))
        }
      }
    }
  }
  out <- raster::stack(out)

  # extract region data (for all regions in shp) with exact_extractr
  out_dt <- exactextractr::exact_extract(out, shp, fun = agg_fun)
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
  replacement_coords[["region"]] <- seq_len(nrow(shp))

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
