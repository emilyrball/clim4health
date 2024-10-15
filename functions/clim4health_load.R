# load data into R object

# dependencies
library(startR)
library(lubridate)
library(easyNCDF)

# main function
clim4health_load <- function(path, var = NULL) {

    # Read .csv files
    if (all(tools::file_ext(path) == 'csv')) {
  
      # read file
      data <- read.csv(path)

      # read headers
      headers <- colnames(data)
      lat.col <- which(headers %in% c('y', 'lat', 'latitude'))
      lon.col <- which(headers %in% c('x', 'lon', 'longitude'))
      time.col <- which(headers %in% c('time', 'date', 'day', 'week', 'month'))
      if (!is.null(var)){var.col <- which(headers %in% var)} else {var.col <- length(headers)} # if no name of the var column, assume it is the last one

      # obtain metadata
      lat <- c() # initialise
      lon <- c() # initialise
      dates <- as.Date(unique(data[,time.col]))
      coords <- unique(paste0(data[,lat.col], '; ', data[,lon.col]))
      for (cc in 1:length(coords)){
        lat <- c(lat, as.integer(strsplit(coords[cc], '; ')[[1]][1]))
        lon <- c(lon, as.integer(strsplit(coords[cc], '; ')[[1]][2]))
      }

      # convert values to array # potential issue depending on the row order in the csv !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      data.array <- array(data[,var.col], dim = c(latitude = length(lat), longitude = length(lon), time = length(dates))) 
      
      # format to s2dv_cube
      data <- list(data = data.array, coords = list(latitude = lat, longitude = lon, time = dates))
      attr(data, 'class') <- 's2dv_cube'

    # Read .nc files
    } else if (all(tools::file_ext(path) == 'nc')) {
      
      # var (internal netcdf variable name needs to be the same in the file name)
      if (is.null(var)){
        var <- strsplit(strsplit(path, '/')[[1]][length(strsplit(path, '/')[[1]])], '_')[[1]][1] # assuming filename "var"+"_"+"date"+".nc"
      }
      dataset <- gsub(var, '$var$', path)

      # read netcdf file dimensions:
      file.dims <- names(NcReadDims(path))

      # Use startR function to open the files
      if (any(c('ensemble', 'member', 'number') %in% names(NcReadDims(path)))){
        data <- Start(
              dataset     = dataset,
              var         = var,
              time        = "all",
              ensemble    = "all",
              latitude = 'all', #values(list(42, 45)),
              latitude_reorder = Sort(decreasing = TRUE),
              longitude = 'all', #values(list(0, 3)),
              longitude_reorder = CircularSort(-180, 180),
              synonims = list(latitude = c('lat', 'latitude'),
                                      longitude = c('lon', 'longitude'),
                                      time = c('time', 'date', 'month'),
                                      ensemble = c('ensemble', 'member', 'number')),
              retrieve    = TRUE)
      } else {
        data <- Start(
              dataset     = dataset,
              var         = var,
              time        = "all",
              latitude = 'all', #values(list(42, 45)),
              latitude_reorder = Sort(decreasing = TRUE),
              longitude = 'all', #values(list(0, 3)),
              longitude_reorder = CircularSort(-180, 180),
              synonims = list(latitude = c('lat', 'latitude'),
                                      longitude = c('lon', 'longitude'),
                                      time = c('time', 'date', 'month')),
              retrieve    = TRUE)
      }
    } else {

      # If the extension of the files is not in the previous if conditions stop the script
      stop ("The type_file has to be csv or nc, not other type files are allowed at the moment")
    
    }

  return (data)

}
