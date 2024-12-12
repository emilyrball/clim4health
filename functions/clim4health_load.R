# function to load data into R object as s2dv_cube (array with metadata)

# dependencies
library(startR)
library(s2dv)
library(lubridate)
library(easyNCDF)
library(CSTools)

# main function
clim4health_load <- function(path, var = NULL, sdates = NULL, time_freq = NULL,
                             lat_selection = 'all', lon_selection = 'all') {

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
      #dates <- as.Date(unique(data[,time.col]), format = "%d-%m-%Y", "%Y/%m/%d")
      if (any(is.na(ymd(unique(data[,time.col]))))){
        dates <- dmy(unique(data[,time.col]))
      } else {
        dates <- ymd(unique(data[,time.com]))
      }
      coords <- unique(paste0(data[,lat.col], '; ', data[,lon.col]))
      for (cc in 1:length(coords)){
        lat <- c(lat, as.integer(strsplit(coords[cc], '; ')[[1]][1]))
        lon <- c(lon, as.integer(strsplit(coords[cc], '; ')[[1]][2]))
      }

      # convert table to array
      #data.array <- array(data[,var.col], dim = c(latitude = length(lat), longitude = length(lon), time = length(dates))) 
      data.array <- array(NA, dim = c(latitude = length(lat), longitude = length(lon), time = length(dates))) 
      for(ll in 1:length(lat)){
        data.array[ll, ll, ] <- data[which(data[,lat.col]==lat[ll] & data[,lon.col]==lon[ll]),var.col]
      }

      # format to s2dv_cube
      data <- list(data = data.array, coords = list(latitude = lat, longitude = lon, time = dates))
      attr(data, 'class') <- 's2dv_cube'

      # subset to only the requested coordinates (lat_selection and lon_selection)
      if (any(lat_selection == 'all')){
        lat_select <- 1:length(lat)
      } else {
        lat_select <- which(lat >= lat_selection[1] & lat <= lat_selection[2])
      }
      if (any(lon_selection == 'all')){
        lon_select <- 1:length(lon)
      } else {
        lon_select <- which(lon >= lon_selection[1] & lon <= lon_selection[2])
      }
      data <- CST_Subset(data, along = c('latitude', 'longitude'), indices = list(lat_select, lon_select), drop = 'none')

    # Read .nc files
    } else if (all(tools::file_ext(path) == 'nc')) {
      
      # var (internal netcdf variable name needs to be the same in the file name)
      if (is.null(var)){
        var <- strsplit(strsplit(path, '/')[[1]][length(strsplit(path, '/')[[1]])], '_')[[1]][1] # assuming filename "var"+"_"+"date"+".nc"
      }
      if (is.null(sdates)){
        sdates <- unlist(lapply(strsplit(path, "_"), function(x) strsplit(x, ".nc")[[length(x)]])) # assuming filename "var"+"_"+"date"+".nc"
      }
      dataset <- gsub(var, '$var$', path)

      ### at the moment only checking against the first element of sdates
      dataset <- gsub(sdates, '$sdate$', dataset)
      
      # read netcdf file dimensions:
      file.dims <- names(NcReadDims(path))

      # define all necessary input parameters to startR
      start_parameters <- list(
              dataset = dataset,
              var = var,
              sdate = sdates,
              time = 'all')#indices(1:NcReadDims(path)[[which(file.dims == "time")]])
      if (any(c('ensemble', 'member', 'number') %in% file.dims)){
        start_parameters$ensemble <- "all"
      }
      if (any(c('day') %in% file.dims)){
        start_parameters$day <- "all"
      }
      

      # define synonyms for startR call
      synonims <- list(latitude = c('lat', 'latitude'),
                       longitude = c('lon', 'longitude'),
                       time = c('time', 'date', 'month'))

      return_vars <- list(latitude = 'dataset',
                          longitude = 'dataset',
                          time = 'sdate')

      # add necessary synonyms
      if (any(c('ensemble', 'member', 'number') %in% file.dims)){
        synonims$ensemble <- c('ensemble', 'member', 'number')
      }
      
      # define final necessary parameters to startR
      start_parameters$latitude <- 'all' #ifelse(lat_selection == 'all', 'all', values(list(lat_selection[1], lat_selection[2]))), #values(list(42, 45)),
      start_parameters$latitude_reorder <- Sort(decreasing = TRUE)
      start_parameters$longitude <- 'all' #ifelse(lon_selection == 'all', 'all', values(list(lon_selection[1], lon_selection[2]))), #values(list(0, 3)),
      start_parameters$longitude_reorder <- CircularSort(-180, 180)
      start_parameters$synonims <- synonims
      start_parameters$return_vars <- return_vars
      start_parameters$retrieve <- TRUE
      start_parameters$split_multiselected_dims <- TRUE
      print(start_parameters)
      # Use startR function to open the files
      data <- do.call(Start, start_parameters)

      # keep dates
      dates <- attr(data, "Variables")$common$time
      dim_attributes <- attr(data, "Variables")$common[[var]]$dim
      
      #for (dd in 1:length(dim_attributes)){
      #  if (dim_attributes[[dd]]$name == 'time'){
      #    time_attributes <- dim_attributes[[dd]]
      #  }
      #}
      #time_units_number <- time_attributes$vals
      #time_units_units <- time_attributes$units # "hours since..."
      #if (grepl("hours since ", time_units_units, fixed = TRUE)){
      #  metadata_dates <- as.Date(gsub("hours since ", "", time_units_units)) + time_units_number/24 # /24 from hours to days
      #}

      # transform to sd2v_cube
      data <- as.s2dv_cube(data)
      #data$coords$time <- metadata_dates
      data$attrs$Dates <- as.Date(data$attrs$Dates)
      # subset lat lon
      lat <- data$coords$latitude
      lon <- data$coords$longitude
      if (any(lat_selection == 'all')){
        lat_select <- 1:length(lat)
      } else {
        lat_select <- which(lat >= lat_selection[1] & lat <= lat_selection[2])
      }
      if (any(lon_selection == 'all')){
        lon_select <- 1:length(lon)
      } else {
        lon_select <- which(lon >= lon_selection[1] & lon <= lon_selection[2])
      }
      data <- CST_Subset(data, along = c('latitude', 'longitude'), indices = list(lat_select, lon_select), drop = 'none')

    } else {

      # If the extension of the files is not in the previous if conditions stop the script
      stop ("The type_file has to be csv or nc, no other type files are allowed at the moment")
    
    }

  return (data)

}