# function to load data into R object as s2dv_cube (array with metadata)

# dependencies
library(startR)
library(s2dv)
library(lubridate)
library(easyNCDF)
library(CSTools)

# main function
clim4health_load <- function(path, var = NULL, sdates = NULL, ext = 'nc',
                             lat_selection = 'all', lon_selection = 'all') {
    full_path <- outer(paste0(path, var, '_'), sdates, FUN = "paste0")
    full_path <- t(full_path)
    dim(full_path) <- NULL

    full_path <- outer(full_path, paste0('.', ext), FUN = "paste0")
    full_path <- t(full_path)
    dim(full_path) <- NULL

    # Read .csv files
    if (ext == 'csv') {
  
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
    } else if (ext == 'nc') {
      dataset <- paste0(path, '$var$_$sdate$.nc')

      # read netcdf file dimensions:
      file.dims <- names(NcReadDims(full_path[1]))
      # define initial input parameters to startR
      start_parameters <- list(
              dataset = dataset,
              var = var,
              sdate = sdates)

      synonims <- list()
      return_vars <- list()

      lat.dim <- which(file.dims %in% c("latitude","lat"))
      lon.dim <- which(file.dims %in% c("longitude","lon"))

      lat.length <- NcReadDims(full_path[1])[[lat.dim]]
      lon.length <- NcReadDims(full_path[1])[[lon.dim]]

      # define synonyms for startR call
      if (lat.length != 1) {
        synonims$latitude <- c("lat",  'latitude')
        return_vars$latitude <- 'dataset'
      } else {
        lat.val <- NcVarReader(file_path=full_path[1],
                               var_name="lat",
                               synonims=list(lat=c("lat","latitude")))[[1]]
      }
      if (lon.length != 1) {
        synonims$longitude <- c("lon", 'longitude')
        return_vars$longitude <- 'dataset'
      } else {
        lon.val <- NcVarReader(file_path=full_path[1],
                               var_name="lon",
                               synonims=list(lon=c("lon","longitude")))[[1]]
      }
      # open by file dimensions
      # if file includes year dimension, add to startR parameters
      if (any(c('year', 'syear') %in% file.dims)){
        start_parameters$year <- "all"
        synonims$year <- c('year', 'syear')
        #start_parameters$year_across <- 'sdate'
        #return_vars$year <- "sdate"
      }
      # if file includes month dimension, add to startR parameters
      if (any(c('month', 'smonth') %in% file.dims)){
        start_parameters$month <- "all"
        synonims$month <- c('month', 'smonth')
        #start_parameters$month_across <- 'sdate'
        #return_vars$month <- "sdate"
      }
      # if file includes week dimension, add to startR parameters
      if (any(c('week', 'sweek') %in% file.dims)){
        start_parameters$week <- "all"
        synonims$week <- c('week', 'sweek')
        #start_parameters$week_across <- 'sdate'
        #return_vars$week <- "sdate"
      }
      # if file includes day dimension, add to startR parameters
      if (any(c('day', 'sday') %in% file.dims)){
        start_parameters$day <- "all"
        synonims$day <- c('day', 'sday')
        #start_parameters$day_across <- 'sdate'
        #return_vars$day <- c("sdate")
      }
      # if file includes time dimension, add to startR parameters
      if (any(c('time') %in% file.dims)){
        start_parameters$time <- 'all'
        if (length(sdates > 1)) {
          start_parameters$time_across <- 'sdate'
          return_vars$time <- "sdate"
        }
      }

      # if file includes ensemble dimension, add to startR parameters
      if (any(c('ensemble', 'member', 'number') %in% file.dims)){
        start_parameters$ensemble <- "all"
      }


      
      #if (any(c('time') %in% file.dims)){
      #  #return_vars$time <- 'sdate'
      #  start_parameters$time_across <- 'sdate'
      #}

      # if file includes ensemble dimension, add to synonims
      if (any(c('ensemble', 'member', 'number') %in% file.dims)){
        synonims$ensemble <- c('ensemble', 'member', 'number')
      }
      
      # define final necessary parameters to startR
      if (lat.length != 1) {
        start_parameters$latitude <- 'all'
        start_parameters$latitude_reorder <- Sort(decreasing = TRUE)
      }
      if (lon.length != 1) {
        start_parameters$longitude <- 'all'
        start_parameters$longitude_reorder <- CircularSort(-180, 180)
      }

      start_parameters$synonims <- synonims
      start_parameters$return_vars <- return_vars
      start_parameters$retrieve <- TRUE
      start_parameters$split_multiselected_dims <- TRUE
      start_parameters$merge_across_dims <- TRUE
      start_parameters$merge_across_dims_narm <- TRUE
      start_parameters$largest_dims_length <- TRUE

      # Use startR function to open the files
      data <- do.call(Start, start_parameters)
      # keep dates
      
      if (any(c('time') %in% file.dims)){
        time_attributes <- attr(data, "Variables")$common$time
        #time_units_number <- time_attributes$vals
        #time_units_units <- time_attributes$units # "hours since..."
        #if (grepl("hours since ", time_units_units, fixed = TRUE)){
        #  metadata_dates <- as.POSIXct(gsub("hours since ", "", time_units_units),tz="UTC")+as.difftime(time_units_number,units="hours")
        #}
        metadata_dates <- as.POSIXct(time_attributes)
      }

      # transform to sd2v_cube
      data <- as.s2dv_cube(data)

      if (any(c('time') %in% file.dims)){
        data$coords$time <- metadata_dates
      #  #if ()
      }

      # store loaded dates in Date format
      data$attrs$Dates <- as.Date(data$attrs$Dates)

      # add lat/lon if needed
      if (lat.length <= 1) {
        data <- CST_InsertDim(data, length(dim(data$data)) + 1, 1, "latitude", values = lat.val)
      }
      if (lon.length <= 1) {
        data <- CST_InsertDim(data, length(dim(data$data)) + 1, 1,"longitude", values = lon.val)
      }
      if (!("sdate" %in% names(dim(data$data)))) {
        data <- CST_InsertDim(data, 3, length(sdates),"sdate", values = sdates)
      }
      
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