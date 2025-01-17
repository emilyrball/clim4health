# function to load data into R object as s2dv_cube (array with metadata)

# dependencies
library(startR)
library(s2dv)
library(lubridate)
library(easyNCDF)
library(CSTools)

# main function
clim4health_load <- function(path, var = NULL, sdates = NULL,
                             data_type = "", ext = 'nc',
                             lat_selection = 'all',
                             lon_selection = 'all') {
    if (!(data_type %in% c("fcst", "hcst", "obs"))) {
      # user must select whether data is observation or forecast/hindcast
      stop ("Please select data_type. Valid options are \"fcst\", \"hcst\", or \"obs\".")
    
    } else if (!(ext %in% c("nc", "csv"))) {
      # If the extension of the files is not in the previous if conditions stop the script
      stop ("The type_file has to be csv or nc, no other type files are allowed at the moment")
    }
    full_path <- outer(paste0(path, var, '_'), sdates, FUN = "paste0")
    full_path <- t(full_path)
    dim(full_path) <- NULL

    full_path <- outer(full_path, paste0('.', ext), FUN = "paste0")
    full_path <- t(full_path)
    dim(full_path) <- NULL

    # Read .csv files
    if (ext == 'csv') {
  
      # read file
      data <- read.csv(full_path)

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

      # read netcdf file dimensions
      file.dims <- names(NcReadDims(full_path[1]))

      synonims <- list()
      return_vars <- list()

      ### extract time dimensions from files ###
      time_dims <- c()
      for (f in c("year", "month", "week", "day", "date", "time")) {
        if (f %in% file.dims) {
          time_dims <- c(time_dims, f)
        }
      }
      # define initial input parameters to startR
      start_parameters <- list(
              dataset = dataset,
              var = var,
              sdate = sdates)
      # add time dimensions
      for (f in time_dims) {
        if ((data_type != "obs") && (f == "time")) {
          f <- paste0("lead_", f)
          synonims$lead_time <- c("time", 'lead_time')
        }
        start_parameters[f] <- "all"
        if ((data_type== "obs") && (f == "time")) {
          start_parameters[paste0(f, "_across")] <- "sdate"
        }
        return_vars[f] <- "sdate"
      }

      # if file includes ensemble dimension, add to startR parameters
      if (any(c('ensemble', 'member', 'number') %in% file.dims)){
        start_parameters$ensemble <- "all"
        synonims$ensemble <- c('ensemble', 'member', 'number')
      }

      # get latitude values
      if (any(c("latitude", "lat") %in% file.dims)) {
        lat.dim <- which(file.dims %in% c("latitude","lat"))
        lat.length <- NcReadDims(full_path[1])[[lat.dim]]
        if (lat.length > 1) {
          synonims$latitude <- c("lat",  'latitude')
          return_vars$latitude <- 'dataset'
          start_parameters$latitude <- 'all'
          start_parameters$latitude_reorder <- Sort(decreasing = TRUE)
        } else {
          lat.val <- NcVarReader(file_path=full_path[1],
                                 var_name="lat",
                                 synonims=list(lat=c("lat",
                                                     "latitude")))[[1]]
        }
      } else {
        lat.val <- indices(1)
      }
      # get longitude values
      if (any(c("longitude", "lon") %in% file.dims)) {
        lon.dim <- which(file.dims %in% c("longitude","lon"))
        lon.length <- NcReadDims(full_path[1])[[lon.dim]]
        if (lon.length > 1) {
          synonims$longitude <- c("lon", 'longitude')
          return_vars$longitude <- 'dataset'
          start_parameters$longitude <- 'all'
          start_parameters$longitude_reorder <- CircularSort(-180, 180)
        } else {
          lon.val <- NcVarReader(file_path=full_path[1],
                                 var_name="lon",
                                 synonims=list(lon=c("lon",
                                                     "longitude")))[[1]]
        }
      } else {
        lon.val <- indices(1)
      }

      # define final parameters for startR calls
      start_parameters$synonims <- synonims
      start_parameters$return_vars <- return_vars
      start_parameters$retrieve <- FALSE
      start_parameters$split_multiselected_dims <- TRUE
      start_parameters$merge_across_dims <- TRUE
      start_parameters$merge_across_dims_narm <- TRUE
      start_parameters$largest_dims_length <- TRUE

      # Use startR function to get the dates
      data <- do.call(Start, start_parameters)
      # keep dates
      if (any(c('time') %in% file.dims)){
        if (data_type == "obs") {
          time_attributes <- attr(data, "Variables")$common$time
        } else {
          time_attributes <- attr(data, "Variables")$common$lead_time
        }
        metadata_dates <- as.POSIXct(time_attributes)
        years <- unique(year(metadata_dates))
        months <- unique(month(metadata_dates))
        days <- unique(day(metadata_dates))
        hours <- hour(metadata_dates)
        minutes <- minute(metadata_dates)
        seconds <- second(metadata_dates)
        hours <- paste(hours, minutes, seconds, sep = ':')
        hours <- unique(hours)
        #reshape dates as needed
        if (data_type == "obs") {
          dim(metadata_dates) <- c(length(years), length(months),
                                   length(days), length(hours), 1)
          names(dim(metadata_dates)) <- c("year", "month", "day",
                                          "time", "lead_time")
        } else {
          dim(metadata_dates) <- c(length(years), length(months),
                                   length(days), length(hours))
          names(dim(metadata_dates)) <- c("year", "month", "day",
                                           "time")
        }

        if (data_type == "obs") {
          start_parameters$time <- values(metadata_dates)
        } else {
          start_parameters$lead_time <- 'all'#values(metadata_dates)
        }
      } else {
        if ("year" %in% time_dims) {
          years <- attr(data, "Variables")$common$year
        } else {
          years <- sdates
        }
        if ("month" %in% time_dims) {
          months <- attr(data, "Variables")$common$month
          for (i in seq_along(months)) {
            if (nchar(months[i]) == 1) {
              months[i] <- paste0('0', months[i])
            }
          }
        } else {
          months <- indices(1)
        }
        if ("day" %in% time_dims) {
          days <- attr(data, "Variables")$common$day
          for (i in seq_along(days)) {
            if (nchar(days[i]) == 1) {
              days[i] <- paste0('0', days[i])
            }
          }
        } else {
          days <- indices(1)
        }
      }

      
      start_parameters$retrieve <- TRUE
      data <- do.call(Start, start_parameters)
      # transform to sd2v_cube
      data <- as.s2dv_cube(data)

      #if (any(c('time') %in% file.dims)){
      #  if (data_type == "obs") {
      #    data$coords$time <- metadata_dates
      #  } else {
      #    data$coords$lead_time <- metadata_dates
      #  }
      #}
      # store loaded dates in Date format
      dates <- as.Date(data$attrs$Dates)
      data$attrs$Dates <- dates
      out_dims <- names(data$dim)
      
      if (!("sdate" %in% names(dim(data$data)))) {
        if (data_type == "obs") {
          data <- CST_InsertDim(data, 3, 1,"sdate", values = sdates[1])
        } else {
          data <- CST_InsertDim(data, 3, length(sdates),"sdate", values = sdates)
        }
      }

      if (!("year" %in% out_dims)) {
        data <- CST_InsertDim(data, 4, length(years),
                              "year", values = years)
      }
      if (!("month" %in% out_dims)) {
        data <- CST_InsertDim(data, 5, length(months),
                              "month", values = months)
      }
      if (!("day" %in% out_dims)) {                        
        data <- CST_InsertDim(data, 6, length(days),
                              "day", values = days)
      }
      if (!("time" %in% out_dims)) {                        
        data <- CST_InsertDim(data, 7, length(hours),
                              "time", values = hours)
      }
      if (data_type == "obs") {
        data$coords$year <- years
        data$coords$month <- months
        data$coords$day <- days
        data$coords$time <- hours
      }
      if (!("lead_time" %in% out_dims)) {
        data <- CST_InsertDim(data, 8,
                              1, "lead_time",
                              values = indices(1))
      }
      if (!("ensemble" %in% out_dims)) {
        data <- CST_InsertDim(data, 9,
                              1, "ensemble",
                              values = indices(1))
      }
      if (!("latitude" %in% out_dims)) {
        data <- CST_InsertDim(data, 10,
                              1, "latitude", values = lat.val)
      }
      if (!("longitude" %in% out_dims)) {
        data <- CST_InsertDim(data, 11,
                              1,"longitude", values = lon.val)
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
      data <- CST_Subset(data, along = c('latitude', 'longitude'),
                         indices = list(lat_select, lon_select),
                         drop = 'none')
    }
  return (data)

}