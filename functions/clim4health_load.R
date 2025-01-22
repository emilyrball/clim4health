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
    if (!(is.null(sdates))) {
      full_path <- outer(paste0(path, var, '_'), sdates, FUN = "paste0")
      full_path <- t(full_path)
      dim(full_path) <- NULL
    } else {
      full_path <- paste0(path, var)
    }

    full_path <- outer(full_path, paste0('.', ext), FUN = "paste0")
    full_path <- t(full_path)
    dim(full_path) <- NULL
    print(full_path)
    # Read .csv files
    if (ext == 'csv') {
      if (!(data_type == "obs")) {
        stop("Cannot currently load forecast/hindcast data in csv format.")
      }
      # read file
      data <- read.csv(full_path)
      if (ncol(data) == 1) {
        for (i in 1:11) {
          if (ncol(read.csv(full_path, skip = i)) > 1) {
            break
          }
        }
        if (i == 11) {
          stop("Too many header lines in csv file, please delete some!")
        } else {
          data <- read.csv(full_path, skip = i)
          metadata <- head(read.csv(full_path), i - 1)
        }
      } else {
        metadata <- NULL
      }
      
      # read headers
      headers <- colnames(data)
      ### get lats/lons ###
      if (any(c("y", "lat", "latitude") %in% headers)) {
        lat.col <- which(headers %in% c('y', 'lat', 'latitude'))
      } else {
        lat.col <- NULL
      }
      if (any(c("x", "lon", "longitude") %in% headers)) {
        lon.col <- which(headers %in% c('x', 'lon', 'longitude'))
      } else {
        lon.col <- NULL
      }

      ### get time columns ###
      time_dim <- list()
      for (f in c('time', 'date', 'day', 'week', 'month', 'year',
                  'Time', 'Date', 'Day', 'Week', 'Month', 'Year')) {
        if (f %in% headers) {
          time_dim[f] <- which(headers == f)
        }
      }

      ### get variable ###
      if (!is.null(var)) {
        var.col <- which(headers %in% var)
      }
      else {
        var.col <- length(headers)
        var <- headers[length(headers)]
      } # if no name of the var column, assume it is the last one

      # obtain metadata
      lat <- c() # initialise
      lon <- c() # initialise
      #dates <- as.Date(unique(data[,time.col]), format = "%d-%m-%Y", "%Y/%m/%d")
      if (any(c("time", "Time", "date", "Date") %in% names(time_dim))) {
        dates <- tryCatch({ymd(data[,names(time_dim)])},
          error = function(x) {return(NA)},
          warning = function(x) {return(NA)})
        if (typeof(dates) == "logical") {
          dates <- tryCatch({dmy(data[,names(time_dim)])},
            error = function(x) {return(NA)},
            warning = function(x) {return(NA)})
        }
        if (typeof(dates) == "logical") {
          dates <- tryCatch({as.POSIXct(data[,names(time_dim)])},
            error = function(x) {return(NA)},
            warning = function(x) {return(NA)})
        }
        if (typeof(dates) == "logical") {
          dates <- tryCatch({ym(data[,names(time_dim)])},
            error = function(x) {return(NA)},
            warning = function(x) {return(NA)})
        }
        if (typeof(dates) == "logical") {
          dates <- tryCatch({year(data[,names(time_dim)])},
            error = function(x) {return(NA)},
            warning = function(x) {return(NA)})
        }
        if (typeof(dates) == "logical") {
          stop("Cannot parse dates. Stopping")
        }
        all_years <- year(dates)
        years <- unique(all_years)
        all_months <- month(dates)
        months <- unique(all_months)
        all_days <- day(dates)
        days <- unique(all_days)
        hours <- hour(dates)
        minutes <- minute(dates)
        seconds <- second(dates)
        all_hours <- paste(hours, minutes, seconds, sep = ':')
        hours <- unique(all_hours)
      } else {
        dates <- data[,names(time_dim)]
        if (any(c("year", "Year") %in% names(time_dim))) {
          all_years <- dates
          years <- unique(all_years)
        } else {
          all_years <- rep(1, length(dates))
          years <- indices(1)
        }
        if (any(c("month", "Month") %in% names(time_dim))) {
          all_months <- dates
          months <- unique(all_months)
        } else {
          all_months <- rep(1, length(dates))
          months <- indices(1)
        }
        if (any(c("day", "Day") %in% names(time_dim))) {
          all_days <- dates
          days <- unique(all_days)
        } else {
          all_days <- rep(1, length(dates))
          days <- indices(1)
        }
        all_hours <- rep("00:00:00", length(dates))
        hours <- "00:00:00"
      }

      if (!(is.null(lat.col)) && !(is.null(lon.col))) {
        coords <- unique(paste0(data[,lat.col], '; ', data[,lon.col]))
        for (cc in 1:length(coords)){
          lat <- c(lat, as.integer(strsplit(coords[cc], '; ')[[1]][1]))
          lon <- c(lon, as.integer(strsplit(coords[cc], '; ')[[1]][2]))
        }
      } else {
        lat <- indices(1)
        lon <- indices(1)
      }

      # convert table to array
      #data.array <- array(data[,var.col], dim = c(latitude = length(lat), longitude = length(lon), time = length(dates))) 
      data.array <- array(data[, var.col], dim = c(dataset = 1, var = 1,
                                      year = length(years), month = length(months),
                                      day = length(days), time_of_day = length(hours),
                                      ensemble = 1,
                                      latitude = length(lat), longitude = length(lon))) 

      dates_tmp <- c(unique(dates),
                     rep(NA, length(years)*length(months)*length(days)*length(hours)-
                              length(unique(dates))))
      dim(dates_tmp) <- c(length(years), length(months),
                                   length(days), length(hours))
      names(dim(dates_tmp)) <- c("year", "month", "day", "time_of_day")

      for (hh in 1:length(hours)) {
        hhx <- which(all_hours == hours[hh])
        for (dd in 1:length(days)) {
          ddx <- which(all_days == days[dd])
          for (mm in 1:length(months)) {
            mmx <- which(all_months == months[mm])
            for (yy in 1:length(years)) {
              yyx <- which(all_years == years[yy])
              idxs <- Reduce(intersect, list(hhx, ddx, mmx, yyx))
                if (!(length(idxs) == 0)) {
                  dates_tmp[yy, mm, dd, hh] <- dates[idxs[1]]
                } else {
                  dates_tmp[yy, mm, dd, hh] <- NA
                }
              for (ln in 1:length(lon)) {
                lnx <- which(data[,lon.col]==lon[ln])
                for (ll in 1:length(lat)) {
                  llx <- which(data[,lat.col]==lat[ll])
                  idxs <- Reduce(intersect, list(lnx, llx, hhx, ddx, mmx, yyx))
                  if (!(length(idxs) == 0)) {
                    data.array[ , , yy, mm, dd, hh, , ll, ln] <- data[idxs, var.col]
                  } else {
                    data.array[ , , yy, mm, dd, hh, , ll, ln] <- NA
                  }
                }
              }
            }
          }
        }
      }

      dates <- dates_tmp

      # format to s2dv_cube
      data_tmp <- s2dv_cube(
                   data = data.array,
                   coords = list(dataset = 1,
                                 var = var,
                                 year = years,
                                 month = months,
                                 day = days,
                                 time_of_day = hours,
                                 ensemble = 1,
                                 latitude = lat,
                                 longitude = lon),
                   varName = var,
                   Dates = dates)
      #attr(data, 'class') <- 's2dv_cube'
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
      data <- CST_Subset(data_tmp, along = c('latitude', 'longitude'),
                         indices = list(lat_select, lon_select), drop = 'none')
      if (!is.null(metadata)) {
        data$attrs$Variable$metadata <- metadata
      }
      data$attrs$Dates <- list(Dates = dates)
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
          #start_parameters[paste0(f, "_across")] <- "sdate"
        }
        if ((data_type== "obs") && (f == "time")) {
          f <- paste0(f, "_of_day")
          start_parameters[paste0(f, "_across")] <- "sdate"
          synonims[f] <- c("time", 'time_of_day')
        }
        start_parameters[f] <- "all"
        return_vars[f] <- "sdate"
      }

      # if file includes ensemble dimension, add to startR parameters
      if (any(c('ensemble', 'member', 'number') %in% file.dims)){
        start_parameters$ensemble <- "all"
        synonims$ensemble <- c('ensemble', 'member', 'number')
      }

      if (any(lat_selection == 'all')){
        lat_select <- 'all'
      } else {
        lat_select <- values(list(min(lat_selection), max(lat_selection)))
      }
      if (any(lon_selection == 'all')){
        lon_select <- 'all'
      } else {
        lon_select <- values(list(min(lon_selection), max(lon_selection)))
      }

      # get latitude values
      if (any(c("latitude", "lat", 'lat_1', 'lat_2') %in% file.dims)) {
        lat.dim <- which(file.dims %in% c("latitude","lat",'lat_1','lat_2'))
        lat.length <- NcReadDims(full_path[1])[[lat.dim]]
        if (lat.length > 1) {
          synonims$latitude <- c("lat", 'latitude', 'lat_1', 'lat_2')
          return_vars$latitude <- 'dataset'
          start_parameters$latitude <- lat_select
          start_parameters$latitude_reorder <- Sort(decreasing = TRUE)
        } else {
          lat.val <- NcVarReader(file_path=full_path[1],
                                 var_name="lat",
                                 synonims=list(lat=c("lat",
                                                     "latitude",
                                                     "lat_1",
                                                     "lat_2")))[[1]]
        }
      } else {
        lat.val <- indices(1)
      }
      # get longitude values
      if (any(c("longitude", "lon", 'lon_1', 'lon_2') %in% file.dims)) {
        lon.dim <- which(file.dims %in% c("longitude","lon",'lon_1','lon_2'))
        lon.length <- NcReadDims(full_path[1])[[lon.dim]]
        if (lon.length > 1) {
          synonims$longitude <- c("lon", 'longitude','lon_1', 'lon_2')
          return_vars$longitude <- 'dataset'
          start_parameters$longitude <- lon_select
          start_parameters$longitude_reorder <- CircularSort(-180, 180)
        } else {
          lon.val <- NcVarReader(file_path=full_path[1],
                                 var_name="lon",
                                 synonims=list(lon=c("lon",
                                                     "longitude",
                                                     'lon_1',
                                                     'lon_2')))[[1]]
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
          time_attributes <- attr(data, "Variables")$common$time_of_day
        } else {
          time_attributes <- attr(data, "Variables")$common$lead_time
        }
        #reshape dates as needed
        metadata_dates <- as.POSIXct(time_attributes)

        years <- unique(year(metadata_dates))
        months <- unique(month(metadata_dates))
        days <- unique(day(metadata_dates))
        hours <- hour(metadata_dates)
        minutes <- minute(metadata_dates)
        hours <- paste(hours, minutes, sep = ':')
        hours <- unique(hours)

        dim(metadata_dates) <- c(length(years), length(months),
                                 length(days), length(hours))
        names(dim(metadata_dates)) <- c("year", "month", "day",
                                         "time_of_day")

        
        if (data_type == "obs") {
          start_parameters$time_of_day <- values(metadata_dates)
        } else {
          names(start_parameters)[names(start_parameters) == "lead_time"] <- "time_of_day"
          start_parameters$time_of_day <- values(metadata_dates)
          names(start_parameters$return_vars)[
            names(start_parameters$return_vars) == "lead_time"] <- "time_of_day"
          start_parameters$return_vars["time_of_day"] <- "sdate"
          start_parameters$time_of_day_across <- "sdate"
          names(start_parameters$synonims)[
            names(start_parameters$synonims) == "lead_time"] <- "time_of_day"
          start_parameters$synonims["time_of_day"] <- c("time", 'lead_time', 'time_of_day')
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
      # store loaded dates in Date format
      dates <- as.Date(data$attrs$Dates)
      data$attrs$Dates <- list(Dates = dates)
      if (!(data_type == "obs")) {
        data$attrs$Dates$metadata <- time_attributes
        data$attrs$Dates$sdate <- sdates
        #data$attrs$Dates$metadata["sdate"] <- time_attributes[, 1]
      }
      out_dims <- names(data$dim)
      

      if (!("year" %in% out_dims)) {
        data <- CST_InsertDim(data, 3, length(years),
                              "year", values = years)
      }
      if (!("month" %in% out_dims)) {
        data <- CST_InsertDim(data, 4, length(months),
                              "month", values = months)
      }
      if (!("day" %in% out_dims)) {                        
        data <- CST_InsertDim(data, 5, length(days),
                              "day", values = days)
      }
      if (!("time_of_day" %in% out_dims)) {                        
        data <- CST_InsertDim(data, 6, length(hours),
                              "time_of_day", values = hours)
      }

      data$coords$year <- years
      data$coords$month <- months
      data$coords$day <- days
      data$coords$time_of_day <- hours

      if (!("ensemble" %in% out_dims)) {
        data <- CST_InsertDim(data, 7,
                              1, "ensemble",
                              values = indices(1))
      }
      if (!("latitude" %in% out_dims)) {
        data <- CST_InsertDim(data, 8,
                              1, "latitude", values = lat.val)
      }
      if (!("longitude" %in% out_dims)) {
        data <- CST_InsertDim(data, 9,
                              1,"longitude", values = lon.val)
      }
      
    }
  return (data)

}