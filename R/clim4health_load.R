#' Load forecast, hindcast, or observational climate data (reanalysis or stations)
#' as s2dv_cube object.
#'
#' @param path String to the folder containing the files to be loaded.
#' @param var String containing the name of the variable to be extracted from 
#' the files.
#' @param sdates List or vector of strings identifying which start dates to load.
#' @param data_type String indicating the type of data to be loaded. Options include
#' forecast ("fcst"), hindcast ("hcst") or observational ("obs") data.
#' @param ext File extension. Options include ".nc" and ".csv".
#' @param lat_selection Range of latitudes to subset the file and load the data.
#' @param lon_selection Range of longitudes to subset the file and load the data.
#'
#' @returns An s2dv_cube object.
#' @export
#'
#' @examples
#' # Forecast data included in the package
#' fcst_path <- system.file("extdata/forecast/", package = "clim4health")
#' fcst_path <- paste0(fcst_path, "/")
#'
#' # Load the forecast data
#' fcst <- clim4health_load(fcst_path, var = "t2m", data_type = "fcst",
#'                          sdates = "20240401", ext = 'nc')
#' class(fcst)
#' dim(fcst$data)
#' summary(fcst$data)

clim4health_load <- function(path, 
                             var = NULL, 
                             sdates = NULL,
                             data_type = NULL,
                             ext = 'nc',
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
  
  # Read .csv files
  if (ext == 'csv') {
    if (!(data_type == "obs")) {
      stop("Cannot currently load forecast/hindcast data in csv format.")
    }
    # read file
    data <- utils::read.csv(full_path)
    ### skip up to 10 lines of header
    if (ncol(data) == 1) {
      for (i in 1:11) {
        if (ncol(utils::read.csv(full_path, skip = i)) > 1) {
          break
        }
      }
      if (i == 11) {
        stop("Too many header lines in csv file, please delete some!")
      } else {
        data <- utils::read.csv(full_path, skip = i)
        metadata <- utils::head(utils::read.csv(full_path), i - 1)
      }
    } else {
      metadata <- NULL
    }
    
    # read headers
    headers <- colnames(data)
    ### get lats/lons ###
    if (any(c("y", "lat", "latitude") %in% headers)) {
      lat.col <- which(headers %in% c('y', 'lat', 'latitude'))
      all_lats <- data[, lat.col]
    } else {
      lat.col <- NULL
      all_lats <- rep(startR::indices(1), length(data[,1]))
    }
    if (any(c("x", "lon", "longitude") %in% headers)) {
      lon.col <- which(headers %in% c('x', 'lon', 'longitude'))
      all_lons <- data[, lon.col]
    } else {
      lon.col <- NULL
      all_lons <- rep(startR::indices(1), length(data[,1]))
    }
    
    ### get time columns ### test what happens if for example you have a year and month column
    time_dim <- list()
    for (f in c('time', 'date', 'day', 'week', 'month', 'year',
                'Time', 'Date', 'Day', 'Week', 'Month', 'Year')) {
      if (f %in% headers) {
        time_dim[f] <- which(headers == f)
      }
    }
    
    ### get variable ###
    if (!is.null(var)) {
      if (any(headers %in% var)) { # if var name exists in file
        var.col <- which(headers %in% var)
      } else { # if var name does not exist
        stop(paste0("var name ", var, " not found"))
      }
    }
    else {
      var.col <- length(headers)
      var <- headers[length(headers)]
    } # if no name of the var column, assume it is the last one
    
    # obtain metadata
    lat <- c() # initialise
    lon <- c() # initialise
    
    # run through possible time/date combinations #
    if (any(c("time", "Time", "date", "Date") %in% names(time_dim))) {
      # most likely stored as YYYY/MM/DD
      dates <- tryCatch({lubridate::ymd(data[,names(time_dim)])},
                        error = function(x) {return(NA)},
                        warning = function(x) {return(NA)})
      # could be stored as DD/MM/YYYY
      if (typeof(dates) == "logical") {
        dates <- tryCatch({lubridate::dmy(data[,names(time_dim)])},
                          error = function(x) {return(NA)},
                          warning = function(x) {return(NA)})
      }
      # could be stored as "YYYY/MM/DD hh:mm:ss"
      if (typeof(dates) == "logical") {
        dates <- tryCatch({as.POSIXct(data[,names(time_dim)])},
                          error = function(x) {return(NA)},
                          warning = function(x) {return(NA)})
      }
      # monthly data can be stored as YYYY/MM
      if (typeof(dates) == "logical") {
        dates <- tryCatch({lubridate::ym(data[,names(time_dim)])},
                          error = function(x) {return(NA)},
                          warning = function(x) {return(NA)})
      }
      # final option could be year is stored as date
      if (typeof(dates) == "logical") {
        dates <- tryCatch({lubridate::year(data[,names(time_dim)])},
                          error = function(x) {return(NA)},
                          warning = function(x) {return(NA)})
      }
      if (typeof(dates) == "logical") {
        stop("Cannot parse dates. Stopping")
      }
      ### now we need to retain all date values to ensure
      ### that our array will be reshaped correctly
      all_years <- lubridate::year(dates)
      years <- unique(all_years)
      all_months <- lubridate::month(dates)
      months <- unique(all_months)
      all_days <- lubridate::day(dates)
      days <- unique(all_days)
      hours <- lubridate::hour(dates)
      minutes <- lubridate::minute(dates)
      all_hours <- .paste_hours_minutes(hours, minutes)
      hours <- unique(all_hours)
    } else {
      ## if time_dim is not called time/date
      dates <- data[,names(time_dim)]
      if (any(c("year", "Year") %in% names(time_dim))) {
        year.col <- which(any(c("year", "Year") %in% headers))
        all_years <- data[,year.col]
        years <- unique(all_years)
      } else {
        all_years <- rep(1, length(t(dates)))
        years <- startR::indices(1)
      }
      if (any(c("month", "Month") %in% names(time_dim))) {
        month.col <- which(any(c("month", "Month") %in% headers))
        all_months <- data[,month.col]
        months <- unique(all_months)
        months <- .paste_zeros(months)
      } else {
        all_months <- rep(1, length(t(dates)))
        months <- startR::indices(1)
      }
      if (any(c("day", "Day") %in% names(time_dim))) {
        day.col <- which(any(c("day", "Day") %in% headers))
        all_days <- data[, day.col]
        days <- unique(all_days)
        days <- .paste_zeros(days)
      } else {
        all_days <- rep(1, length(t(dates)))
        days <- startR::indices(1)
      }
      ## assume that time of day is not stored
      all_hours <- rep("00:00", length(t(dates)))
      hours <- "00:00"
    }
    
    if (!(is.null(lat.col)) && !(is.null(lon.col))) {
      coords <- unique(paste0(data[,lat.col], '; ', data[,lon.col]))
      for (cc in 1:length(coords)){
        lat <- c(lat, as.integer(strsplit(coords[cc], '; ')[[1]][1]))
        lon <- c(lon, as.integer(strsplit(coords[cc], '; ')[[1]][2]))
      }
    } else {
      lat <- startR::indices(1)
      lon <- startR::indices(1)
    }
    
    # convert table to array of correct dimensions for s2dv_cube
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
    
    # reshape data and dates
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
              dates_tmp[yy, mm, dd, hh] <- paste0(
                years[yy],"-", months[mm],"-", days[dd]," ",hours[hh])
            } else {
              dates_tmp[yy, mm, dd, hh] <- NA
            }
            for (ln in 1:length(lon)) {
              lnx <- which(all_lons==lon[ln])
              for (ll in 1:length(lat)) {
                llx <- which(all_lats==lat[ll])
                idxs <- Reduce(intersect, list(lnx, llx, idxs))
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
    data_tmp <- CSTools::s2dv_cube(
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
    data <- CSTools::CST_Subset(data_tmp, along = c('latitude', 'longitude'),
                                indices = list(lat_select, lon_select), drop = 'none')
    
    if (!is.null(metadata)) {
      data$attrs$Variable$metadata <- metadata
    }
    data$attrs$Dates <- list(Dates = dates)
    
    
  } else if (ext == 'nc') { # Read .nc files
    dataset <- paste0(path, '$var$_$sdate$.nc')
    
    # read netcdf file dimensions of first file specified
    file.dims <- names(easyNCDF::NcReadDims(full_path[1]))
    
    # initialise startR parameters
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
      # treat case of "time" differently for obs/fcst
      if ((data_type != "obs") && (f == "time")) {
        f <- paste0("lead_", f)
        synonims$lead_time <- c("time", 'lead_time')
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
    
    # select lat and lon
    if (any(lat_selection == 'all')){
      lat_select <- 'all'
    } else {
      lat_select <- startR::values(list(min(lat_selection), max(lat_selection)))
    }
    if (any(lon_selection == 'all')){
      lon_select <- 'all'
    } else {
      lon_select <- startR::values(list(min(lon_selection), max(lon_selection)))
    }
    
    # get latitude values
    if (any(c("latitude", "lat", 'lat_1', 'lat_2') %in% file.dims)) {
      lat.dim <- which(file.dims %in% c("latitude","lat",'lat_1','lat_2'))
      lat.length <- easyNCDF::NcReadDims(full_path[1])[[lat.dim]]
      if (lat.length > 1) {
        synonims$latitude <- c("lat", 'latitude', 'lat_1', 'lat_2')
        return_vars$latitude <- 'dataset'
        start_parameters$latitude <- lat_select
        start_parameters$latitude_reorder <- startR::Sort(decreasing = TRUE)
      } else {
        # in case startR drops latitude value
        lat.val <- startR::NcVarReader(file_path=full_path[1],
                                       var_name="lat",
                                       synonims=list(lat=c("lat",
                                                           "latitude",
                                                           "lat_1",
                                                           "lat_2")))[[1]]
      }
    } else { # if lat is not stored in file
      lat.val <- startR::indices(1)
    }
    # get longitude values
    if (any(c("longitude", "lon", 'lon_1', 'lon_2') %in% file.dims)) {
      lon.dim <- which(file.dims %in% c("longitude","lon",'lon_1','lon_2'))
      lon.length <- easyNCDF::NcReadDims(full_path[1])[[lon.dim]]
      if (lon.length > 1) {
        synonims$longitude <- c("lon", 'longitude','lon_1', 'lon_2')
        return_vars$longitude <- 'dataset'
        start_parameters$longitude <- lon_select
        start_parameters$longitude_reorder <- startR::CircularSort(-180, 180)
      } else {
        # in case startR drops longitude value
        lon.val <- startR::NcVarReader(file_path=full_path[1],
                                       var_name="lon",
                                       synonims=list(lon=c("lon",
                                                           "longitude",
                                                           'lon_1',
                                                           'lon_2')))[[1]]
      }
    } else { # if lon is not stored in file
      lon.val <- startR::indices(1)
    }
    
    # define final parameters for startR calls
    start_parameters$synonims <- synonims
    start_parameters$return_vars <- return_vars
    start_parameters$retrieve <- FALSE
    start_parameters$split_multiselected_dims <- TRUE
    start_parameters$merge_across_dims <- TRUE
    start_parameters$merge_across_dims_narm <- TRUE
    start_parameters$largest_dims_length <- TRUE
    start_parameters$silent <- TRUE
    
    # Use startR function with retrieve=FALSE to get the dates
    data <- do.call(startR::Start, start_parameters)
    # keep dates
    if (any(c('time') %in% file.dims)){
      if (data_type == "obs") {
        time_attributes <- attr(data, "Variables")$common$time_of_day
      } else {
        time_attributes <- attr(data, "Variables")$common$lead_time
      }
      #reshape dates as needed
      metadata_dates <- as.POSIXct(time_attributes)
      
      years <- unique(lubridate::year(metadata_dates))
      months <- unique(lubridate::month(metadata_dates))
      days <- unique(lubridate::day(metadata_dates))
      hours <- lubridate::hour(metadata_dates)
      minutes <- lubridate::minute(metadata_dates)
      hours <- .paste_hours_minutes(hours, minutes)
      hours <- unique(hours)
      
      dim(metadata_dates) <- c(length(years), length(months),
                               length(days), length(hours))
      names(dim(metadata_dates)) <- c("year", "month", "day",
                                      "time_of_day")
      
      # load data with dates in required shape
      if (data_type == "obs") {
        start_parameters$time_of_day <- startR::values(metadata_dates)
      } else {
        names(start_parameters)[names(start_parameters) == "lead_time"] <- "time_of_day"
        start_parameters$time_of_day <- startR::values(metadata_dates)
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
        months <- .paste_zeros(months)
      } else {
        months <- startR::indices(1)
      }
      if ("day" %in% time_dims) {
        days <- attr(data, "Variables")$common$day
        days <- paste0(days)
      } else {
        days <- startR::indices(1)
      }
    }
    
    # load data as StartR array
    start_parameters$retrieve <- TRUE
    start_parameters$silent <- FALSE
    data <- do.call(startR::Start, start_parameters)
    
    # transform to sd2v_cube
    data <- CSTools::as.s2dv_cube(data)
    # store loaded dates in Date format
    dates <- as.Date(data$attrs$Dates)
    data$attrs$Dates <- list(Dates = dates)
    if (!(data_type == "obs")) {
      # store start dates and lead times
      data$attrs$Dates$metadata <- time_attributes
      data$attrs$Dates$sdate <- sdates
    }
    out_dims <- names(data$dim)
    
    # if any dimensions are missing, insert length 1
    if (!("year" %in% out_dims)) {
      data <- CSTools::CST_InsertDim(data, 3, length(years),
                                     "year", values = years)
    }
    if (!("month" %in% out_dims)) {
      data <- CSTools::CST_InsertDim(data, 4, length(months),
                                     "month", values = months)
    }
    if (!("day" %in% out_dims)) {                        
      data <- CSTools::CST_InsertDim(data, 5, length(days),
                                     "day", values = days)
    }
    if (!("time_of_day" %in% out_dims)) {                        
      data <- CSTools::CST_InsertDim(data, 6, length(hours),
                                     "time_of_day", values = hours)
    }
    
    # rename coordinates
    data$coords$year <- years
    data$coords$month <- months
    data$coords$day <- days
    data$coords$time_of_day <- hours
    
    if (!("ensemble" %in% out_dims)) {
      data <- CSTools::CST_InsertDim(data, 7,
                                     1, "ensemble",
                                     values = startR::indices(1))
    }
    if (!("latitude" %in% out_dims)) {
      data <- CSTools::CST_InsertDim(data, 8,
                                     1, "latitude", values = lat.val)
    }
    if (!("longitude" %in% out_dims)) {
      data <- CSTools::CST_InsertDim(data, 9,
                                     1,"longitude", values = lon.val)
    }
    
  }
  # return s2dv_cube
  return (data)
}