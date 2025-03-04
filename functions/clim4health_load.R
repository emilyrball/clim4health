# function to load data into R object as s2dv_cube (array with metadata)

# dependencies
library(startR)
library(s2dv)
library(lubridate)
library(easyNCDF)
library(CSTools)
library(ncdf4)

# helper functions
paste_hours_minutes <- function(hours, minutes) {
  hours <- paste0(hours)
  minutes <- paste0(minutes)
  hours <- paste_zeros(hours)
  minutes <- paste_zeros(minutes)
  return(paste(hours, minutes, sep = ":"))
}

paste_zeros <- function(times) {
  for (i in seq_along(times)) {
    if (nchar(times[i]) == 1) {
      times[i] <- paste0("0", times[i])
    }
  }
  return(times)
}

paste_outer <- function(path, ext) {
  path <- outer(path, ext, FUN = "paste0")
  path <- t(path)
  dim(path) <- NULL
  return(path)
}

spatial_select <- function(selection) {
  if (any(selection == "all")) {
    select <- "all"
  } else {
    select <- startR::values(list(min(selection), max(selection)))
  }
  return(select)
}

get_spatial_length <- function(dim_index, file_dims) {
  if (is.null(dim_index)) {
    dim_length <- NULL
  } else {
    dim_length <- file_dims[[dim_index]]
  }
  return(dim_length)
}

get_spatial_name <- function(file_dim_names, possible_names) {
  for (f in possible_names) {
    # add f to time_dims if included in file_dims
    if (any(grep(f, tolower(file_dim_names)))) {
      dim_name <- file_dim_names[grep(f, tolower(file_dim_names))]
      break
    }
  }
  return(dim_name)
}

get_time_names <- function() {
  return(c("year", "month", "week", "day", "date", "time"))
}

get_spatial_val <- function(dim_length, full_path, dim_name, dim) {
  if (is.null(dim_length)) {
    # if lat is not stored in file
    spatial_val <- indices(1)
  } else if (dim_length > 1) {
    spatial_val <- NULL
  } else if (dim_length == 1) {
    # in case startR drops value
    spatial_val <- startR::NcVarReader(file_path = full_path[1],
                                       var_name = dim,
                                       synonims = list(dim = dim_name))[[1]]
  }
  return(spatial_val)
}

get_start_parameters <- function(path, full_path, file_dims,
                                 dataset = NULL, var = NULL, sdates = NULL,
                                 data_type = NULL,
                                 lat_selection = NULL, lon_selection = NULL) {

  possible_time_names <- get_time_names()
  possible_lat_names <- c("latitude", "lat", "lat_1", "lat_2")
  possible_lon_names <- c("longitude", "lon", "lon_1", "lon_2")

  # file dim names
  file_dim_names <- names(file_dims)

  # initialise startR parameters
  synonims <- list()
  return_vars <- list()

  # extract time dimensions from files
  time_dims <- c()
  lower_time_dims <- c()

  for (f in possible_time_names) {
    # add f to time_dims if included in file_dims
    if (any(grep(f, tolower(file_dim_names)))) {
      dim_name_in_file <- file_dim_names[grep(f, tolower(file_dim_names))]
      time_dims <- c(time_dims, dim_name_in_file)
      lower_time_dims <- c(lower_time_dims, f)
      if (dim_name_in_file != f) {
        synonims[[f]] <- c(f, dim_name_in_file)
      }
    }
  }

  # define initial input parameters to startR
  start_parameters <- list(dataset = dataset,
                           var = var,
                           sdate = sdates)

  if ("time" %in% names(synonims)) {
    if (!(data_type == "obs")) {
      # if loading fcst/hcst, rename time to lead_time
      names(synonims)[which("time" == names(synonims))] <- "lead_time"
    } else {
      # if loading obs, rename time to time
      names(synonims)[which("time" == names(synonims))] <- "time"
    }
    # case of ecmwf seasonal forecast data downloaded using get
    if (all(c("forecastMonth", "forecast_reference_time")
            %in% file_dim_names)) {
      names(synonims)[which("lead_time" == names(synonims))] <- "time"
      names(synonims)[which("month" == names(synonims))] <- "lead_time"
    }
  } else {
    if (!(data_type == "obs")) {
      # if loading fcst/hcst, rename time to lead_time
      synonims$lead_time <- c("lead_time", "time")
    } else if ("time" %in% lower_time_dims) {
      start_parameters[["time"]] <- "all"
      return_vars[["time"]] <- "sdate"
    }
  }

  for (f in names(synonims)) {
    start_parameters[[f]] <- "all"
    return_vars[[f]] <- "sdate"
  }

  # if file includes ensemble dimension, add to startR parameters
  if (any(c("ensemble", "member", "number") %in% file_dim_names)) {
    start_parameters$ensemble <- "all"
    synonims$ensemble <- c("ensemble", "member", "number")
  }

  # select lat and lon
  lat_select <- spatial_select(lat_selection)
  lon_select <- spatial_select(lon_selection)

  lat_name <- get_spatial_name(file_dim_names, possible_lat_names)
  lon_name <- get_spatial_name(file_dim_names, possible_lon_names)

  lat_length <- get_spatial_length(lat_name, file_dims)
  lon_length <- get_spatial_length(lon_name, file_dims)

  lat_val <- get_spatial_val(lat_length, full_path, lat_name, "latitude")
  lon_val <- get_spatial_val(lon_length, full_path, lon_name, "longitude")

  if (is.null(lat_val)) {
    if (lat_name != "latitude") {
      synonims$latitude <- c("latitude", lat_name)
    }
    return_vars$latitude <- "dataset"
    start_parameters$latitude <- lat_select
    start_parameters$latitude_reorder <- startR::Sort(decreasing = TRUE)
  }
  if (is.null(lon_val)) {
    if (lon_name != "longitude") {
      synonims$longitude <- c("longitude", lon_name)
    }
    return_vars$longitude <- "dataset"
    start_parameters$longitude <- lon_select
    start_parameters$longitude_reorder <- startR::CircularSort(-180, 180)
  }

  # define final parameters for startR calls
  start_parameters$synonims <- synonims
  start_parameters$return_vars <- return_vars
  start_parameters$split_multiselected_dims <- TRUE
  start_parameters$merge_across_dims <- TRUE
  start_parameters$merge_across_dims_narm <- TRUE
  start_parameters$largest_dims_length <- TRUE

  return(list(sp = start_parameters, lat = lat_val, lon = lon_val))
}

get_csv_dims <- function(headers, data, names) {
  if (any(names %in% headers)) {
    col <- which(headers %in% names)
    val <- data[, col]
  } else {
    col <- NULL
    val <- rep(indices(1), length(data[, 1]))
  }
  return(list(val = val, col = col))
}

clim4health_load_nc <- function(var, path, full_path,
                                lat_selection, lon_selection,
                                sdates, data_type) {
  # if one variable is provided, assume the path
  # is the directory containing the files
  if (length(var) == 1) {
    dataset <- paste0(path, "$var$_$sdate$.nc")
  } else {
    # if more than one variable is provided, assume
    # files are stored as path/var/var_sdate.nc
    dataset <- paste0(path, "$var$/$var$_$sdate$.nc")
  }

  # read netcdf file dimensions of first file specified
  ### tryCatch alternative for era5land data?
  file_dims <- easyNCDF::NcReadDims(full_path[1], var = var)
  parameters <- list(path = path,
                     full_path = full_path,
                     file_dims = file_dims,
                     dataset = dataset,
                     var = var,
                     sdates = sdates,
                     data_type = data_type,
                     lat_selection = lat_selection,
                     lon_selection = lon_selection)

  start_parameters <- do.call(get_start_parameters, parameters)
  lat_val <- start_parameters$lat
  lon_val <- start_parameters$lon
  start_parameters <- start_parameters$sp

  start_parameters$retrieve <- FALSE
  start_parameters$silent <- TRUE
  # Use startR function with retrieve = FALSE to get the dates
  data <- do.call(startR::Start, start_parameters)

  if (any(c("time", "valid_time") %in% names(file_dims))) {
    if (data_type == "obs") {
      time_attributes <- attr(data, "Variables")$common$time
    } else {
      time_attributes <- attr(data, "Variables")$common$lead_time
    }

    metadata_dates <- as.POSIXct(time_attributes)

    # load data with dates in required shape
    if (!(data_type == "obs")) {
      # rename lead_time to time
      names(dim(metadata_dates))[names(dim(metadata_dates)) ==
                                   "lead_time"] <- "time"
      names(start_parameters)[names(start_parameters) ==
                                "lead_time"] <- "time"
      names(start_parameters$return_vars)[names(start_parameters$return_vars)
                                          == "lead_time"] <- "time"
      names(start_parameters$synonims)[names(start_parameters$synonims) ==
                                         "lead_time"] <- "time"
      start_parameters$synonims[["time"]] <-
        unique(c(start_parameters$synonims[["time"]], "lead_time"))
    }
    start_parameters$time <- "all"
    start_parameters$time_across <- "sdate"
  } else if (all(c("forecastMonth", "forecast_reference_time")
                 %in% names(file_dims))) {
    # case of ecmwf seasonal forecast data downloaded using get
    time_attributes <- attr(data, "Variables")$common$lead_time
    start_dates <- attr(data, "Variables")$common$time

    metadata_dates <- start_dates %m+% months(time_attributes - 1)

    dim(metadata_dates) <- dim(time_attributes)
    names(dim(metadata_dates)) <- c("sdate", "time")

    start_parameters$time <- "all"
    start_parameters$time_across <- "sdate"
  }

  # load data as StartR array
  start_parameters$retrieve <- TRUE
  start_parameters$silent <- FALSE

  data <- do.call(Start, start_parameters)

  # transform to sd2v_cube
  data <- as.s2dv_cube(data)
  dates <- metadata_dates

  if (all(c("forecastMonth", "forecast_reference_time")
          %in% names(file_dims))) {
    data <- CST_MergeDims(data, merge_dims = c("lead_time", "time"),
                          rename_dim = "time", na.rm = FALSE)
    data$coords$time <- dates
    dim(data$coords$time) <- NULL
  }

  data$attrs$Dates <- dates
  if (!(data_type == "obs")) {
    # store start dates and lead times
    data$attrs$sdate$metadata <- time_attributes
    sdates_new <- metadata_dates[, 1]
    if (!(length(sdates_new) == length(sdates))) {
      sdates_new <- metadata_dates[1, ]
    }
    data$attrs$sdate$vals <- sdates_new
  }
  out_dims <- names(data$dim)

  # if any dimensions are missing, insert length 1
  if (!("ensemble" %in% out_dims)) {
    data <- CST_InsertDim(data, 4,
                          1, "ensemble",
                          values = indices(1))
  }
  if (!("latitude" %in% out_dims)) {
    data <- CST_InsertDim(data, 5,
                          1, "latitude", values = lat_val)
  }
  if (!("longitude" %in% out_dims)) {
    data <- CST_InsertDim(data, 6,
                          1, "longitude", values = lon_val)
  }
  return(data)
}

clim4health_load_csv <- function(full_path, var = NULL, sdates = NULL,
                                 data_type = NULL,
                                 lat_selection = NULL, lon_selection = NULL) {
  # read file
  data <- read.csv(full_path)

  # skip up to 10 lines of header
  if (ncol(data) == 1) {
    for (i in 1:11) {
      if (ncol(read.csv(full_path, skip = i)) > 1) {
        break
      }
    }
    if (i == 11) {
      stop("Currently cannot handle >10 header lines in file.")
    } else {
      data <- read.csv(full_path, skip = i)
      metadata <- head(read.csv(full_path), i - 1)
    }
  } else {
    metadata <- NULL
  }

  # read headers
  headers <- colnames(data)
  # get lats/lons
  lats <- get_csv_dims(headers, data, c("y", "lat", "latitude"))
  lons <- get_csv_dims(headers, data, c("x", "lon", "longitude"))

  # get time columns
  ### test what happens if you have a year and month column
  possible_time_names <- get_time_names()
  times <- get_csv_dims(headers, data, possible_time_names)

  vars <- list()
  # get variable
  if (!is.null(var)) {
    if (any(headers %in% var)) {
      # if var name exists in file
      vars$col <- which(headers %in% var)
    } else {
      # if var name does not exist
      stop(paste0("var name ", var, " not found"))
    }
  } else {
    # if variable is not specified assume it is the last one
    vars$col <- length(headers)
    var <- headers[length(headers)]
  }
  vars$val <- data[, vars$col]

  # run through possible time/date combinations
  ### what about case YYYY and MM stored in separate columns?
  print("Trying date formats")
  sum_na <- length(times$val)
  dates <- NA
  for (f in list(lubridate::ymd, lubridate::dmy, lubridate::mdy,
                 lubridate::ym, lubridate::my)) {
    dates_tmp <- f(times$val)
    if (sum(is.na(dates_tmp)) < sum_na) {
      dates <- dates_all <- dates_tmp
      sum_na <- sum(is.na(dates_tmp))
      ## choose a date format that produces least NAs
    }
  }
  if (!typeof(dates) == "logical") {
    dates <- unique(dates)
  }

  # could be stored as "YYYY/MM/DD hh:mm:ss"
  print("Trying YYYY/MM/DD hh:mm:ss date format")
  dates_tmp <- tryCatch({
    as.POSIXct(times$val, tz = "UTC")
  },
  error = function(x) {
    return(NA)
  },
  warning = function(x) {
    return(NA)
  })

  # if length of dates_tmp is longer then hourly data could be stored
  if (length(unique(dates_tmp)) > length(dates)) {
    dates <- dates_all <- dates_tmp
  }

  # final option could be year is stored as date
  if (typeof(dates) == "logical") {
    print("Trying YYYY date format")
    dates <- tryCatch({
      year(times$val)
    },
    error = function(x) {
      return(NA)
    },
    warning = function(x) {
      return(NA)
    })
    dates_all <- dates
  }
  if (typeof(dates) == "logical") {
    stop("Cannot parse dates. Stopping")
  }

  dates <- unique(dates)

  # obtain metadata
  lat <- c() # initialise
  lon <- c() # initialise

  if (!(is.null(lats$col)) && !(is.null(lons$col))) {
    coords <- unique(paste0(lats$val, "; ", lons$val))
    for (cc in seq_along(coords)){
      lat <- c(lat, as.integer(strsplit(coords[cc], "; ")[[1]][1]))
      lon <- c(lon, as.integer(strsplit(coords[cc], "; ")[[1]][2]))
    }
  } else {
    lat <- indices(1)
    lon <- indices(1)
  }

  # convert table to array of correct dimensions for s2dv_cube
  data_array <- array(data[, vars$col],
                      dim = c(dataset = 1, var = 1,
                              time = length(dates),
                              ensemble = 1,
                              latitude = length(lat), longitude = length(lon)))

  # reshape data and dates
  for (tt in seq_along(dates)) {
    ttx <- which(dates_all == dates[tt])
    for (ln in seq_along(lon)) {
      lnx <- which(lons$val == lon[ln])
      for (ll in seq_along(lat)) {
        llx <- which(lats$val == lat[ll])
        idxs <- Reduce(intersect, list(lnx, llx, ttx))
        if (!(length(idxs) == 0)) {
          data_array[, , tt, , ll, ln] <- data[idxs, vars$col]
        } else {
          data_array[, , tt, , ll, ln] <- NA
        }
      }
    }
  }

  # format to s2dv_cube
  data_tmp <- s2dv_cube(data = data_array,
                        coords = list(dataset = 1,
                                      var = var,
                                      time = dates,
                                      ensemble = 1,
                                      latitude = lat,
                                      longitude = lon),
                        varName = var,
                        Dates = dates)

  # subset to only the requested coordinates (lat_selection and lon_selection)
  if (any(lat_selection == "all")) {
    lat_select <- seq_along(lat)
  } else {
    lat_select <- which(lat >= lat_selection[1] & lat <= lat_selection[2])
  }
  if (any(lon_selection == "all")) {
    lon_select <- seq_along(lon)
  } else {
    lon_select <- which(lon >= lon_selection[1] & lon <= lon_selection[2])
  }
  data <- CST_Subset(data_tmp, along = c("latitude", "longitude"),
                     indices = list(lat_select, lon_select), drop = "none")

  if (!is.null(metadata)) {
    data$attrs$Variable$metadata <- metadata
  }
  return(data)
}

# main function
clim4health_load <- function(path, var = NULL, sdates = NULL,
                             data_type = "", ext = NULL,
                             lat_selection = "all",
                             lon_selection = "all") {

  if (!(data_type %in% c("fcst", "hcst", "obs"))) {
    # user must select whether data is observation or forecast/hindcast
    stop("Please select data_type. Valid options",
         "are \"fcst\", \"hcst\", or \"obs\".")
  } else if (!(ext %in% c("nc", "csv"))) {
    # If the extension of the files is not "nc" or "csv", stop
    stop("The type_file has to be csv or nc, no other type files",
         "are allowed at the moment")
  }
  if (!(is.null(sdates))) {
    full_path <- paste_outer(paste0(path, var, "_"), sdates)
  } else {
    full_path <- paste0(path, var)
  }

  full_path <- paste_outer(full_path, paste0(".", ext))


  if (ext == "csv") { # Read .csv files
    if (!(data_type == "obs")) {
      stop("Cannot currently load forecast/hindcast data in csv format.")
    }
    data <- clim4health_load_csv(full_path, var = var,
                                 sdate = sdates,
                                 data_type = data_type,
                                 lat_selection = lat_selection,
                                 lon_selection = lon_selection)
  } else if (ext == "nc") { # Read .nc files
    if (is.null(var)) {
      stop("Please specify var")
    } else if (is.null(sdates)) {
      stop("Please specify sdates")
    }

    data <- clim4health_load_nc(var = var, path = path, full_path = full_path,
                                lat_selection = lat_selection,
                                lon_selection = lon_selection,
                                sdate = sdates, data_type = data_type)
  }
  # return s2dv_cube from csv or nc
  return(data)
}