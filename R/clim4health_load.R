#' clim4health_load
#'
#' Load forecast, hindcast, or observational climate data (reanalysis or
#' stations) as an s2dv_cube object.
#'
#' @param path String to the folder containing the files to be loaded.
#' @param var String containing the name of the variable to be extracted from 
#' the files. It will be concatenated to path to identify the files to be
#' loaded.
#' @param sdates List or vector of strings identifying the start dates. It will 
#' be concatenated to path to identify the files to be loaded.
#' @param data_type String indicating the type of data to be loaded. Options
#' include forecast ("fcst"), hindcast ("hcst") or observational ("obs") data.
#' @param ext File extension. Options include ".nc" and ".csv".
#' @param lat_selection Range of latitudes to subset the file and load the data.
#' @param lon_selection Range of longitudes to subset the file and load the
#' data.
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
#'
clim4health_load <- function(path,
                             var = NULL,
                             date = NULL,
                             data_type = "",
                             ext = NULL,
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

  if (ext == "csv") { # Read .csv files
    if (!(is.null(date))) {
      full_path <- .paste_outer(paste0(path, var, "_"), date)
    } else {
      full_path <- paste0(path, var)
    }

    full_path <- .paste_outer(full_path, paste0(".", ext))

    if (!(data_type == "obs")) {
      stop("Cannot currently load forecast/hindcast data in csv format.")
    }
    data <- .clim4health_load_csv(full_path, var = var,
                                  date = date,
                                  data_type = data_type,
                                  lat_selection = lat_selection,
                                  lon_selection = lon_selection)
  } else if (ext == "nc") { # Read .nc files
    if (is.null(var)) {
      stop("Please specify var")
    } else if (is.null(date)) {
      stop("Please specify date")
    }
    full_path <- paste0(path, var, "_", date[1], ".", ext)
    data <- .clim4health_load_nc(var = var, path = path, full_path = full_path,
                                 lat_selection = lat_selection,
                                 lon_selection = lon_selection,
                                 date = date, data_type = data_type)

  }
  # return s2dv_cube from csv or nc
  return(data)
}



#' .clim4health_load_nc
#'
#' TBC
#'
#' @param var TBC
#' @param path TBC
#' @param full_path TBC
#' @param lat_selection TBC
#' @param lon_selection TBC
#' @param date TBC
#' @param data_type TBC
#'
#' @returns TBC
.clim4health_load_nc <- function(var, path, full_path,
                                 lat_selection, lon_selection,
                                 date, data_type) {

  # read netcdf file dimensions of first file specified
  file_dims <- easyNCDF::NcReadDims(full_path, var = var)

  if (data_type == "obs") {
    if ("valid_time" %in% names(file_dims)) {
      if (file_dims["valid_time"] == 1) {
        # if only one time step, monthly data
        data <- .load_cds_obs_monthly(var = var,
                                      full_path = full_path,
                                      path = path,
                                      date = date,
                                      lat_selection = lat_selection,
                                      lon_selection = lon_selection)
      } else {
        data <- .load_cds_obs_daily(var = var,
                                    full_path = full_path,
                                    path = path,
                                    date = date,
                                    lat_selection = lat_selection,
                                    lon_selection = lon_selection)
      }
    }
  } else {
    if ("forecast_reference_time" %in% names(file_dims)) {
      data <- .load_cds_seasonal_monthly(var = var,
                                         full_path = full_path,
                                         path = path,
                                         date = date,
                                         lat_selection = lat_selection,
                                         lon_selection = lon_selection)
    } else if ("forecast_period" %in% names(file_dims)) {
      data <- .load_cds_seasonal_daily()
    }
  }
  out_dims <- names(data$dim)
  # if any dimensions are missing, insert length 1
  if (!("sdate" %in% out_dims)) {
    data <- CSTools::CST_InsertDim(data, 3,
                                   1, "sdate",
                                   values = startR::indices(1))
  }
  if (!("ensemble" %in% out_dims)) {
    data <- CSTools::CST_InsertDim(data, 5,
                                   1, "ensemble",
                                   values = startR::indices(1))
  }
  if (!("latitude" %in% out_dims)) {
    data <- CSTools::CST_InsertDim(data, 6,
                                   1, "latitude", values = lat_val)
  }
  if (!("longitude" %in% out_dims)) {
    data <- CSTools::CST_InsertDim(data, 7,
                                   1, "longitude", values = lon_val)
  }
  return(data)
}

#' .clim4health_load_csv
#'
#' TBC
#'
#' @param full_path TBC
#' @param var TBC
#' @param date TBC
#' @param data_type TBC
#' @param lat_selection TBC
#' @param lon_selection TBC
#'
#' @returns TBC
.clim4health_load_csv <- function(full_path, var = NULL, date = NULL,
                                  data_type = NULL,
                                  lat_selection = NULL, lon_selection = NULL) {
  # read file
  data <- utils::read.csv(full_path)

  # skip up to 10 lines of header
  if (ncol(data) == 1) {
    for (i in 1:11) {
      if (ncol(utils::read.csv(full_path, skip = i)) > 1) {
        break
      }
    }
    if (i == 11) {
      stop("Currently cannot handle >10 header lines in file.")
    } else {
      data <- utils::read.csv(full_path, skip = i)
      metadata <- utils::head(utils::read.csv(full_path), i - 1)
    }
  } else {
    metadata <- NULL
  }

  # read headers
  headers <- colnames(data)
  # get lats/lons
  lats <- .get_csv_dims(headers, data, c("y", "lat", "latitude"))
  lons <- .get_csv_dims(headers, data, c("x", "lon", "longitude"))

  # get time columns
  ### test what happens if you have a year and month column
  possible_time_names <- .get_time_names()
  times <- .get_csv_dims(headers, data, possible_time_names)

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
      lubridate::year(times$val)
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
    lat <- startR::indices(1)
    lon <- startR::indices(1)
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
  data_tmp <- CSTools::s2dv_cube(data = data_array,
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
  data <- CSTools::CST_Subset(data_tmp, along = c("latitude", "longitude"),
                              indices = list(lat_select, lon_select),
                              drop = "none")

  if (!is.null(metadata)) {
    data$attrs$Variable$metadata <- metadata
  }
  return(data)
}
