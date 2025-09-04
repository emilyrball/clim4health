#' clim4health_load
#'
#' Load forecast, hindcast, or observational climate data (reanalysis or
#' stations) as an s2dv_cube object.
#'
#' @param path String to the folder containing the files to be loaded.
#' @param variable The variable that you want to load.
#' @param year A vector of years, starting in 1950.
#' @param month A vector of months. e.g. c(1,2) for January and February.
#' Default is all months.
#' @param day A vector of days in the month. Default is all days.
#' @param time A vector of hours (0-23). Only needed when datasets are hourly.
#' Default is all hours.
#' @param leadtime_month A vector of months for the forecast leadtime. e.g.
#' c(1,2) for January and February. Default is all months.
#' @param data_type String indicating the type of data to be loaded. Options
#' include forecast ("fcst"), hindcast ("hcst") or observational ("obs") data.
#' @param ext File extension. Options include "nc", ".nc", "csv", and ".csv".
#' @param bbox A vector of coordinates. (in order: North, West, South, East).
#'
#' @returns An s2dv_cube object.
#' @import dplyr
#' @export
#'
#' @examples
#' # # Forecast data included in the package
#' # fcst_path <- system.file("extdata/forecast/", package = "clim4health")
#' # fcst_path <- paste0(fcst_path, "/")
#' # 
#' # # Load the forecast data
#' # fcst <- clim4health_load(fcst_path, variable = "t2m", data_type = "fcst",
#' #                          sdates = "20240401", ext = 'nc')
#' # class(fcst)
#' # dim(fcst$data)
#' # summary(fcst$data)
#'
clim4health_load <- function(path,
                             variable = NULL,
                             year = "all",
                             month = "all",
                             day = "all",
                             time = "all",
                             leadtime_month = "all",
                             data_type = NULL,
                             ext = NULL,
                             bbox = NULL) {

  if (!(data_type %in% c("fcst", "hcst", "obs"))) {
    # user must select whether data is observation or forecast/hindcast
    stop("Please select data_type. Valid options",
         "are \"fcst\", \"hcst\", or \"obs\".")
  } else if (!(ext %in% c("nc", "csv"))) {
    # If the extension of the files is not "nc" or "csv", stop
    if (ext %in% c(".nc", ".csv")) {
      ext <- gsub("^\\.", "", ext)
    } else {
      stop("The type_file has to be csv or nc, no other type files",
           "are allowed at the moment")
    }
  }
  if (ext == "nc" && is.null(variable)) {
    stop("Please specify parameter 'variable' to load.")
  }

  if (is.null(bbox)) {
    warning("'bbox' is not specified, loading all latitudes and longitudes.")
    lat_selection <- "all"
    lon_selection <- "all"
  } else {
    lat_selection <- c(bbox[3], bbox[1])
    lon_selection <- c(bbox[2], bbox[4])
  }

  # obtain all files in <path>
  file_list <- list.files(path = path, pattern = paste0("\\.", ext),
                           full.names = TRUE, recursive = TRUE)
  if (length(file_list) == 0) {
    stop("No files found in the specified path.")
  }
  # if <variable> is specified and can be found in any file names, filter
  find_variable <- unlist(lapply(variable, grepl, file_list))
  if (!is.null(variable) & any(find_variable)) {
    file_list <- rep(file_list, length(variable))[find_variable]
  }

  find_year <- unlist(lapply(year, grepl, file_list))
  if (!(any(year== "all")) & any(find_year)) {
    file_list <- rep(file_list, length(year))[find_year]
  }

  if (ext == "nc") {
    data <- .clim4health_load_nc_stars(file_list = file_list,
                                       variable = variable,
                                       year = year,
                                       month = month,
                                       day = day,
                                       time = time,
                                       leadtime_month = leadtime_month,
                                       lat_selection = lat_selection,
                                       lon_selection = lon_selection,
                                       data_type = data_type)
  }

  if (ext == "csv") { # Read .csv files
    if (!(data_type == "obs")) {
      stop("Cannot currently load forecast/hindcast data in csv format.")
    }
    data <- .clim4health_load_csv(path, var = variable,
                                  date = date,
                                  lat_selection = lat_selection,
                                  lon_selection = lon_selection)
  } else if (ext == "nc") { # Read .nc files
  #  if (is.null(variable)) {
  #    stop("Please specify variable")
  #  } else if (is.null(date)) {
  #    stop("Please specify date")
  #  }
  #  #full_path <- paste0(path, variable, "_", date[1], ".", ext)
  #  full_path <- file_list[1]
  #  data <- .clim4health_load_nc(var = variable, path = path, full_path = full_path,
  #                               lat_selection = lat_selection,
  #                               lon_selection = lon_selection,
  #                               date = date, data_type = data_type)
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
.clim4health_load_csv <- function(path, var = NULL, date = NULL,
                                  lat_selection = "all",
                                  lon_selection = "all") {
  # read file
  if (!(is.null(date))) {
    if (file.exists(paste0(path, "_", date[1], ".csv"))) {
      full_path <- .paste_outer(paste0(path, "_"), date)
    } else {
      full_path <- paste0(path)
    }
  } else {
    full_path <- paste0(path)
  }

  full_path <- .paste_outer(full_path, paste0(".csv"))

  data <- data.table::fread(full_path)

  # skip up to 10 lines of header
  if (ncol(data) == 1) {
    for (i in 1:11) {
      if (ncol(data.table::fread(full_path, skip = i)) > 1) {
        break
      }
    }
    if (i == 11) {
      stop("Currently cannot handle >10 header lines in file.")
    } else {
      data <- data.table::fread(full_path, skip = i)
      metadata <- utils::head(data.table::fread(full_path), i - 1)
    }
  } else {
    metadata <- NULL
  }

  # read headers
  headers <- colnames(data)

  if (all(headers %in% c("V1", "station", "longitude", "latitude",
                         "date", "hour", "tag", "value"))) {
    data <- .load_csv_tolimavalle(data, var = var, dates = date,
                                  lat_selection = lat_selection,
                                  lon_selection = lon_selection)
    return(data)
  } else if (all(headers %in% c("prov", "year", "month", "day", "value",
                                "variable", "Estacion", "date"))) {
    data <- .load_csv_dr(data, var = var, dates = date)
  } else {
    data <- .load_csv_unknown(data, var = var,
                              lat_selection = lat_selection,
                              lon_selection = lon_selection)
    return(data)
  }
}
