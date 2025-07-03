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
#' # # Forecast data included in the package
#' # fcst_path <- system.file("extdata/forecast/", package = "clim4health")
#' # fcst_path <- paste0(fcst_path, "/")
#' # 
#' # # Load the forecast data
#' # fcst <- clim4health_load(fcst_path, var = "t2m", data_type = "fcst",
#' #                          sdates = "20240401", ext = 'nc')
#' # class(fcst)
#' # dim(fcst$data)
#' # summary(fcst$data)
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
    if (!(data_type == "obs")) {
      stop("Cannot currently load forecast/hindcast data in csv format.")
    }
    data <- .clim4health_load_csv(path, var = var,
                                  date = date,
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
  print(full_path)
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

  if (all(headers == c("V1", "station", "longitude", "latitude",
                       "date", "hour", "tag", "value"))) {
    data <- .load_csv_tolimavalle(data, var = var, dates = date,
                                  lat_selection = lat_selection,
                                  lon_selection = lon_selection)
    return(data)
  } else {
    data <- .load_csv_unknown(data, var = var,
                              lat_selection = lat_selection,
                              lon_selection = lon_selection)
    return(data)
  }
}
