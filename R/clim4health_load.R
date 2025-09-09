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
#' # fcst <- clim4health_load(fcst_path, variable = "t2m",
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
                             ext = NULL,
                             bbox = NULL) {


  if (!(ext %in% c("nc", "csv"))) {
    # If the extension of the files is not "nc" or "csv", stop
    if (ext %in% c(".nc", ".csv")) {
      ext <- gsub("^\\.", "", ext)
    } else {
      stop("The type_file has to be csv or nc, no other type files",
           "are allowed at the moment.")
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

  if (!file.exists(path)) {
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

    file_list <- sort(unique(file_list))
  } else {
    # case: path is a single file
    file_list <- path
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
                                       lon_selection = lon_selection)
  }

  if (ext == "csv") { # Read .csv files
    data <- .clim4health_load_csv(file_list = file_list,
                                  variable = variable,
                                  year = year,
                                  month = month,
                                  day = day,
                                  time = time,
                                  leadtime_month = leadtime_month,
                                  lat_selection = lat_selection,
                                  lon_selection = lon_selection)
  }
  # return s2dv_cube from csv or nc
  return(data)
}

#' .clim4health_load_csv
#'
#' TBC
#'
#' @param full_path TBC
#' @param var TBC
#' @param date TBC
#' @param lat_selection TBC
#' @param lon_selection TBC
#'
#' @returns TBC
.clim4health_load_csv <- function(file_list,
                                  variable = NULL,
                                  year = "all",
                                  month = "all",
                                  day = "all",
                                  time = "all",
                                  leadtime_month = "all",
                                  lat_selection = "all",
                                  lon_selection = "all") {

  data <- data.table::fread(file_list)

  # skip up to 10 lines of header
  if (ncol(data) == 1) {
    for (i in 1:11) {
      if (ncol(data.table::fread(file_list, skip = i)) > 1) {
        break
      }
    }
    if (i == 11) {
      stop("Currently cannot handle >10 header lines in file.")
    } else {
      data <- data.table::fread(file_list, skip = i)
      metadata <- utils::head(data.table::fread(file_list), i - 1)
    }
  } else {
    metadata <- NULL
  }

  # read headers
  headers <- colnames(data)

  if (all(headers %in% c("V1", "station", "longitude", "latitude",
                         "date", "hour", "tag", "value"))) {
    data <- .load_csv_tolimavalle(data, headers,
                                  variable = variable,
                                  year = year,
                                  month = month,
                                  day = day,
                                  time = time,
                                  leadtime_month = leadtime_month,
                                  lat_selection = lat_selection,
                                  lon_selection = lon_selection)
  } else if (all(headers %in% c("prov", "year", "month", "day", "value",
                                "variable", "Estacion", "date"))) {
    data <- .load_csv_dr(data, headers,
                         variable = variable,
                         year = year,
                         month = month,
                         day = day,
                         time = time,
                         leadtime_month = leadtime_month)
  } else {
    data <- .load_csv_unknown(data, headers, metadata,
                              variable = variable,
                              year = year,
                              month = month,
                              day = day,
                              time = time,
                              leadtime_month = leadtime_month,
                              lat_selection = lat_selection,
                              lon_selection = lon_selection)
  }
  return(data)
}
