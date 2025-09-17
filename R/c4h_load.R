#' c4h_load
#'
#' Load forecast, hindcast, or observational climate data (reanalysis or
#' stations) as an s2dv_cube object.
#'
#' @param path String to the folder containing the files to be loaded, or a
#' vector of specific file paths.
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
#' # fcst <- c4h_load(fcst_path,
#'                    variable = "t2m",
#'                    year = 2025,
#'                    month = 1,
#'                    ext = 'nc')
#' # class(fcst)
#' # dim(fcst$data)
#' # summary(fcst$data)
#'
c4h_load <- function(path,
                     variable = NULL,
                     year = "all",
                     month = "all",
                     day = "all",
                     time = "all",
                     leadtime_month = "all",
                     ext = NULL,
                     bbox = NULL) {
  if (is.null(ext)) {
    stop("Please specify the file extension using the 'ext' parameter.")
  } else if (!(ext %in% c("nc", "csv"))) {
    # If the extension of the files is not "nc" or "csv", stop
    if (ext %in% c(".nc", ".csv")) {
      ext <- gsub("^\\.", "", ext)
    } else {
      stop(paste0("Please specify a valid file extension using the 'ext'",
                  " parameter. Valid options are 'nc', 'csv', '.nc', and ",
                  "'.csv'."))
    }
  }
  if (ext == "nc" && is.null(variable)) {
    stop("Please specify parameter 'variable' to load.")
  }

  # handle bbox parameter
  if (is.null(bbox)) {
    message(paste0("Note: parameter 'bbox' is not specified, loading all ",
                   "latitudes and longitudes."))
    lat_selection <- "all"
    lon_selection <- "all"
  } else {
    lat_selection <- c(bbox[3], bbox[1])
    lon_selection <- c(bbox[2], bbox[4])
  }

  # obtain files to load
  if (length(path) == 1) {
    # case: path is a single directory or file
    if (dir.exists(path)) {
      # obtain all files in <path> if path is a directory
      file_list <- list.files(path = path, pattern = paste0("\\.", ext),
                              full.names = TRUE)#, recursive = TRUE)
      if (length(file_list) == 0) {
        stop("No files found in the specified path.")
      }

      # if ext is nc, check which variables are in the files
      if (ext == "nc") {
        file_vars <- lapply(file_list, sf::gdal_subdatasets)
        file_vars <- unlist(file_vars)

        all_vars <- unique(gsub(".*:", "", file_vars))
        message(paste0("Variables found in 'path' are: '",
                       paste(all_vars, collapse = "', '"), "'. Requested ",
                       "variables are: '", paste(variable, collapse = "', '"),
                       "'."))
      }
      # if <variable> is specified and can be found in any file names, filter
      find_variable <- unlist(lapply(variable, grepl, file_list))
      if (!is.null(variable) && any(find_variable)) {
        file_list <- rep(file_list, length(variable))[find_variable]
      }

      # if <year> is specified and can be found in any file names, filter
      find_year <- unlist(lapply(year, grepl, file_list))
      if (!(any(year == "all")) && any(find_year)) {
        file_list <- rep(file_list, length(year))[find_year]
      }

      file_list <- sort(unique(file_list))
    } else if (file.exists(path)) {
      # case: path is a single file
      file_list <- path
    } else {
      stop("The specified 'path' does not exist.")
    }
  } else if (length(path) > 1) {
    # case: path is a vector of files
    file_list <- path
    # check if files exist
    file_exists <- file.exists(file_list)
    if (!all(file_exists)) {
      stop(paste0("The following files do not exist: ",
                  paste(file_list[!file_exists], collapse = ", ")))
    }
  }

  if (ext == "nc") { # read .nc files
    data <- .c4h_load_nc_stars(file_list = file_list,
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
    data <- .c4h_load_csv(file_list = file_list,
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