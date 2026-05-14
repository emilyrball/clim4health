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
#' c(1,2) for January and February for data initialized in January.
#' Default is "all". If "all" for hindcast and forecast data, all leadtime
#' months will be loaded. If "all" for reanalysis or station data, data will be
#' loaded as a time series in the time dimension, and the sdate dimension will
#' be set to 1.
#' @param ext File extension. Options include "nc", ".nc", "csv", and ".csv".
#' @param bbox A vector of coordinates in order: c(lat_max, lon_min,
#' lat_min, lon_max).
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
#' fcst <- c4h_load(fcst_path,
#'                  variable = "t2m",
#'                  year = 2025,
#'                  month = 1,
#'                  ext = "nc")
#' class(fcst)
#' dim(fcst$data)
#' summary(fcst$data)
#'
#' # Hindcast data included in the package
#' hcst_path <- system.file("extdata/hindcast/", package = "clim4health")
#' hcst_path <- paste0(hcst_path, "/")
#'
#' # Load the hindcast data
#' hcst <- c4h_load(hcst_path,
#'                  variable = "t2m",
#'                  year = 2010:2012,
#'                  month = 1,
#'                  leadtime_month = 1:3,
#'                  ext = "nc")
#' class(hcst)
#' dim(hcst$data)
#' summary(hcst$data)
#'
#' # Reanalysis data included in the package
#' rean_path <- system.file("extdata/reanalysis/", package = "clim4health")
#' rean_path <- paste0(rean_path, "/")
#'
#' # Load the reanalysis data with dates to match the hindcast
#' rean <- c4h_load(rean_path,
#'                  variable = "t2m",
#'                  year = 2010:2012,
#'                  month = 1,
#'                  leadtime_month = 1:3,
#'                  ext = "nc")
#' class(rean)
#' dim(rean$data)
#' summary(rean$data)
#'
#' # Load the reanalysis data as a time series
#' rean <- c4h_load(rean_path,
#'                  variable = "t2m",
#'                  year = 2010,
#'                  month = 1:3,
#'                  leadtime_month = "all",
#'                  ext = "nc")
#' class(rean)
#' dim(rean$data)
#' summary(rean$data)
#'
#' # Load station data as a time series matching the reanalysis above
#' stat_path <- system.file("extdata/stations/", package = "clim4health")
#' stat_path <- paste0(stat_path, "/")
#'
#' station <- c4h_load(stat_path,
#'                     variable = "temp_mean",
#'                     year = 2010,
#'                     month = 1:3,
#'                     leadtime_month = "all",
#'                     ext = "csv")
#' class(station)
#' dim(station$data)
#' summary(station$data)
c4h_load <- function(
  path,
  variable = NULL,
  year = "all",
  month = "all",
  day = "all",
  time = "all",
  leadtime_month = "all",
  ext = NULL,
  bbox = NULL
) {
  if (is.null(ext)) {
    stop("Please specify the file extension using the 'ext' parameter.")
  } else if (!(ext %in% c("nc", "csv"))) {
    # If the extension of the files is not "nc" or "csv", stop
    if (ext %in% c(".nc", ".csv")) {
      ext <- gsub("^\\.", "", ext)
    } else {
      stop(paste0(
        "Please specify a valid file extension using the 'ext'",
        " parameter. Valid options are 'nc', 'csv', '.nc', and ",
        "'.csv'."
      ))
    }
  }

  # handle bbox parameter
  if (is.null(bbox)) {
    lat_selection <- "all"
    lon_selection <- "all"
  } else {
    if (!is.numeric(bbox) || length(bbox) != 4) {
      stop(paste0(
        "Parameter 'bbox' must be a numeric vector of length 4 ",
        "(c(lat_max, lon_min, lat_min, lon_max))."
      ))
    }
    lat_selection <- c(bbox[3], bbox[1])
    lon_selection <- c(bbox[2], bbox[4])
  }

  if (any(grep("esarchive/recon", path)) || any(grep("esarchive/exp", path))) {
    data <- .c4h_load_esarchive(path = path,
                                variable = variable,
                                year = year,
                                month = month,
                                day = day,
                                time = time,
                                leadtime_month = leadtime_month,
                                lat_selection = lat_selection,
                                lon_selection = lon_selection)
    return(data)
  }

  # obtain files to load
  if (length(path) == 1) {
    # case: path is a single directory or file
    if (dir.exists(path)) {
      # obtain all files in <path> if path is a directory
      file_list <- list.files(
        path = path,
        pattern = paste0("\\.", ext),
        full.names = TRUE
      )
      if (length(file_list) == 0) {
        stop("No files found in the specified 'path'.")
      }

      # if ext is nc, check which variables are in the files
      if (ext == "nc") {
        .get_variables_nc(file_list, variable = variable)
      }

      # if <variable> is specified and can be found in any file names, filter
      file_list <- .subset_file_list(file_list, variable)
      # if <year> is specified and can be found in any file names, filter
      file_list <- .subset_file_list(file_list, year)

      file_list <- sort(unique(file_list))
    } else if (file.exists(path)) {
      # case: path is a single file
      file_list <- path
      if (ext == "nc") {
        .get_variables_nc(file_list, variable = variable)
      }
    } else {
      stop("The specified 'path' does not exist.")
    }
  } else if (length(path) > 1) {
    # case: path is a vector of files
    file_list <- path
    # check if files exist
    file_exists <- file.exists(file_list)
    if (!all(file_exists)) {
      stop(paste0(
        "The following files do not exist: ",
        paste(file_list[!file_exists], collapse = ", ")
      ))
    }
  }

  if (ext == "nc") {
    # read .nc files
    data <- .c4h_load_nc_stars(
      file_list = file_list,
      variable = variable,
      year = year,
      month = month,
      day = day,
      time = time,
      leadtime_month = leadtime_month,
      lat_selection = lat_selection,
      lon_selection = lon_selection
    )
  } else if (ext == "csv") {
    # Read .csv files
    data <- .c4h_load_csv(
      file_list = file_list,
      variable = variable,
      year = year,
      month = month,
      day = day,
      time = time,
      leadtime_month = leadtime_month,
      lat_selection = lat_selection,
      lon_selection = lon_selection
    )
  }

  # CRS checks
  lats <- data$coords$lat
  if (!all(lats >= -90 & lats <= 90)) {
    stop(
      "Latitudes not in the interval [-90,90] have been found. ",
      "clim4health currently only works with a WGS84 CRS. ",
      "Aborting data loading."
    )
  }
  lons <- data$coords$lon
  if (!(all(lons >= -180 & lons <= 180) || all(lons >= 0 & lons <= 360))) {
    stop(
      "Longitudes not in the interval [-180, 180] or [0, 360] ",
      "have been found. clim4health currently only works ",
      "with a WGS84 CRS. Aborting data loading."
    )
  }

  # return s2dv_cube from csv or nc
  return(data)
}
