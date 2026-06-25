#' c4h_get
#'
#' Download netCDF climate data from the Copernicus Climate Data Store.
#'
#' @param pat Your personal acces token (PAT). You can find your PAT in your CDS
#' profile after logging in.
#' @param dataset The name of the dataset you want to load. Available datasets
#' can be found via [c4h_get_help].
#' @param product_type The product type that you want to load. Available
#' variables can be found via [c4h_get_help].
#' @param variable The variable that you want to load. Available variables can
#' be found via [c4h_get_help].
#' @param year A vector of years, starting in 1950.
#' @param month A vector of months. e.g. c(1,2) for January and February.
#' Default is all months.
#' @param day A vector of days in the month. Default is all days.
#' @param time A vector of hours (0-23) in UTC time, only needed when datasets
#' are hourly. Note that you may need to convert your local time zone to UTC
#' to get the correct time points. Default is 0 (midnight UTC).
#' @param bbox A vector of coordinates. (in order: North, West, South, East).
#' @param leadtime_month A vector of months for the forecast leadtime. e.g.
#' c(1,2) for January and February. Default is all months.
#' @param originating_centre The originating centre of the forecast if
#' parameter `dataset` is "seasonal-monthly-single-levels". Options are "ecmwf",
#' "ukmo", "meteo_france", "dwd", "cmcc", "ncep", "jma", "eccc", and "bom".
#' There is no default value for this parameter.
#' @param system Model identifier of the forecast if parameter `dataset` is
#' "seasonal-monthly-single-levels". Options are 1, 2, 3, 4, 5, 6, 7, 8, 9, 12,
#' 13, 14, 15, 21, 22, 35, 51, 600, 601, 602, 603, 604, 605, 610. Valid options
#' depend on the value of originating_centre. There is no default value for this
#' parameter.
#' @param outname The name stem of the resultant netcdf file.
#' @param outpath The path where the resultant netcdf file should be saved.
#' Default is the current working directory.
#'
#' @returns A netcdf file or files.
#'
#' @seealso [c4h_get_help] to get help about the available datasets and
#' parameters used in `c4h_get`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download ERA5 land 2m temperature reanalysis data
#' c4h_get(pat = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
#'         dataset = "reanalysis-era5-land-monthly-means",
#'         product_type = "monthly_averaged_reanalysis",
#'         variable = "2m_temperature",
#'         year = c(2010, 2011, 2012),
#'         month = c(4, 5),
#'         bbox = c(33, -93, -23, -17),
#'         outname = "era5land")
#'
#' # Download 2m temperature system 51 ECMWF monthly seasonal forecasts
#' # to be used as forecasts and hindcasts respectively.
#' c4h_get(pat = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
#'         dataset = "seasonal-monthly-single-levels",
#'         originating_centre = c("ecmwf"),
#'         system = c("51"),
#'         variable = c("2m_temperature"),
#'         product_type = c("monthly_mean"),
#'         year = c(2010, 2011, 2012),
#'         month = 4,
#'         leadtime_month = 1:3,
#'         bbox = c(33, -93, -23, -17),
#'         outname = "hindcast")
#' c4h_get(pat = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
#'         dataset = "seasonal-monthly-single-levels",
#'         originating_centre = c("ecmwf"),
#'         system = c("51"),
#'         variable = c("2m_temperature"),
#'         product_type = c("monthly_mean"),
#'         year = 2024,
#'         month = 4,
#'         leadtime_month = 1:3,
#'         bbox = c(33, -93, -23, -17),
#'         outname = "forecast")
#' }

c4h_get <- function(
  pat,
  dataset,
  product_type,
  variable,
  year,
  month = 1:12,
  day = 1:31,
  time = 0,
  bbox,
  leadtime_month,
  originating_centre,
  system,
  outname = paste0("cds"),
  outpath = getwd()
) {
  all_params <- c(as.list(environment()))

  # check the parameters of the function and give error messages
  datasets <- names(.datasets_info)

  # check whether the dataset is valid
  if (missing(dataset) || !dataset %in% datasets) {
    stop(paste0(
      "Dataset is not valid. Dataset must be one of the following: ",
      paste(datasets, collapse = ", ")
    ))
  }

  # check whether there are missing required parameters
  required_params <- names(.datasets_info[[dataset]])
  passed_params <- names(all_params[sapply(all_params, function(x) {
    !is.symbol(x)
  })])
  missing_params <- required_params[!required_params %in% passed_params]
  if (length(missing_params) != 0) {
    stop(paste0(
      "You are missing parameters. For ",
      dataset,
      " the following additional parameters are required: ",
      paste(missing_params, collapse = ", ")
    ))
  }

  # check if all required params have correct values
  for (p in required_params) {
    if (p == "bbox") {
      if (
        !(length(bbox) == 4 &&
          is.numeric(bbox) &&
          (all(bbox[c(1, 3)] > -90) || all(bbox[c(1, 3)] < 90)) &&
          (all(bbox[c(1, 3)] > -90) || all(bbox[c(1, 3)] < 90)) &&
          (all(bbox[c(2, 4)] > -180) || all(bbox[c(1, 3)] < 180)) &&
          (all(bbox[c(2, 4)] > -180) || all(bbox[c(1, 3)] < 180)))
      ) {
        stop(
          "Coordinates are out of bound. N,W,S,E coordinates, 
               latitudes from 90 to -90, longitudes -180 to 180. Please adjust
                'bbox' parameter."
        )
      }
    } else if (!all(all_params[[p]] %in% .datasets_info[[dataset]][[p]])) {
      stop(paste0(
        "Your value for ",
        p,
        " is not valid. It must be one of the following: ",
        paste(.datasets_info[[dataset]][[p]], collapse = ", ")
      ))
    }
  }

  # give a correct filename, remove ".nc" (it will be added later again)
  if (outname != make.names(outname)) {
    outname <- make.names(outname)
  }
  if (substr(outname, nchar(outname) - 2, nchar(outname)) == ".nc") {
    outname <- substr(outname, 1, nchar(outname) - 3)
  }

  # Force absolute paths
  outpath <- paste0(normalizePath(outpath, mustWork = TRUE), "/")

  # select monthly data if monthly, other wise use the hourly data
  if (grepl("monthly", dataset)) {
    time <- "'00:00'"
  } else {
    time <- paste0(sprintf("%02d", time), ":00")
  }

  day <- sprintf("%02d", day)

  # set PAT
  ecmwfr::wf_set_key(
    key = pat
  )

  # loop over each month to request data
  for (year_x in year) {
    for (month_x in month) {
      if (
        lubridate::ymd(paste0(year_x, "-", sprintf("%02d", month_x), "-01")) >
          Sys.Date()
      ) {
        stop("Cannot download data for a future date.")
      }
      print(paste0(
        "Loading data to: ",
        outpath,
        outname,
        "_",
        year_x,
        sprintf("%02d", month_x),
        ".nc"
      ))
      # set mandatory parameters
      request <- list(
        download_format = "unarchived",
        data_format = "netcdf",
        target = paste0(outname, "_", year_x, sprintf("%02d", month_x), ".nc"),
        dataset_short_name = dataset,
        year = year_x,
        month = sprintf("%02d", month_x)
      )
      # add additional parameters
      for (i in required_params[!required_params %in% c("year", "month")]) {
        if (i == "bbox") {
          request[["area"]] <- get("bbox")
        } else {
          request[[i]] <- get(i)
        }
      }
      # load data and print time it took
      t1 <- Sys.time()
      ecmwfr::wf_request(
        request,
        path = outpath
      )
      t2 <- Sys.time()
      print(paste0("Loaded ", month_x, "-", year_x, ": took ", t2 - t1))
      print(paste0(
        "File saved to: ",
        outpath,
        outname,
        "_",
        year_x,
        sprintf("%02d", month_x),
        ".nc"
      ))
    }
  }
}
