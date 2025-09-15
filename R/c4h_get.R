#' Download netcdf files from the climate data store
#'
#' @param pat Your personal acces token (PAT). You can find your PAT in your CDS
#' profile after logging in.
#' @param dataset The name of the dataset you want to load. available datasets
#' can be found via c4h_get_datasets().
#' @param product_type The product type that you want to load. Available
#' variables can be found via c4h_get_product_types().
#' @param variable The variable that you want to load. Available variables can
#' be found via c4h_get_variables().
#' @param year A vector of years, starting in 1950.
#' @param month A vector of months. e.g. c(1,2) for January and February.
#' Default is all months.
#' @param day A vector of days in the month. Default is all days.
#' @param time A vector of hours (0-23). Only needed when datasets are hourly.
#' Default is all hours.
#' @param bbox A vector of coordinates. (in order: North, West, South, East).
#' @param leadtime_month A vector of months for the forecast leadtime. e.g.
#' c(1,2) for January and February. Default is all months.
#' @param originating_centre DESCRIPTION PENDING!
#' @param system DESCRIPTION PENDING!
#' @param outname The name stem of the resultant netcdf file.
#' @param outpath The path where the resultant netcdf file should be saved.
#' Default is the current working directory.
#' @returns A netcdf file.
#'
#' @author Daniela Lührsen
#' @export
#'
#' @examples
#' \dontrun{
#' c4h_get(pat = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
#'                 dataset = "reanalysis-era5-land-monthly-means",
#'                 product_type = "monthly_averaged_reanalysis",
#'                 variable = "2m_temperature",
#'                 year = c(2010, 2011, 2012),
#'                 month = c(4, 5),
#'                 bbox = c(33, -93, -23, -17),
#'                 outname = "era5land")
#' 
#' c4h_get(pat = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
#'                 dataset = "seasonal-monthly-single-levels",
#'                 originating_centre = c("ecmwf"),
#'                 system = c("51"),
#'                 variable = c("2m_temperature"),
#'                 product_type = c("monthly_mean"),
#'                 year = c("2010", "2011", "2012"),
#'                 month = c("04"),
#'                 leadtime_month = c("1", "2", "3"),
#'                 bbox = c(33, -93, -23, -17),
#'                 outname = "hindcast")
#' 
#' c4h_get(pat = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
#'                 dataset = "seasonal-monthly-single-levels",
#'                 originating_centre = c("ecmwf"),
#'                 system = c("51"),
#'                 variable = c("2m_temperature"),
#'                 product_type = c("monthly_mean"),
#'                 year = c("2024"),
#'                 month = c("04"),
#'                 leadtime_month = c("1", "2", "3"),
#'                 bbox = c(33, -93, -23, -17),
#'                 outname = "forecast")
#' }

c4h_get <- function(pat,
                    dataset,
                    product_type,
                    variable,
                    year,
                    month = c(1:12),
                    day = c(1:31),
                    time = c(0),
                    bbox,
                    leadtime_month,
                    originating_centre,
                    system,
                    outname = paste0("cds_"),
                    outpath = getwd()) {
  area <- bbox
  rm(bbox)

  all_params <- c(as.list(environment()))

  # check the parameters of the function and give error messages
  datasets <- names(.datasets_info)

  # check whether the dataset is valid
  if (missing(dataset) || !dataset %in% datasets) {
    stop(paste0("Dataset is not valid. Dataset must be one of the following: ",
                paste(datasets, collapse = ", ")))
  }

  # check whether there are missing required parameters
  required_params <- names(.datasets_info[[dataset]])
  passed_params <- names(all_params[sapply(all_params,
                                           function(x) !is.symbol(x))])
  missing_params <- required_params[!required_params %in% passed_params]
  if (length(missing_params) != 0) {
    stop(paste0("You are missing parameters. For ", dataset,
                " the following additional parameters are required: ",
                paste(missing_params, collapse = ", ")))
  }

  # check if all required params have correct values
  for (p in required_params) {
    if (!all(all_params[[p]] %in% .datasets_info[[dataset]][[p]])) {
      if (p == "area") {
        if (!(length(area) == 4 && is.numeric(area) &&
                all(area[c(1, 3)] > -90) || all(area[c(1, 3)] < 90) &&
                all(area[c(1, 3)] > -90) || all(area[c(1, 3)] < 90) &&
                all(area[c(2, 4)] > -180) || all(area[c(1, 3)] < 180) &&
                all(area[c(2, 4)] > -180) || all(area[c(1, 3)] < 180))) {
          stop("Coordinates are out of bound. N,W,S,E coordinates, 
               latitudes from 90 to -90, longitudes -180 to 180")
        }
      } else {
        cat(paste0("Your value for ", p,
                   " is not valid. It must be one of the following: ",
                   paste(.datasets_info[[dataset]][[p]], collapse = ", ")))
        return(invisible(NULL))
      }
    }
  }


  if (!(is.numeric(year)) || any(year > format(Sys.Date(), "%Y")) ||
        any(year < 1950)) {
    stop("Years but be numeric and between 1950 and current year.")
  }
  if (!(is.numeric(month)) || any(month > 12) || any(month < 1)) {
    stop("Months  must be numeric and between 1 and 12.")
  }
  if (!(is.numeric(day)) || any(day > 31) || any(day < 1)) {
    stop("Days must be numeric and between 1 and 31.")
  }
  if (!is.numeric(time) || any(time > 23) || any(time < 0)) {
    stop("Times must be numeric and between 0 and 23.")
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
      if (lubridate::ymd(paste0(year_x, "-", sprintf("%02d", month_x),
                                "-01")) > Sys.Date()) {
        stop("Cannot download data for a future date.")
      }
      print(paste0("Loading data to: ", outpath, outname, "_", year_x,
                   sprintf("%02d", month_x), ".nc"))
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
        request[[i]] <- get(i)
      }
      # load data and print time it took
      t1 <- Sys.time()
      ecmwfr::wf_request(
        request,
        path = outpath
      )
      t2 <- Sys.time()
      print(paste0("Loaded ", month_x, "-", year_x, ": took ", t2 - t1))
      print(paste0("File saved to: ", outpath,
                   outname, "_", year_x, sprintf("%02d", month_x), ".nc"))
    }
  }
}
