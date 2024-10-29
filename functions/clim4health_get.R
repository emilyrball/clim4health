#' Download netcdf files from the climate data store
#'
#' @param pat Your personal acces token (PAT). You can find your PAT in your CDS
#' profile after logging in.
#' @param dataset The name of the dataset you want to load. available datasets
#' can be found via clim4health_get_datasets().
#' @param product_type The product type that you want to load. Available
#' variables can be found via clim4health_get_product_types().
#' @param variable The variable that you want to load. Available variables can
#' be found via clim4health_get_variables().
#' @param years A vector of years, starting in 1950.
#' @param months A vector of months. e.g. c(1,2) for January and February.
#' Default is all months.
#' @param days A vector of days in the month. Default is all days.
#' @param times A vector of hours (0-23). Only needed when datasets are hourly.
#' Default is all hours.
#' @param coords A vector of coordinates. (in order: South, North, West, East)
#' @param outname The name (and path) of the resultant netcdf file
#' @returns A netcdf file.
#' @examples
#' clim4health_get(pat = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
#' "hourly", years = c(2020:2021),
#' months = c(1:12), days = c(1:31), times = c(1:31), coords = coords)
#' add(10, 1)
#' @author Daniela Lührsen
#' @import ecmwfr


clim4health_get(pat = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
                "hourly", years = c(2020:2021),
                months = c(1:12), days = c(1:31), times = c(1:31),
                coords = c(43, 2.5, 42.5, 3))

clim4health_get(pat = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
                "hourly", years = c(2020:2021),
                months = c(1), days = c(1), times = c(1:31),
                coords = c(43, 2.5, 42.5, 3))

clim4health_get(pat = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
                years = c(2020), coords = c(43, 2.9, 42.9, 3),
                outname = "dailytest")

source("dataset_info.R")

clim4health_get <- function(pat,
                            dataset,
                            product_type,
                            variable,
                            years,
                            months = c(1:12),
                            days = c(1:31),
                            times = c(0:23),
                            coords,
                            outname = paste0("clim4health_")) {

  # check the parameters of the function and give error messages
  datasets <- names(datasets_info)

  if (!dataset %in% datasets){
    stop(paste0("Dataset is not valid. Dataset must be one of the following: ",
                paste(datasets, collapse = ", ")))
  }
  if (!product_type %in% datasets_info[[dataset]][["product_types"]]) {
    stop(paste0("Product type is not valid. For the selected dataset,
                product type must be one of the following: ",
                paste(datasets_info[[dataset]][["product_types"]],
                      collapse = ", ")))
  }
  if (!variable %in% datasets_info[[dataset]][["variables"]]) {
    stop(paste0("Variable is not valid. For the selected dataset,
         variable must be one of the following: ",
         paste(datasets_info[[dataset]][["variables"]], collapse = ", ")))
  }
  if (!(is.numeric(years)) || any(years > format(Sys.Date(), "%Y")) || any(years < 1950)) {
    stop("Years but be numeric and between 1950 and current year.")
  }
  if (!(is.numeric(months)) || any(months > 12) || any(months < 1)) {
    stop("Months  must be numeric and between 1 and 12.")
  }
  if (!(is.numeric(days)) || any(days > 31) || any(days < 1)) {
    stop("Days must be numeric and between 1 and 31.")
  }
  if (!is.numeric(times) || any(times > 23) || any(times < 0)) {
    stop("Times must be numeric and between 0 and 23.")
  } 
  if (!(length(coords) == 4 && is.numeric(coords) &
          all(coords[c(1, 3)] > -90) || all(coords[c(1, 3)] < 90) &&
          all(coords[c(1, 3)] > -90) || all(coords[c(1, 3)] < 90) &&
          all(coords[c(2, 4)] > -180) || all(coords[c(1, 3)] < 180) &&
          all(coords[c(2, 4)] > -180) || all(coords[c(1, 3)] < 180))) {
    stop("Coordinates are out of bound.")
  }

  # give a correct filename, remove ".nc" (it will be added later again)
  if (outname != make.names(outname)) {
    outname <- make.names(outname)
  }
  if (substr(outname, nchar(outname) - 2, nchar(outname)) == ".nc") {
    outname <- substr(outname, 1, nchar(outname) - 3)
  }
  # select monthly data if monthly, other wise use the hourly data
  if (grepl("monthly",dataset)) {
    time <- "'00:00'"
  } else {
    time <- paste0(sprintf("%02d", times), ":00")
  }

  # set PAT
  ecmwfr::wf_set_key(
    key = pat
  )

  # loop over each month to request data
  for (year in years) {
    for (month in months) {
      request <- list(
        dataset_short_name = dataset,
        product_type = product_type,
        variable = variable,
        year = year,
        month = month,
        day = days,
        time = time,
        data_format = "netcdf",
        download_format = "unarchived",
        area = coords,
        target = paste0(outname, "_", year, sprintf("%02d", month), ".nc")
      )
      t1 <- Sys.time()
      ecmwfr::wf_request(
        request,
        path = "."
      )
      t2 <- Sys.time()
      print(paste0(month, "-", year, " took ", t2 - t1))
    }
  }
}

### END