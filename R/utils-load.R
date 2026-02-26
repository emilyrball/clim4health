#' .paste_outer
#'
#' Paste two character vectors in an outer way.
#'
#' @param path A character vector containing the first elements to be pasted.
#' @param ext A character vector containing the second elements to be pasted.
#'
#' @returns A character vector containing the pasted elements.
#' @noRd
.paste_outer <- function(path, ext) {
  path <- outer(path, ext, FUN = "paste0")
  path <- t(path)
  dim(path) <- NULL
  path
}

#' .get_csv_dims
#'
#' Retrieve values and column index for specified names from CSV headers
#' and data.
#'
#' @param headers A character vector containing the CSV headers.
#' @param data A data frame containing the CSV data.
#' @param names A character vector containing the names to retrieve.
#'
#' @returns A list with two elements: 'val' containing the values from the
#' specified columns, and 'col' containing the column indices.
#' @noRd
.get_csv_dims <- function(headers, data, names) {
  if (any(names %in% headers)) {
    col <- which(headers %in% names)
    val <- data[, col]
  } else {
    col <- NULL
    val <- rep(1, length(data[, 1]))
  }
  list(val = val, col = col)
}

#' .c4h_load_csv
#'
#' Load data from .csv files into an s2dv_cube object.
#'
#' @param file_list list of files or path to directory containing files to be
#' loaded.
#' @param variable name of variable to be loaded.
#' @param month month of the data to be loaded. If "all", all months will be
#' loaded.
#' @param day day of the data to be loaded. If "all", all days will be loaded.
#' @param time time of day of the data to be loaded. If "all", all times will be
#' loaded.
#' @param leadtime_month leadtime months to be loaded. If "all", all leadtime
#' months will be loaded.
#' @param lat_selection minimum and maximum latitudes to be loaded derived from
#' the bounding box. If "all", all latitudes will be loaded.
#' @param lon_selection minimum and maximum longitudes to be loaded derived from
#' the bounding box. If "all", all longitudes will be loaded.
#'
#' @returns an s2dv_cube object.
#' @noRd
.c4h_load_csv <- function(file_list,
                          variable,
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
    found_header <- FALSE
    for (i in 1:10) {
      if (ncol(data.table::fread(file_list, skip = i)) > 1) {
        found_header <- TRUE
        break
      }
    }
    if (!found_header) {
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

  # get name and values of latitude and longitude columns
  lats <- .get_csv_dims(headers, data, c("y", "lat", "latitude"))
  lons <- .get_csv_dims(headers, data, c("x", "lon", "longitude"))

  # If lat_selection or lon_selection is "all", select all coordinates
  if (lat_selection == "all" && !is.null(lats$col)) {
    lat_selection <- c(min(lats$val), max(lats$val))
  }
  if (lon_selection == "all" && !is.null(lons$col)) {
    lon_selection <- c(min(lons$val), max(lons$val))
  }
  # subset to only the requested coordinates (lat_selection and lon_selection)
  if (!is.null(lats$col) && !is.null(lons$col)) {
    lat_lon_selection <- which(lats$val >= min(lat_selection) &
                                 lats$val <= max(lat_selection) &
                                 lons$val >= min(lon_selection) &
                                 lons$val <= max(lon_selection))
    data <- data[lat_lon_selection, ]
    lats$val <- lats$val[lat_lon_selection]
    lons$val <- lons$val[lat_lon_selection]
  }
  # end latitude/longitude management

  vars <- list()
  # get variable
  if (!is.null(variable)) {
    if (any(headers %in% variable)) {
      # if variable name exists in file
      vars$col <- which(headers %in% variable)
    } else {
      # if variable name does not exist
      stop(paste0("Variable name ", variable, " not found. Possible names are:",
                  " ", paste(headers, collapse = ", ")))
    }
  } else {
    # if variable is not specified, print the list of possible columns
    stop(paste0("Please specify the variable name. Possible names are: ",
                paste(headers, collapse = ", ")))
  }
  vars$val <- data[, vars$col]

  # datetime management
  # get time columns
  possible_time_names <- c("year", "month", "week", "day", "date", "time")
  times <- .get_csv_dims(headers, data, possible_time_names)

  dates_all <- lubridate::parse_date_time(
    times$val,
    orders = c("Ymd HMS", "Ymd", "dmY HMS", "dmY", "mdY HMS", "mdY",
               "Ym", "mY")#,
    #tz = "UTC"
  )
  dates <- unique(dates_all)

  # filter dates if year, month, day or time are specified
  if (any(year == "all")) {
    year <- unique(lubridate::year(dates_all))
  }
  if (any(month == "all")) {
    month <- 1:12
  }
  if (any(day == "all")) {
    day <- 1:31
  }
  if (any(time == "all")) {
    time <- 0:23
  }
  start_dates <- .paste_outer(.paste_outer(year, sprintf("-%02d", month)),
                              sprintf("-%02d", day))
  if (any(with(as.POSIXlt(dates), hour != 0 | min != 0 | sec != 0))) {
    start_dates <- .paste_outer(start_dates,
                                sprintf(" %02d:00:00", time))
  }

  start_dates_tmp <-
    lubridate::parse_date_time(start_dates, orders =
                               c("Ymd HMS", "Ymd"))#, tz = "UTC")
  start_dates <- unique(start_dates_tmp[!is.na(start_dates_tmp)])

  if (any(leadtime_month == "all")) {
    timeseries <- TRUE
    leadtime_month <- 1
  } else {
    timeseries <- FALSE
  }
  grid_dates <- expand.grid(sdate = as.POSIXct(start_dates,
                                               #tz = "UTC",
                                               format = "%Y-%m-%d %H:%M:%S"),
                            time = leadtime_month)

  grid_dates$forecast_date <- lubridate::add_with_rollback(
    grid_dates$sdate, months(grid_dates$time - 1)
  )

  # use forecast_date as this contains all possible dates
  start_dates <- unique(sort(c(grid_dates$forecast_date)))

  date_selection <- which(as.Date(dates_all) %in% as.Date(start_dates))
  data <- data[date_selection, ]
  dates_all <- dates_all[date_selection]
  lats$val <- lats$val[date_selection]
  lons$val <- lons$val[date_selection]
  dates <- unique(dates_all)

  dates <- matrix(as.POSIXct(grid_dates$forecast_date,
                             origin = "1970-01-01"),# tz = "UTC"),
                  nrow = length(unique(grid_dates$sdate)),
                  ncol = length(leadtime_month),
                  byrow = FALSE)

  dates <- as.POSIXct(dates, origin = "1970-01-01")#, tz = "UTC")

  names(dim(dates)) <- c("sdate", "time")
  # end datetime management

  # obtain metadata
  lat <- c() # initialise
  lon <- c() # initialise

  if (!is.null(lats$col) && !is.null(lons$col)) {
    coordinates <- unique(paste0(lats$val, "; ", lons$val))
    splits <- strsplit(coordinates, "; ")
    coords_mat <- do.call(rbind, splits)
    lat <- as.numeric(coords_mat[, 1])
    lon <- as.numeric(coords_mat[, 2])
  } else {
    coordinates <- 1
    lat <- 1
    lon <- 1
  }

  # convert table to array of correct dimensions for s2dv_cube
  data_array <- array(NA_real_,
                      dim = c(dataset = 1,
                              var = length(vars$col),
                              sdate = dim(dates)[["sdate"]],
                              time  = dim(dates)[["time"]],
                              ensemble = 1,
                              location = length(coordinates)))

  # reshape data and dates
  for (i in seq_len(nrow(grid_dates))) {
    s <- which(dates[, 1] == grid_dates$sdate[i])
    if (length(s) == 0) {
      next
    }
    t <- as.integer(grid_dates$time[i]) - min(leadtime_month) + 1
    d <- which(as.Date(dates_all) == as.Date(grid_dates$forecast_date[i]))

    for (loc in seq_along(coordinates)) {
      lnx <- which(lons$val == lon[loc])
      llx <- which(lats$val == lat[loc])
      idxs <- Reduce(intersect, list(lnx, llx, d))
      if (length(idxs) > 0) {
        for (v in seq_along(vars$col)) {
          data_array[, v, s, t, , loc] <- data[idxs, vars$col[v]]
        }
      } else {
        data_array[, , s, t, , loc] <- rep(NA, length(vars$col))
      }
    }
  }

  coords <- list(dataset = 1,
                 var = variable,
                 sdate = seq(dim(dates)[["sdate"]]),
                 time = seq(dim(dates)[["time"]]),
                 ensemble = 1,
                 location = seq_along(coordinates))
  # case 1: leadtime_month = 1
  if (length(leadtime_month) == 1 && any(leadtime_month == 1) && timeseries) {
    # rename sdate and time dimensions so time is stored in time dimension
    # and sdate is always 1
    data_array <- aperm(data_array, c(1, 2, 4, 3, 5, 6))
    names(dim(data_array)) <- c("dataset", "var", "sdate",
                                "time", "ensemble", "location")
    dates <- t(dates)
    names(dim(dates)) <- c("sdate", "time")
    coords[["sdate"]] <- 1
    coords[["time"]] <- seq_along(unique(grid_dates$sdate))
  }

  # format to s2dv_cube
  data <- CSTools::s2dv_cube(data = data_array,
                             coords = coords,
                             varName = variable,
                             Dates = dates,
                             metadata = list(source_files = file_list,
                                             units = NULL))

  if (!is.null(metadata)) {
    data$attrs$Variable$metadata$header <- metadata
  }

  data$attrs$location <- list(longitude = lon,
                              latitude = lat)
  return(data)
}

#' .c4h_load_nc_stars
#'
#' Load data from a list of netCDF files using the stars package.
#'
#' @param file_list A character vector containing the paths to the files.
#' @param variable A character string specifying the variable to load.
#' @param year A numeric vector specifying the years to load. Default is "all".
#' @param month A numeric vector specifying the month to load. Default is "all".
#' @param day A numeric vector specifying the day to load. Default is "all".
#' @param time A numeric vector specifying the time of day to load. Default is
#' "all".
#' @param leadtime_month A numeric vector specifying the lead time in months.
#' Default is "all".
#' @param lat_selection A numeric vector specifying the latitude selection
#' derived from bbox.
#' @param lon_selection A numeric vector specifying the longitude selection
#' derived from bbox.
#'
#' @returns An s2dv_cube object containing the loaded data.
#' @noRd
.c4h_load_nc_stars <- function(file_list,
                               variable = NULL,
                               year = "all",
                               month = "all",
                               day = "all",
                               time = "all",
                               leadtime_month = "all",
                               lat_selection = "all",
                               lon_selection = "all") {
  # get variable names in files
  file_vars <- lapply(file_list, function(x) {
    names(sf::gdal_read_mdim(x, proxy = TRUE)[[1]])
  })

  file_list_var <- list()
  # for each variable, find the files that contain it
  for (var in variable) {
    file_list_var[[var]] <- file_list[
      vapply(file_vars, function(v) var %in% v, logical(1))
    ]
    names(file_list_var[[var]]) <- NULL
  }
  data_out <- list()
  units_out <- list()

  for (j in seq_along(variable)) {
    # loop through variables
    var <- variable[j]
    file_list <- file_list_var[[var]]
    if (length(file_list) == 0) {
      message(paste0("Requested variable '", var, "' not found in 'path'.",
                     " Skipping."))
      next
    }

    # get individual file dimensions
    x <- stars::read_stars(file_list[1], proxy = TRUE)
    dim_names <- names(stars::st_dimensions(x))
    # classify by time
    if ("valid_time" %in% dim_names) {
      concat_dim <- "valid_time"
    } else if ("forecast_reference_time" %in% dim_names) {
      concat_dim <- "forecast_reference_time"
    } else if ("forecast_period" %in% dim_names) {
      concat_dim <- "forecast_period"
    }

    date_vals <- stars::st_get_dimension_values(x, concat_dim)
    date_vals <- as.POSIXct(date_vals, origin = "1970-01-01",
                            format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

    len_date_day <- length(unique(format(date_vals, "%Y-%m-%d")))
    len_date_month <- length(unique(format(date_vals, "%Y-%m")))
    if (length(unique(date_vals)) > len_date_day) {
      time_freq <- "hourly"
    } else if (len_date_day > len_date_month) {
      time_freq <- "daily"
    } else {
      time_freq <- "monthly"
    }

    ### spatially subset
    lon_vals <- stars::st_get_dimension_values(x, "x")
    lat_vals <- stars::st_get_dimension_values(x, "y")

    if (any(lat_selection == "all")) {
      lat_selection <- lat_vals
    }
    if (any(lon_selection == "all")) {
      lon_selection <- lon_vals
    }
    lon_idx <- which(lon_vals >= min(lon_selection) &
                       lon_vals <= max(lon_selection))
    lat_idx <- which(lat_vals >= min(lat_selection) &
                       lat_vals <= max(lat_selection))

    lon_off  <- min(lon_idx)
    lon_size <- length(lon_idx)
    lat_off  <- min(lat_idx)
    lat_size <- length(lat_idx)

    if (lat_size == length(lat_vals)) {
      lat_off <- NULL
      lat_size <- NULL
    }
    if (lon_size == length(lon_vals)) {
      lon_off <- NULL
      lon_size <- NULL
    }
    ### end spatial subsetting

    # read files with stars, applying spatial subsetting if necessary
    x <- stars::read_stars(file_list,
                           along = concat_dim,
                           RasterIO = list(nXOff = lon_off,
                                           nYOff = lat_off,
                                           nXSize = lon_size,
                                           nYSize = lat_size),
                           curvilinear = FALSE)

    # select only the requested variable
    attr_names <- names(x)
    if (length(attr_names) == 1) {
      attr_var <- attr_names
    } else {
      attr_var <- attr_names[grep(var, attr_names)]
    }
    x <- x[attr_var, drop = FALSE]

    lon_vals <- stars::st_get_dimension_values(x, "x")
    lat_vals <- stars::st_get_dimension_values(x, "y")
    date_vals <- stars::st_get_dimension_values(x, concat_dim)
    date_vals <- as.POSIXct(date_vals, origin = "1970-01-01",
                            format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

    units <- .extract_units(x, attr_var)

    if (any(year == "all")) {
      year <- unique(lubridate::year(date_vals))
    }
    if (any(month == "all")) {
      month <- 1:12
    }
    if (any(day == "all")) {
      day <- 1:31
    }
    if (any(time == "all")) {
      time <- 0:23
    }

    if ("valid_time" %in% dim_names) {
      # case: observations
      if (time_freq == "hourly") {
        data <- .load_hourly_observations(x = x,
                                          leadtime_month = leadtime_month,
                                          date_vals = date_vals,
                                          year = year,
                                          month = month,
                                          day = day,
                                          time = time,
                                          file_list = file_list,
                                          var = var,
                                          units = units)
      } else if (time_freq == "daily") {
        data <- .load_daily_observations(x = x,
                                         leadtime_month = leadtime_month,
                                         date_vals = date_vals,
                                         year = year,
                                         month = month,
                                         day = day,
                                         file_list = file_list,
                                         var = var,
                                         units = units)
      } else if (time_freq == "monthly") {
        data <- .load_monthly_observations(x = x,
                                           leadtime_month = leadtime_month,
                                           date_vals = date_vals,
                                           year = year,
                                           month = month,
                                           day = day,
                                           file_list = file_list,
                                           var = var,
                                           units = units)
      }
    } else if ("forecast_reference_time" %in% dim_names) {
      # case: forecasts
      data <- .load_forecast(x = x,
                             leadtime_month = leadtime_month,
                             date_vals = date_vals,
                             year = year,
                             month = month,
                             day = day,
                             file_list = file_list,
                             var = var,
                             units = units)
    }
    # assign data for each variable
    data_out[[length(data_out) + 1]] <- data
    units_out[[length(units_out) + 1]] <- units
  }

  if (length(data_out) == 1) {
    data_out <- data_out[[1]]
  } else if (length(data_out) == 0) {
    stop("No valid data found.")
  } else {
    dates <- data_out[[1]]$attrs$Dates
    data_out <- CSTools::CST_BindDim(data_out, along = "var",
                                     var_dim = "var",
                                     dat_dim = "dataset")

    data_out$attrs$Dates <- dates
  }
  data_out$attrs$source_files <- unlist(file_list_var)
  data_out$attrs$source_files <- gsub("NETCDF:\"(.*)\"", "\\1",
                                      data_out$attrs$source_files)
  #data_out$attrs$source_files <- gsub(":(.*)", "\\1",
  #                                    data_out$attrs$source_files)
  names(data_out$attrs$source_files) <- NULL
  data_out$attrs$Variable$metadata$units <- unlist(units_out)
  names(data_out$attrs$Variable$metadata$units) <- data_out$coords$var
  return(data_out)
}

#' Slice stars object using a dimension name and a slice index.
#'
#' @param x stars object
#' @param dimname dimension name to be sliced
#' @param slice index of the slice
#'
#' @returns the filtered stars object
#' @noRd
.slice_stars_dimname <- function(x, dimname, slice) {

  # Get dims and the position of dimname
  dims <- names(stars::st_dimensions(x))
  pos <- match(dimname, dims)

  # Insert slicing
  idx_strs <- rep("", length(dims))
  idx_strs[pos] <- paste0("c(", paste(slice, collapse = ","), ")")

  # build call text, making sure drop is placed inside [ ]
  call_text <- paste0("x[,", paste(idx_strs, collapse = ","),
                      ", drop = FALSE", "]")

  # evaluate the call in an environment where x is present
  eval(parse(text = call_text), envir = list2env(list(x = x)))
}

#' Load sub-daily observations in s2dv_cube format
#'
#' @param x stars object
#' @param leadtime_month A numeric vector specifying the lead time in months.
#' @param date_vals A vector of date values in POSIXct format.
#' @param year A numeric vector specifying the years to load.
#' @param month A numeric vector specifying the month to load.
#' @param day A numeric vector specifying the day to load.
#' @param time A numeric vector specifying the time of day to load.
#' @param file_list A character vector containing the paths to the files.
#' @param var A character string specifying the variable to load.
#' @param units A character string specifying the units of the variable.
#'
#' @returns the s2dv_cube object
#' @noRd
.load_hourly_observations <- function(x, leadtime_month, date_vals,
                                      year, month, day, time,
                                      file_list, var, units) {
  if (any(leadtime_month == "all")) {
    timeseries <- TRUE
  } else {
    timeseries <- FALSE
  }
  one_dim <- any(leadtime_month == "all") || identical(leadtime_month, 1)

  start_dates <- .paste_outer(year, sprintf("-%02d", month))
  if (one_dim) {
    # case: dates will be stored in one dimension
    start_dates <- .paste_outer(start_dates,
                                sprintf("-%02d", day))

    start_dates <- .paste_outer(start_dates,
                                sprintf(" %02d:00:00", time))

    leadtime_month <- 1
    grid_dates <- expand.grid(sdate =
                                as.POSIXct(start_dates, tz = "UTC",
                                           format = "%Y-%m-%d %H:%M:%S"),
                              time = 1)

    grid_dates$forecast_date <- lubridate::add_with_rollback(
      grid_dates$sdate, months(grid_dates$time - 1)
    )
  } else {
    start_dates_sd <- .paste_outer(start_dates,
                                   sprintf("-%02d", day[1]))

    start_dates_sd <- .paste_outer(start_dates_sd,
                                   sprintf(" %02d:00:00", time[1]))
    start_dates_dh <- .paste_outer(sprintf("-%02d", day),
                                   sprintf(" %02d:00:00", time))

    grid_dates <- expand.grid(dayhour = start_dates_dh,
                              time = leadtime_month,
                              sdate =
                                as.POSIXct(start_dates_sd, tz = "UTC",
                                           format = "%Y-%m-%d %H:%M:%S"))

    grid_dates <- rev(grid_dates)
    grid_dates$forecast_date <- lubridate::add_with_rollback(
      grid_dates$sdate, months(grid_dates$time - 1)
    )
    grid_dates$forecast_date <-
      paste0(lubridate::year(grid_dates$forecast_date),
             sprintf("-%02d", lubridate::month(grid_dates$forecast_date)),
             grid_dates$dayhour)
    grid_dates$forecast_date <-
      as.POSIXct(grid_dates$forecast_date, tz = "UTC",
                 format = "%Y-%m-%d %H:%M:%S")
  }

  start_dates <- unique(sort(c(grid_dates$forecast_date)))
  if (!(any(as.POSIXct(date_vals, origin = "1970-01-01", tz = "UTC") %in%
              start_dates))) {
    stop(paste0("No matching dates found for the specified year, month,",
                " day, and time. Dates found in data lie in the range: ",
                paste0(range(date_vals), collapse = " - "),
                ". Please adjust the date selection accordingly."))
  }
  if (!(all(as.POSIXct(date_vals, origin = "1970-01-01", tz = "UTC") %in%
              start_dates))) {
    filter_indx <- as.POSIXct(stars::st_get_dimension_values(x, "valid_time"),
                              origin = "1970-01-01",
                              tz = "UTC") %in% start_dates
    filter_indx <- which(filter_indx)
    x <-  .slice_stars_dimname(x, "valid_time", filter_indx)
  }

  dims <- stars::st_dimensions(x)

  date_vals <- as.POSIXct(stars::st_get_dimension_values(x, "valid_time"),
                          origin = "1970-01-01", tz = "UTC")

  if (one_dim) {
    grid_dates <- grid_dates[grid_dates$forecast_date %in% date_vals, ]
  }
  coords <- list()
  for (dim in names(dims)) {
    coords[[dim]] <- as.vector(stars::st_get_dimension_values(x, dim))
  }
  coords[["sdate"]] <- seq_along(unique(grid_dates$sdate))
  coords[["ensemble"]] <- 1
  names(coords)[which(names(coords) == "x")] <- "longitude"
  names(coords)[which(names(coords) == "y")] <- "latitude"
  names(coords)[which(names(coords) == "valid_time")] <- "dates"

  coords[["dates"]] <- as.POSIXct(coords[["dates"]],
                                  origin = "1970-01-01",
                                  tz = "UTC")

  if (one_dim) {
    dates <- matrix(as.POSIXct(grid_dates$forecast_date,
                               origin = "1970-01-01", tz = "UTC"),
                    nrow = length(unique(grid_dates$sdate)),
                    ncol = length(leadtime_month),
                    byrow = FALSE)
  } else {
    dates <- matrix(as.POSIXct(grid_dates$forecast_date,
                               origin = "1970-01-01", tz = "UTC"),
                    nrow = length(unique(grid_dates$sdate)),
                    byrow = TRUE)
    dates <- dates[, colSums(is.na(dates)) != nrow(dates)]
  }

  dates <- as.POSIXct(dates, origin = "1970-01-01", tz = "UTC")

  if (is.null(dim(dates))) {
    dim(dates) <- c(sdate = 1, time = length(dates))
  }
  names(dim(dates)) <- c("sdate", "time")

  if (one_dim) {
    coords[["time"]] <- seq_along(leadtime_month)
  } else {
    coords[["time"]] <- seq_len(ncol(dates))
  }
  data_array <- array(NA_real_,
                      dim = c(dataset = 1,
                              var = 1,
                              sdate = dim(dates)[["sdate"]],
                              time  = dim(dates)[["time"]],
                              ensemble = length(coords[["ensemble"]]),
                              latitude = length(coords[["latitude"]]),
                              longitude = length(coords[["longitude"]])))
  date_index <- match(grid_dates$forecast_date, date_vals)
  dims <- names(dim(x[[1]]))
  reordered <- match(dims, c("y", "x", "valid_time"))
  x[[1]] <- aperm(x[[1]], reordered)
  # fill by indexing along valid_time
  for (i in seq_len(nrow(grid_dates))) {
    s <- which(dates[, 1] == grid_dates$sdate[i])
    t <- grid_dates$time[i]
    v <- date_index[i]  # index in arr_x valid_time
    data_array[1, 1, s, t, 1, , ] <- x[[1]][, , v]
  }

  coords <- coords[c("dataset", "var", "sdate", "time",
                     "ensemble", "latitude", "longitude")]

  # case 1: leadtime_month = 1
  if (length(leadtime_month) == 1 && any(leadtime_month == 1) && timeseries) {
    # rename sdate and time dimensions so time is stored in time dimension
    # and sdate is always 1
    data_array <- aperm(data_array, c(1, 2, 4, 3, 5, 6, 7))
    names(dim(data_array)) <- c("dataset", "var", "sdate",
                                "time", "ensemble", "latitude",
                                "longitude")
    dates <- t(dates)
    names(dim(dates)) <- c("sdate", "time")
    coords[["sdate"]] <- 1
    coords[["time"]] <- seq_along(unique(grid_dates$sdate))
  }

  data <- CSTools::s2dv_cube(data = data_array,
                             coords =
                               c(list(dataset = 1, var = var), coords),
                             varName = var,
                             Dates = dates,
                             metadata =
                               list(variable = var,
                                    units = units,
                                    source_files = file_list))
  data
}

#' Load daily observations in s2dv_cube format
#'
#' @param x stars object
#' @param leadtime_month A numeric vector specifying the lead time in months.
#' @param date_vals A vector of date values in POSIXct format.
#' @param year A numeric vector specifying the years to load.
#' @param month A numeric vector specifying the month to load.
#' @param day A numeric vector specifying the day to load.
#' @param file_list A character vector containing the paths to the files.
#' @param var A character string specifying the variable to load.
#' @param units A character string specifying the units of the variable.
#'
#' @returns the s2dv_cube object
#' @noRd
.load_daily_observations <- function(x, leadtime_month,
                                     date_vals,
                                     year, month, day,
                                     file_list, var, units) {
  if (any(leadtime_month == "all")) {
    timeseries <- TRUE
  } else {
    timeseries <- FALSE
  }
  one_dim <- any(leadtime_month == "all") || identical(leadtime_month, 1)

  if (one_dim) {
    # case: only one leadtime month
    start_dates <- .paste_outer(.paste_outer(year,
                                             sprintf("-%02d", month)),
                                sprintf("-%02d", day))
    leadtime_month <- 1
    grid_dates <- expand.grid(sdate = as.POSIXct(start_dates, tz = "UTC",
                                                 format = "%Y-%m-%d",
                                                 origin = "1970-01-01"),
                              time = leadtime_month)
    grid_dates$forecast_date <- lubridate::add_with_rollback(
      grid_dates$sdate, months(grid_dates$time - 1)
    )
  } else {
    # case: multiple leadtime_month
    start_dates_sd <- .paste_outer(.paste_outer(year,
                                                sprintf("-%02d", month)),
                                   sprintf("-%02d", day[1]))

    start_dates_dh <- sprintf("-%02d", day)

    grid_dates <- expand.grid(day = start_dates_dh,
                              time = leadtime_month,
                              sdate =
                                as.POSIXct(start_dates_sd, tz = "UTC",
                                           format = "%Y-%m-%d",
                                           origin = "1970-01-01"))

    grid_dates <- rev(grid_dates)
    grid_dates$forecast_date <- lubridate::add_with_rollback(
      grid_dates$sdate, months(grid_dates$time - 1)
    )
    grid_dates$forecast_date <-
      paste0(lubridate::year(grid_dates$forecast_date),
             sprintf("-%02d", lubridate::month(grid_dates$forecast_date)),
             grid_dates$day)
    grid_dates$forecast_date <- as.POSIXct(grid_dates$forecast_date, tz = "UTC",
                                           format = "%Y-%m-%d",
                                           origin = "1970-01-01")
  }

  start_dates <- unique(sort(c(grid_dates$forecast_date)))
  if (!(any(as.POSIXct(date_vals, origin = "1970-01-01",
                       tz = "UTC", format = "%Y-%m-%d") %in% start_dates))) {
    stop(paste0("No matching dates found for the specified year, month,",
                " day, and time. Dates found in data lie in the range: ",
                paste0(range(as.POSIXct(date_vals, origin = "1970-01-01",
                                        tz = "UTC", format = "%Y-%m-%d")),
                       collapse = " - "),
                ". Please adjust the date selection accordingly."))
  }
  if (!(all(as.POSIXct(date_vals, origin = "1970-01-01",
                       tz = "UTC", format = "%Y-%m-%d") %in% start_dates))) {
    filter_indx <- as.POSIXct(stars::st_get_dimension_values(x, "valid_time"),
                              origin = "1970-01-01",
                              tz = "UTC", format = "%Y-%m-%d") %in% start_dates
    filter_indx <- which(filter_indx)
    x <-  .slice_stars_dimname(x, "valid_time", filter_indx)
  }

  dims <- stars::st_dimensions(x)
  date_vals <- as.POSIXct(stars::st_get_dimension_values(x, "valid_time"),
                          origin = "1970-01-01", tz = "UTC")
  if (one_dim) {
    grid_dates <- grid_dates[grid_dates$forecast_date %in% date_vals, ]
  }

  coords <- list()
  for (dim in names(dims)) {
    coords[[dim]] <- as.vector(stars::st_get_dimension_values(x, dim))
  }
  coords[["sdate"]] <- seq_along(unique(grid_dates$sdate))
  coords[["ensemble"]] <- 1
  names(coords)[which(names(coords) == "x")] <- "longitude"
  names(coords)[which(names(coords) == "y")] <- "latitude"
  names(coords)[which(names(coords) == "valid_time")] <- "time"

  coords[["time"]] <- as.POSIXct(coords[["time"]], origin = "1970-01-01",
                                 tz = "UTC")
  if (one_dim) {
    dates <- matrix(as.POSIXct(grid_dates$forecast_date,
                               origin = "1970-01-01", tz = "UTC"),
                    nrow = length(unique(grid_dates$sdate)),
                    ncol = length(leadtime_month),
                    byrow = FALSE)
  } else {
    dates <- matrix(as.POSIXct(grid_dates$forecast_date,
                               origin = "1970-01-01", tz = "UTC"),
                    nrow = length(unique(grid_dates$sdate)),
                    byrow = TRUE)
    dates <- dates[, colSums(is.na(dates)) != nrow(dates)]
  }

  dates <- as.POSIXct(dates, origin = "1970-01-01", tz = "UTC")

  if (is.null(dim(dates))) {
    dim(dates) <- c(sdate = 1, time = length(dates))
  }
  names(dim(dates)) <- c("sdate", "time")
  if (one_dim) {
    coords[["time"]] <- seq_along(leadtime_month)
  } else {
    coords[["time"]] <- seq_len(ncol(dates))
  }
  data_array <- array(NA_real_,
                      dim = c(dataset = 1,
                              var = 1,
                              sdate = dim(dates)[["sdate"]],
                              time  = dim(dates)[["time"]],
                              ensemble = length(coords[["ensemble"]]),
                              latitude = length(coords[["latitude"]]),
                              longitude = length(coords[["longitude"]])))
  date_index <- match(grid_dates$forecast_date, date_vals)
  dims <- names(dim(x[[1]]))
  reordered <- match(dims, c("y", "x", "valid_time"))
  x[[1]] <- aperm(x[[1]], reordered)
  # fill by indexing along valid_time
  for (i in seq_len(nrow(grid_dates))) {
    s <- which(dates[, 1] == grid_dates$sdate[i])
    t <- grid_dates$time[i]
    v <- date_index[i]  # index in arr_x valid_time
    data_array[1, 1, s, t, 1, , ] <- x[[1]][, , v]
  }

  coords <- coords[c("dataset", "var", "sdate", "time",
                     "ensemble", "latitude", "longitude")]

  # case 1: leadtime_month = 1
  if (length(leadtime_month) == 1 && any(leadtime_month == 1) && timeseries) {
    # rename sdate and time dimensions so time is stored in time dimension
    # and sdate is always 1
    data_array <- aperm(data_array, c(1, 2, 4, 3, 5, 6, 7))
    names(dim(data_array)) <- c("dataset", "var", "sdate",
                                "time", "ensemble", "latitude",
                                "longitude")
    dates <- t(dates)
    names(dim(dates)) <- c("sdate", "time")
    coords[["sdate"]] <- 1
    coords[["time"]] <- seq_along(unique(grid_dates$sdate))
  }

  data <- CSTools::s2dv_cube(data = data_array,
                             coords =
                               c(list(dataset = 1, var = var), coords),
                             varName = var,
                             Dates = dates,
                             metadata =
                               list(variable = var,
                                    units = units,
                                    source_files = file_list))
  data
}

#' Load monthly observations in s2dv_cube format
#'
#' @param x stars object
#' @param leadtime_month A numeric vector specifying the lead time in months.
#' @param date_vals A vector of date values in POSIXct format.
#' @param year A numeric vector specifying the years to load.
#' @param month A numeric vector specifying the month to load.
#' @param day A numeric vector specifying the day to load.
#' @param file_list A character vector containing the paths to the files.
#' @param var A character string specifying the variable to load.
#' @param units A character string specifying the units of the variable.
#'
#' @returns the s2dv_cube object
#' @noRd
.load_monthly_observations <- function(x,
                                       leadtime_month,
                                       date_vals,
                                       year, month, day,
                                       file_list, var, units) {
  if (any(leadtime_month == "all")) {
    timeseries <- TRUE
  } else {
    timeseries <- FALSE
  }
  one_dim <- any(leadtime_month == "all") || identical(leadtime_month, 1)

  if (one_dim) {
    # case: only one leadtime month
    start_dates <- .paste_outer(.paste_outer(year,
                                             sprintf("-%02d", month)),
                                sprintf("-%02d", day))
    leadtime_month <- 1
    grid_dates <- expand.grid(sdate = as.POSIXct(start_dates, tz = "UTC",
                                                 format = "%Y-%m-%d",
                                                 origin = "1970-01-01"),
                              time = leadtime_month)
    grid_dates$forecast_date <- lubridate::add_with_rollback(
      grid_dates$sdate, months(grid_dates$time - 1)
    )
  } else {
    # case: multiple leadtime_month
    start_dates_sd <- .paste_outer(.paste_outer(year,
                                                sprintf("-%02d", month)),
                                   sprintf("-%02d", day[1]))
    start_dates_dh <- sprintf("-%02d", day)
    grid_dates <- expand.grid(day = start_dates_dh,
                              time = leadtime_month,
                              sdate =
                                as.POSIXct(start_dates_sd, tz = "UTC",
                                           format = "%Y-%m-%d",
                                           origin = "1970-01-01"))
    grid_dates <- rev(grid_dates)
    grid_dates$forecast_date <- lubridate::add_with_rollback(
      grid_dates$sdate, months(grid_dates$time - 1)
    )
    grid_dates$forecast_date <-
      paste0(lubridate::year(grid_dates$forecast_date),
             sprintf("-%02d", lubridate::month(grid_dates$forecast_date)),
             grid_dates$day)
    grid_dates$forecast_date <-
      as.POSIXct(grid_dates$forecast_date, tz = "UTC",
                 format = "%Y-%m-%d", origin = "1970-01-01")
  }
  start_dates <- unique(sort(c(grid_dates$forecast_date)))
  if (!(any(as.POSIXct(date_vals, tz = "UTC", origin = "1970-01-01",
                       format = "%Y-%m-%d") %in% start_dates))) {
    stop(paste0("No matching dates found for the specified year, month,",
                " day, and time. Dates found in data lie in the range: ",
                paste0(range(as.POSIXct(date_vals, tz = "UTC", 
                                        origin = "1970-01-01",
                                        format = "%Y-%m-%d")),
                       collapse = " - "),
                ". Please adjust the date selection accordingly."))
  }
  if (!(all(as.POSIXct(date_vals, tz = "UTC", origin = "1970-01-01",
                       format = "%Y-%m-%d") %in% start_dates))) {
    filter_indx <- as.POSIXct(stars::st_get_dimension_values(x, "valid_time"),
                              origin = "1970-01-01", tz = "UTC",
                              format = "%Y-%m-%d") %in% start_dates
    filter_indx <- which(filter_indx)
    x <-  .slice_stars_dimname(x, "valid_time", filter_indx)
  }
  dims <- stars::st_dimensions(x)
  date_vals <- as.POSIXct(stars::st_get_dimension_values(x, "valid_time"),
                          origin = "1970-01-01", tz = "UTC",
                          format = "%Y-%m-%d")
  if (one_dim) {
    grid_dates <- grid_dates[grid_dates$forecast_date %in% date_vals, ]
  }
  sdate_date <- as.POSIXct(grid_dates$sdate, origin = "1970-01-01", tz = "UTC",
                           format = "%Y-%m-%d")
  fdate_date <- as.POSIXct(grid_dates$forecast_date, origin = "1970-01-01",
                           tz = "UTC", format = "%Y-%m-%d")
  grid_dates <- grid_dates[sdate_date %in% date_vals &
                             fdate_date %in% date_vals, ]
  coords <- list()
  for (dim in names(dims)) {
    coords[[dim]] <- as.vector(stars::st_get_dimension_values(x, dim))
  }
  coords[["sdate"]] <- seq_along(unique(grid_dates$sdate))
  coords[["ensemble"]] <- 1
  names(coords)[which(names(coords) == "x")] <- "longitude"
  names(coords)[which(names(coords) == "y")] <- "latitude"
  names(coords)[which(names(coords) == "valid_time")] <- "time"
  coords[["time"]] <- as.POSIXct(coords[["time"]], origin = "1970-01-01",
                                 tz = "UTC", format = "%Y-%m-%d")
  if (one_dim) {
    dates <- matrix(as.POSIXct(grid_dates$forecast_date, format = "%Y-%m-%d",
                               origin = "1970-01-01", tz = "UTC"),
                    nrow = length(unique(grid_dates$sdate)),
                    ncol = length(leadtime_month),
                    byrow = FALSE)
  } else {
    dates <- matrix(as.POSIXct(grid_dates$forecast_date, format = "%Y-%m-%d",
                               origin = "1970-01-01", tz = "UTC"),
                    nrow = length(unique(grid_dates$sdate)),
                    byrow = TRUE)
    dates <- dates[, colSums(is.na(dates)) != nrow(dates)]
  }
  dates <- as.POSIXct(dates, origin = "1970-01-01",
                      tz = "UTC", format = "%Y-%m-%d")
  if (is.null(dim(dates))) {
    dim(dates) <- c(sdate = 1, time = length(dates))
  }
  names(dim(dates)) <- c("sdate", "time")
  if (one_dim) {
    coords[["time"]] <- seq_along(leadtime_month)
  } else {
    coords[["time"]] <- seq_len(ncol(dates))
  }
  data_array <- array(NA_real_,
                      dim = c(dataset = 1,
                              var = 1,
                              sdate = dim(dates)[["sdate"]],
                              time  = dim(dates)[["time"]],
                              ensemble = length(coords[["ensemble"]]),
                              latitude = length(coords[["latitude"]]),
                              longitude = length(coords[["longitude"]])))
  date_index <- match(grid_dates$forecast_date, date_vals)
  dims <- names(dim(x[[1]]))
  reordered <- match(dims, c("y", "x", "valid_time"))
  x[[1]] <- aperm(x[[1]], reordered)
  # fill by indexing along valid_time
  for (i in seq_len(nrow(grid_dates))) {
    s <- which(dates[, 1] == grid_dates$sdate[i])
    t <- grid_dates$time[i]
    v <- date_index[i]  # index in arr_x valid_time
    data_array[1, 1, s, t, 1, , ] <- x[[1]][, , v]
  }
  coords <- coords[c("dataset", "var", "sdate", "time",
                     "ensemble", "latitude", "longitude")]
  # case 1: leadtime_month = 1
  if (length(leadtime_month) == 1 && any(leadtime_month == 1) && timeseries) {
    # rename sdate and time dimensions so time is stored in time dimension
    # and sdate is always 1
    data_array <- aperm(data_array, c(1, 2, 4, 3, 5, 6, 7))
    names(dim(data_array)) <- c("dataset", "var", "sdate",
                                "time", "ensemble", "latitude",
                                "longitude")
    dates <- t(dates)
    names(dim(dates)) <- c("sdate", "time")
    coords[["sdate"]] <- 1
    coords[["time"]] <- seq_along(unique(grid_dates$sdate))
  }
  data <- CSTools::s2dv_cube(data = data_array,
                             coords =
                               c(list(dataset = 1, var = var), coords),
                             varName = var,
                             Dates = dates,
                             metadata =
                               list(variable = var,
                                    units = units,
                                    source_files = file_list))
  data
}

#' Load seasonal forecast in s2dv_cube format
#'
#' @param x stars object
#' @param leadtime_month A numeric vector specifying the lead time in months.
#' @param date_vals A vector of date values in POSIXct format.
#' @param year A numeric vector specifying the years to load.
#' @param month A numeric vector specifying the month to load.
#' @param day A numeric vector specifying the day to load.
#' @param file_list A character vector containing the paths to the files.
#' @param var A character string specifying the variable to load.
#' @param units A character string specifying the units of the variable.
#'
#' @returns the s2dv_cube object
#' @noRd
.load_forecast <- function(x,
                           leadtime_month,
                           date_vals,
                           year, month, day,
                           file_list, var, units) {
  if (!any(leadtime_month == "all")) {
    filter_indx <-
      stars::st_get_dimension_values(x, "forecastMonth") %in% leadtime_month
    filter_indx <- which(filter_indx)
    x <-  .slice_stars_dimname(x, "forecastMonth", filter_indx)
  }
  start_dates <- .paste_outer(.paste_outer(year, sprintf("-%02d", month)),
                              sprintf("-%02d", day))
  start_dates <- as.POSIXct(start_dates, tz = "UTC", format = "%Y-%m-%d")

  if (!(all(date_vals %in% start_dates))) {
    filter_indx <-
      as.POSIXct(stars::st_get_dimension_values(x,
                                                "forecast_reference_time"),
                 origin = "1970-01-01", tz = "UTC") %in% start_dates
    filter_indx <- which(filter_indx)
    x <-  .slice_stars_dimname(x, "forecast_reference_time", filter_indx)
  }
  dims <- stars::st_dimensions(x)

  coords <-
    lapply(names(dims),
           function(dim) as.vector(stars::st_get_dimension_values(x, dim)))
  names(coords) <- names(dims)  # set names explicitly

  # Rename coordinates
  coord_names <- names(coords)
  old_names <- c("x", "y", "forecastMonth", "forecast_reference_time", "number")
  new_names <- c("longitude", "latitude", "time", "sdate", "ensemble")
  idx <- match(coord_names, old_names)
  replace_idx <- which(!is.na(idx))
  coord_names[replace_idx] <- new_names[idx[replace_idx]]
  names(coords) <- coord_names

  coords[["sdate"]] <- as.POSIXct(coords[["sdate"]], origin = "1970-01-01",
                                  tz = "UTC")
  dates <- outer(
    coords[["sdate"]],
    coords[["time"]],
    FUN = function(sd, t) lubridate::add_with_rollback(sd, months(t - 1))
  )
  names(dim(dates)) <- c("sdate", "time")

  data_array <- array(NA_real_,
                      dim = c(dataset = 1,
                              var = 1,
                              sdate = dim(dates)[["sdate"]],
                              time  = dim(dates)[["time"]],
                              ensemble = length(coords[["ensemble"]]),
                              latitude = length(coords[["latitude"]]),
                              longitude = length(coords[["longitude"]])))

  reorder_dims <- c("sdate", "time", "ensemble", "latitude", "longitude")
  reorder_dims <- match(reorder_dims, names(coords))
  data_array[1, 1, , , , , ] <- aperm(x[[1]], reorder_dims)

  coords <- coords[c("sdate", "time",
                     "ensemble", "latitude", "longitude")]

  coords[["sdate"]] <- seq_along(coords[["sdate"]])

  data <- CSTools::s2dv_cube(data = data_array,
                             coords =
                               c(list(dataset = 1, var = var), coords),
                             varName = var,
                             Dates = dates,
                             metadata =
                               list(variable = var,
                                    units = units,
                                    source_files = file_list))
  data
}

#' .extract_units
#'
#' Extract the units of a variable from a stars object
#'
#' @param x stars object
#' @param attr_var a string indicating the variable name
#'
#' @returns a character string containing the units of the variable.
#' @noRd
.extract_units <- function(x, attr_var) {
  numerator <- units(x[[attr_var]])$numerator
  denominator <- units(x[[attr_var]])$denominator

  if (length(denominator) > 0) {
    units_str <- paste0(numerator, "/", denominator)
  } else {
    units_str <- numerator
  }
  units_str
}

#' .get_variables_nc
#'
#' Extract the variable names from a list of NetCDF files
#'
#' @param file_list A vector of file paths to NetCDF files
#' @param variable A vector of variable names to extract.
#'
#' @returns NULL. This function raises an error via \code{stop()} when
#' \code{variable} is \code{NULL}, or prints an informational message via
#' \code{message()} otherwise.
#' @noRd
.get_variables_nc <- function(file_list, variable) {
  file_vars <- lapply(file_list, function(x) {
    names(sf::gdal_read_mdim(x, proxy = TRUE)[[1]])
  })
  file_vars <- unlist(file_vars)
  all_vars <- unique(gsub(".*:", "", file_vars))

  if (is.null(variable)) {
    stop(paste0("Please specify the variable name. Variables found ",
                "in 'path': '", paste(all_vars, collapse = "', '"), "'."))
  } else {
    message(paste0("Variables found in 'path' are: '",
                   paste(all_vars, collapse = "', '"), "'. Requested ",
                   "variables are: '", paste(variable, collapse = "', '"),
                   "'."))
  }
  return(NULL)
}

#' .subset_file_list
#'
#' Subset list of files based on presence of name in file
#'
#' @param file_list A vector of file paths to NetCDF files
#' @param in_var A vector of variable names or other strings to extract.
#'
#' @returns file_list, subsetted to only include files that contain any of
#' the strings in in_var.
#' @noRd
.subset_file_list <- function(file_list, in_var) {
  if (!identical(in_var, "all")) {
    matches_var <- vapply(
      file_list,
      function(f) any(vapply(in_var, grepl, logical(1), f)),
      logical(1)
    )
    if (any(matches_var)) {
      file_list <- file_list[matches_var]
    }
  }
  return(file_list)
}