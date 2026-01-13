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
  return(path)
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
  return(list(val = val, col = col))
}

#' .c4h_load_csv
#'
#' @param file_list list of files or path to directory containing files to be
#' loaded.
#' @param variable name of variable to be loaded. If NULL, all variables will be
#' loaded.
#' @param year year of the data to be loaded. If "all", all years will be
#' loaded.
#' @param month month of the data to be loaded. If "all", all months will be
#' loaded.
#' @param day day of the data to be loaded. If "all", all days will be
#' loaded.
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

#' .load_csv_tolimavalle
#'
#' Load data from Tolima-Valle .csv files
#'
#' @param data A data frame containing the CSV data.
#' @param headers A character vector containing the CSV headers.
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
.load_csv_tolimavalle <- function(data, headers,
                                  variable = NULL,
                                  year = "all",
                                  month = "all",
                                  day = "all",
                                  time = "all",
                                  leadtime_month = "all",
                                  lat_selection = "all",
                                  lon_selection = "all") {

  # Select all variables if not specified
  if (is.null(var)) {
    var <- unique(data[["tag"]])
  }
  # If lat_selection or lon_selection is "all", select all coordinates
  if (lat_selection == "all") {
    lat_selection <- c(min(data[["latitude"]]),
                       max(data[["latitude"]]))
  }
  if (lon_selection == "all") {
    lon_selection <- c(min(data[["longitude"]]),
                       max(data[["longitude"]]))
  }
  if (!is.null(dates)) {
    # if dates is provided, filter data
    date_selection <-
      paste0(lubridate::year(data[["date"]]),
             sprintf("%02d", lubridate::month(data[["date"]]))) %in% dates
  } else {
    date_selection <- rep(TRUE, nrow(data))
  }

  # filter data
  data <- data[data[["tag"]] %in% var &
      data[["latitude"]] >= min(lat_selection) &
      data[["latitude"]] <= max(lat_selection) &
      data[["longitude"]] >= min(lon_selection) &
      data[["longitude"]] <= max(lon_selection) &
      date_selection,
  ]

  # get datetime object
  data[["time"]] <- as.POSIXct(paste(data[["date"]], data[["hour"]]),
                               tz = "COT")
  data <- data[, !names(data) %in% c("date", "hour")]
  locs <- paste(data[["latitude"]], data[["longitude"]], sep = "; ")
  locs <- unique(locs)
  stations <- unique(data[["station"]])

  lat_coord <- sort(unique(data[["latitude"]]), decreasing = TRUE)
  lon_coord <- sort(unique(data[["longitude"]]), decreasing = FALSE)
  time_coord <- sort(unique(data[["time"]]), decreasing = FALSE)
  all_dates <- paste0(lubridate::year(data[["time"]]),
                      sprintf("%02d",
                              lubridate::month(data[["time"]])))

  # add sdate dimension
  if (is.null(dates)) {
    dates <- paste0(lubridate::year(data[["date"]]),
                    sprintf("%02d", lubridate::month(data[["date"]])))
    dim(dates) <- c(sdate = 1, time = length(dates))
  } else if (is.null(dim(dates))) {
    dim(dates) <- c(sdate = 1, time = length(dates))
  }

  data[["var_idx"]] <- match(data[["tag"]], var)
  data[["lat_idx"]] <- match(data[["latitude"]], lat_coord)
  data[["lon_idx"]] <- match(data[["longitude"]], lon_coord)

  dims <- names(dim(dates))
  sdate_dim <- which(dims == "sdate")
  times_tmp <- list()
  dates_tmp <- list()
  time_length <- 0

  for (i in seq(dim(dates)[["sdate"]])) {
    idx <- vector("list", length(dims))
    idx[] <- TRUE
    idx[[sdate_dim]] <- i
    date_tmp <- do.call(`[`, c(list(dates), idx))
    dates_tmp[[i]] <- date_tmp
    times_tmp[[i]] <- data[["time"]][all_dates %in% date_tmp]
    times_tmp[[i]] <- sort(unique(times_tmp[[i]]), decreasing = FALSE)
    time_length <- max(time_length, length(times_tmp[[i]]))
  }

  for (i in seq(dim(dates)[["sdate"]])) {
    data_array <- NULL
    date_tmp <- dates_tmp[[i]]

    data_tmp <- data[all_dates %in% date_tmp, ]

    time_coord <- sort(unique(data_tmp[["time"]]), decreasing = FALSE)

    data_tmp[["time_idx"]] <- match(data_tmp[["time"]], time_coord)

    data_tmp <- data_tmp[, c("var_idx", "lat_idx",
                             "lon_idx", "time_idx", "value")]

    data_array <- array(NA,
                        dim = c(dataset = 1,
                                var = length(var),
                                sdate = 1,
                                time = time_length,
                                ensemble = 1,
                                latitude = length(lat_coord),
                                longitude = length(lon_coord)))

    # fill array
    data_array[1, data_tmp[["var_idx"]], 1, data_tmp[["time_idx"]],
               1, data_tmp[["lat_idx"]],
               data_tmp[["lon_idx"]]] <- data_tmp[["value"]]
    time_coord <- c(time_coord, rep(NA, time_length - length(time_coord)))
    dim(time_coord) <- c(sdate = 1, time = time_length)

    # format to s2dv_cube
    data_tmp <- CSTools::s2dv_cube(data = data_array,
                                   coords = list(dataset = 1,
                                                 var = var,
                                                 sdate = i,
                                                 time = seq(time_length),
                                                 ensemble = 1,
                                                 latitude = lat_coord,
                                                 longitude = lon_coord),
                                   varName = var,
                                   Dates = time_coord,
                                   metadata =
                                     list(variable = var,
                                          lat_selection = lat_selection,
                                          lon_selection = lon_selection,
                                          date = date_tmp,
                                          station_locations = locs,
                                          station = stations))


    dim(data_tmp$attrs$Dates) <- c(sdate = 1,
                                   time = time_length)
    if (i == 1) {
      data_out <- data_tmp
    } else {
      data_out <- CSTools::CST_BindDim(x = list(data_out, data_tmp),
                                       along = "sdate",
                                       var_dim = "var", dat_dim = "dataset")
    }
  }
  data_out$coords$sdate <- data_out$attrs$Dates[, 1]
  return(data_out)
}

#' .load_csv_dr
#'
#' Load data from Dominican Republic .csv files
#'
#' @param data A data frame containing the CSV data.
#' @param headers A character vector containing the CSV headers.
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
.load_csv_dr <- function(data, headers,
                         variable = NULL,
                         year = "all",
                         month = "all",
                         day = "all",
                         time = "all",
                         leadtime_month = "all",
                         lat_selection = "all",
                         lon_selection = "all") {

  # Clean data
  # D.VIENTO is NA
  data <- data[data[["variable"]] != "D.VIENTO", ]
  data <- data[data[["variable"]] != "D. VIENTO", ]
  # ensure variables have same name
  data[["variable"]][data[["variable"]] %in%
                       c("H. RELATIVA", "H.RELATIVA", "HUMEDAD RELATIVA",
                         "HUMEDADA RELATIVA")] <- "H.RELATIVA"
  data[["variable"]][data[["variable"]] %in%
                       c("LA ROMANA PR. ", "PRECIPITATION")] <- "PRECIPITACION"
  data[["variable"]][data[["variable"]] %in%
                       c("T.MIN", "T. MIN", "TEMP MINIMA",
                         "TEMP. MINIMA", "TEMPERATURA MINIMA")] <- "T.MIN"
  data[["variable"]][data[["variable"]] %in%
                       c("T.MAX", "T. MAX", "TEMP MAXIMA",
                         "TEMP. MAXIMA", "TEMPERATURA MAXIMA")] <- "T.MAX"
  data[["variable"]][data[["variable"]] %in%
                       c("V. VIENTO", "VELOCIDAD VIENTO",
                         "V.VIENTO")] <- "V.VIENTO"

  # rename years with typos
  data[["year"]][data[["year"]] == 22022] <- 2022
  data[["year"]][data[["year"]] == 32023] <- 2023
  data[["year"]][data[["year"]] == 32024] <- 2024

  date_tmp <- paste(data[["year"]],
                    sprintf("%02d", data[["month"]]),
                    sprintf("%02d", data[["day"]]), sep = "-")

  # remove invalid dates
  date_tmp[grep("-02-30", date_tmp)] <- NA
  date_tmp[grep("-02-31", date_tmp)] <- NA
  date_tmp[grep("-04-31", date_tmp)] <- NA
  date_tmp[grep("-06-31", date_tmp)] <- NA
  date_tmp[grep("-09-31", date_tmp)] <- NA
  date_tmp[grep("-11-31", date_tmp)] <- NA

  data[["date"]] <- date_tmp
  data <- data[!is.na(data[["date"]]), ]
  ## finish date cleaning

  if (!is.null(dates)) {
    date_selection <- paste0(data[["year"]],
                             sprintf("%02d", data[["month"]])) %in% dates
  } else {
    date_selection <- !is.na(data[["date"]])
  }

  data[["date_sel"]] <- date_selection
  # set keys
  data.table::setkeyv(data,
                      c("variable", "Estacion", "value", "date", "date_sel"))
  # select all variables if not specified
  if (is.null(var)) {
    var <- unique(data[["variable"]])
  }

  # filter data
  data <- data[data[["variable"]] %in% var & data[["date_sel"]] == TRUE, ]

  data <- data[, !names(data) %in% "date_sel"]
  # get datetime object
  stations <- unique(data[["Estacion"]])

  # add sdate dimension
  if (is.null(dates)) {
    dates <- paste0(data[["year"]], sprintf("%02d", data[["month"]]))
    dim(dates) <- c(sdate = 1, time = length(dates))
  } else if (is.null(dim(dates))) {
    dim(dates) <- c(sdate = 1, time = length(dates))
  }

  data[["var_idx"]]     <- match(data[["variable"]], var)
  data[["station_idx"]] <- match(data[["Estacion"]], stations)

  dims <- names(dim(dates))
  sdate_dim <- which(dims == "sdate")
  times_tmp <- list()
  dates_tmp <- list()
  time_length <- 0
  ym <- paste0(data[["year"]], sprintf("%02d", data[["month"]]))
  data[["ym"]] <- ym

  for (i in seq(dim(dates)[["sdate"]])) {
    idx <- vector("list", length(dims))
    idx[] <- TRUE
    idx[[sdate_dim]] <- i
    date_tmp <- do.call(`[`, c(list(dates), idx))
    dates_tmp[[i]] <- date_tmp
    times_tmp[[i]] <- data[["date"]][data[["ym"]] %in% date_tmp]
    times_tmp[[i]] <- sort(unique(times_tmp[[i]]), decreasing = FALSE)
    time_length <- max(time_length, length(times_tmp[[i]]))
  }

  data_out <- array(NA, dim = c(1, length(var), dim(dates)[["sdate"]],
                                time_length, 1, length(stations)))
  attr_dates <- array(NA, dim = c(dim(dates)[["sdate"]], time_length))

  for (i in seq(dim(dates)[["sdate"]])) {
    date_i <- dates_tmp[[i]]
    times_i <- times_tmp[[i]]
    for (j in seq_along(stations)) {
      station_j <- stations[j]
      for (k in seq_along(var)) {
        data_tmp <- data[data[["variable"]] == var[k] &
                           data[["Estacion"]] == station_j &
                           data[["ym"]] %in% date_i, ]
        idx <- match(times_i, data_tmp[["date"]])
        data_out[1, k, i, , 1, j] <- data_tmp[["value"]][idx]
      }
    }
    attr_dates[i, seq_along(times_i)] <- times_i
  }
  invalid_dates <- c("2014-02-29", "2015-02-29", "2017-02-29",
                     "2018-02-29", "2019-02-29", "2021-02-29",
                     "2022-02-29", "2023-02-29")
  attr_dates[which(attr_dates %in% invalid_dates)] <- NA
  attr_dates <- as.POSIXct(attr_dates, format = "%Y-%m-%d")

  dim(attr_dates) <- c(sdate = dim(dates)[["sdate"]],
                       time = time_length)

  # format to s2dv_cube
  names(dim(data_out)) <- c("dataset", "var", "sdate", "time",
                            "ensemble", "location")
  data_out <- CSTools::s2dv_cube(data = data_out,
                                 coords =
                                   list(dataset = 1,
                                        var = var,
                                        sdate = seq(dim(dates)[["sdate"]]),
                                        time = seq(time_length),
                                        ensemble = 1,
                                        location = seq_along(stations)),
                                 varName = var,
                                 Dates = attr_dates,
                                 metadata =
                                   list(variable = var,
                                        location = stations))

  data_out$coords$sdate <- data_out$attrs$Dates[, 1]
  return(data_out)
}

#' .load_csv_unknown
#'
#' Load data from a more general csv file.
#'
#' @param data A data frame containing the CSV data.
#' @param headers A character vector containing the CSV headers.
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
.load_csv_unknown <- function(data, headers, metadata,
                              variable = NULL,
                              year = "all",
                              month = "all",
                              day = "all",
                              time = "all",
                              leadtime_month = "all",
                              lat_selection = "all",
                              lon_selection = "all") {
  # latitude/longitude management
  # get lats/lons and columns
  lats <- .get_csv_dims(headers, data, c("y", "lat", "latitude"))
  lons <- .get_csv_dims(headers, data, c("x", "lon", "longitude"))

  # If lat_selection or lon_selection is "all", select all coordinates
  if (lat_selection == "all") {
    lat_selection <- c(min(lats$val), max(lats$val))
  }
  if (lon_selection == "all") {
    lon_selection <- c(min(lons$val), max(lons$val))
  }
  # subset to only the requested coordinates (lat_selection and lon_selection)
  if (!(is.null(lats$col)) && !(is.null(lons$col))) {
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
      stop(paste0("variable name ", variable, " not found"))
    }
  } else {
    # if variable is not specified, print the list of possible columns
    stop(paste0("Please specify the variable name. Possible names are: ",
                paste(headers, collapse = ", ")))
  }
  vars$val <- data[, vars$col]

  # datetime management
  # get time columns
  ### test what happens if you have a year and month column
  possible_time_names <- c("year", "month", "week", "day", "date", "time")
  times <- .get_csv_dims(headers, data, possible_time_names)

  dates_all <- lubridate::parse_date_time(
    times$val,
    orders = c("Ymd HMS", "Ymd", "dmY HMS", "dmY", "mdY HMS", "mdY",
               "Ym", "mY"),
    tz = "UTC"
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
                               c("Ymd HMS", "Ymd"), tz = "UTC")
  start_dates <- unique(start_dates_tmp[!is.na(start_dates_tmp)])

  if (any(leadtime_month == "all")) {
    leadtime_month <- 1
  }
  grid_dates <- expand.grid(sdate = as.POSIXct(start_dates,
                                               tz = "UTC",
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
                             origin = "1970-01-01", tz = "UTC"),
                  nrow = length(unique(grid_dates$sdate)),
                  ncol = length(leadtime_month),
                  byrow = FALSE)

  dates <- as.POSIXct(dates, origin = "1970-01-01", tz = "UTC")

  names(dim(dates)) <- c("sdate", "time")
  # end datetime management

  # obtain metadata
  lat <- c() # initialise
  lon <- c() # initialise

  if (!(is.null(lats$col)) && !(is.null(lons$col))) {
    coordinates <- unique(paste0(lats$val, "; ", lons$val))
    for (cc in seq_along(coordinates)) {
      lat <- c(lat, as.numeric(strsplit(coordinates[cc], "; ")[[1]][1]))
      lon <- c(lon, as.numeric(strsplit(coordinates[cc], "; ")[[1]][2]))
    }
  } else {
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
      if (!(length(idxs) == 0)) {
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
  if (length(leadtime_month) == 1 && any(leadtime_month == 1)) {
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
                             Dates = dates)

  if (!is.null(metadata)) {
    data$attrs$Variable$metadata <- metadata
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
  file_vars <- unlist(file_vars)

  file_list_var <- list()
  for (var in variable) {
    file_list_var[[var]] <- file_list[grep(var, file_vars)]
    names(file_list_var[[var]]) <- NULL
  }

  data_out <- list()
  units_out <- list()

  for (j in seq_along(variable)) {
    var <- variable[j]
    file_list <- file_list_var[[var]]
    if (length(file_list) == 0) {
      message(paste0("Requested variable '", var, "' not found in 'path'.",
                     " Skipping."))
      next
    }

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
    date_vals <- as.POSIXct(date_vals, origin = "1970-01-01", tz = "UTC")
    if (length(unique(date_vals)) > length(unique(lubridate::ymd(date_vals)))) {
      time_freq <- "hourly"
    } else if (length(unique(lubridate::ymd(date_vals))) >
                 length(unique(lubridate::ym(date_vals)))) {
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

    lon_off  <- min(lon_idx)# - 1   # offset (0-based for RasterIO)
    lon_size <- length(lon_idx)
    lat_off  <- min(lat_idx)# - 1   # offset (0-based for RasterIO)
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

    x <- stars::read_stars(file_list,
                           along = concat_dim,
                           RasterIO = list(nXOff = lon_off,
                                           nYOff = lat_off,
                                           nXSize = lon_size,
                                           nYSize = lat_size),
                           curvilinear = FALSE)
    lon_vals <- stars::st_get_dimension_values(x, "x")
    lat_vals <- stars::st_get_dimension_values(x, "y")
    date_vals <- stars::st_get_dimension_values(x, concat_dim)
    date_vals <- as.POSIXct(date_vals, origin = "1970-01-01", tz = "UTC")

    units <- sapply(x, function(y) attr(y, "units")$numerator[1])
    names(units) <- NULL

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
    data_out[[length(data_out) + 1]] <- data
    units_out[[length(units_out) + 1]] <- units
  }

  if (length(data_out) == 1) {
    data_out <- data_out[[1]]
  } else if (length(data_out) == 0) {
    stop("No valid data found.")
  } else {
    data_out <- CSTools::CST_BindDim(data_out, along = "var",
                                     var_dim = "var", dat_dim = "dataset")
  }
  data_out$attrs$source_files <- unlist(file_list_var)
  data_out$attrs$source_files <- gsub("NETCDF:\"(.*)\"", "\\1",
                                      data_out$attrs$source_files)
  data_out$attrs$source_files <- gsub(":(.*)", "\\1",
                                      data_out$attrs$source_files)
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
  start_dates <- .paste_outer(year, sprintf("-%02d", month))

  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
    start_dates <- .paste_outer(start_dates,
                                sprintf("-%02d", day))

    start_dates <- .paste_outer(start_dates,
                                sprintf(" %02d:00:00", time))

    leadtime_month <- 1
    grid_dates <- expand.grid(sdate =
                                as.POSIXct(start_dates, tz = "UTC",
                                           format = "%Y-%m-%d %H:%M:%S"),
                              time = leadtime_month)

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

  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
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

  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
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

  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
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
  if (length(leadtime_month) == 1 && any(leadtime_month == 1)) {
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
  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
    # case: only one leadtime month
    start_dates <- .paste_outer(.paste_outer(year,
                                             sprintf("-%02d", month)),
                                sprintf("-%02d", day))
    leadtime_month <- 1
    grid_dates <- expand.grid(sdate = as.Date(start_dates),
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
                                           format = "%Y-%m-%d"))

    grid_dates <- rev(grid_dates)
    grid_dates$forecast_date <- lubridate::add_with_rollback(
      grid_dates$sdate, months(grid_dates$time - 1)
    )
    grid_dates$forecast_date <-
      paste0(lubridate::year(grid_dates$forecast_date),
             sprintf("-%02d", lubridate::month(grid_dates$forecast_date)),
             grid_dates$day)
    grid_dates$forecast_date <-
      as.Date(grid_dates$forecast_date,
              format = "%Y-%m-%d")
  }

  start_dates <- unique(sort(c(grid_dates$forecast_date)))
  if (!(any(as.Date(date_vals) %in% start_dates))) {
    stop(paste0("No matching dates found for the specified year, month,",
                " day, and time. Dates found in data lie in the range: ",
                paste0(range(as.Date(date_vals)), collapse = " - "),
                ". Please adjust the date selection accordingly."))
  }
  if (!(all(as.Date(date_vals) %in% start_dates))) {
    filter_indx <- as.Date(stars::st_get_dimension_values(x, "valid_time"),
                           origin = "1970-01-01") %in% start_dates
    filter_indx <- which(filter_indx)
    x <-  .slice_stars_dimname(x, "valid_time", filter_indx)
  }

  dims <- stars::st_dimensions(x)
  date_vals <- as.Date(stars::st_get_dimension_values(x, "valid_time"),
                       origin = "1970-01-01")
  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
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
  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
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
  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
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
  if (length(leadtime_month) == 1 && any(leadtime_month == 1)) {
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
  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
    # case: only one leadtime month
    start_dates <- .paste_outer(.paste_outer(year,
                                             sprintf("-%02d", month)),
                                sprintf("-%02d", day))
    leadtime_month <- 1
    grid_dates <- expand.grid(sdate = as.Date(start_dates),
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
                                           format = "%Y-%m-%d"))
    grid_dates <- rev(grid_dates)
    grid_dates$forecast_date <- lubridate::add_with_rollback(
      grid_dates$sdate, months(grid_dates$time - 1)
    )
    grid_dates$forecast_date <-
      paste0(lubridate::year(grid_dates$forecast_date),
             sprintf("-%02d", lubridate::month(grid_dates$forecast_date)),
             grid_dates$day)
    grid_dates$forecast_date <-
      as.Date(grid_dates$forecast_date,
              format = "%Y-%m-%d")
  }
  start_dates <- unique(sort(c(grid_dates$forecast_date)))
  if (!(any(as.Date(date_vals) %in% start_dates))) {
    stop(paste0("No matching dates found for the specified year, month,",
                " day, and time. Dates found in data lie in the range: ",
                paste0(range(as.Date(date_vals)), collapse = " - "),
                ". Please adjust the date selection accordingly."))
  }
  if (!(all(as.Date(date_vals) %in% start_dates))) {
    filter_indx <- as.Date(stars::st_get_dimension_values(x, "valid_time"),
                           origin = "1970-01-01") %in% start_dates
    filter_indx <- which(filter_indx)
    x <-  .slice_stars_dimname(x, "valid_time", filter_indx)
  }
  dims <- stars::st_dimensions(x)
  date_vals <- as.Date(stars::st_get_dimension_values(x, "valid_time"),
                       origin = "1970-01-01")
  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
    grid_dates <- grid_dates[grid_dates$forecast_date %in% date_vals, ]
  }
  grid_dates <- grid_dates[as.Date(grid_dates$sdate,
                                   origin = "1970-01-01") %in% date_vals &
                             as.Date(grid_dates$forecast_date,
                                     origin = "1970-01-01") %in% date_vals, ]
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
  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
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
  if (any(leadtime_month == "all") ||
        (length(leadtime_month) == 1 && (leadtime_month == 1))) {
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
  if (length(leadtime_month) == 1 && any(leadtime_month == 1)) {
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

  names(coords) <- sub("^x$", "longitude", names(coords))
  names(coords) <- sub("^y$", "latitude", names(coords))
  names(coords) <- sub("^forecastMonth$", "time", names(coords))
  names(coords) <- sub("^forecast_reference_time$", "sdate", names(coords))
  names(coords) <- sub("^number$", "ensemble", names(coords))

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

  coords <- coords[c("dataset", "var", "sdate", "time",
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
