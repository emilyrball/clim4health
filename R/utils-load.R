#source(paste0("https://earth.bsc.es/gitlab/external/cstools/-/blob/",
#              "master/R/CST_BindDim.R"))
#source(paste0("R/CST_BindDim.R"))
#' .paste_hours_minutes
#'
#' TBC
#'
#' @param hours Vector of hours.
#' @param minutes Vector of minutes.
#'
#' @returns A vector of concatenated hours and minutes.
#'
.paste_hours_minutes <- function(hours, minutes) {
  hours <- paste0(hours)
  minutes <- paste0(minutes)
  hours <- .paste_zeros(hours)
  minutes <- .paste_zeros(minutes)
  return(paste(hours, minutes, sep = ":"))
}

#' .paste_zeros
#'
#' TBC
#'
#' @param times vector of time strings.
#'
#' @returns The vector of time strings with a leading 0 if applicable.
#'
.paste_zeros <- function(times) {
  for (i in seq_along(times)) {
    if (nchar(times[i]) == 1) {
      times[i] <- sprintf("%02d", times[i])
    }
  }
  return(times)
}

#' .paste_outer
#'
#' TBC
#'
#' @param path TBC
#' @param ext TBC
#'
#' @returns TBC
.paste_outer <- function(path, ext) {
  path <- outer(path, ext, FUN = "paste0")
  path <- t(path)
  dim(path) <- NULL
  return(path)
}

#' .spatial_select
#'
#' TBC
#'
#' @param selection TBC
#'
#' @returns TBC
.spatial_select <- function(selection) {
  if (any(selection == "all")) {
    select <- "all"
  } else {
    select <- startR::values(list(min(selection), max(selection)))
  }
  return(select)
}

#' .get_spatial_length
#'
#' TBC
#'
#' @param dim_index TBC
#' @param file_dims TBC
#'
#' @returns TBC
.get_spatial_length <- function(dim_index, file_dims) {
  if (is.null(dim_index)) {
    dim_length <- NULL
  } else {
    dim_length <- file_dims[[dim_index]]
  }
  return(dim_length)
}

#' .get_spatial_name
#'
#' TBC
#'
#' @param file_dim_names TBC
#' @param possible_names TBC
#'
#' @returns TBC
.get_spatial_name <- function(file_dim_names, possible_names) {
  for (f in possible_names) {
    # add f to time_dims if included in file_dims
    if (any(grep(f, tolower(file_dim_names)))) {
      dim_name <- file_dim_names[grep(f, tolower(file_dim_names))]
      break
    }
  }
  return(dim_name)
}

#' .get_time_names
#'
#' TBC
#'
#' @returns TBC
.get_time_names <- function() {
  return(c("year", "month", "week", "day", "date", "time"))
}

#' .get_spatial_val
#'
#' TBC
#'
#' @param dim_length TBC
#' @param full_path TBC
#' @param dim_name TBC
#' @param dim TBC
#'
#' @returns TBC
.get_spatial_val <- function(dim_length, full_path, dim_name, dim) {
  if (is.null(dim_length)) {
    # if lat is not stored in file
    spatial_val <- startR::indices(1)
  } else if (dim_length > 1) {
    spatial_val <- NULL
  } else if (dim_length == 1) {
    # in case startR drops value
    spatial_val <- startR::NcVarReader(file_path = full_path,
                                       var_name = dim,
                                       synonims = list(dim = dim_name))[[1]]
  }
  return(spatial_val)
}

#' .get_csv_dims
#'
#' TBC
#'
#' @param headers TBC
#' @param data TBC
#' @param names TBC
#'
#' @returns TBC
.get_csv_dims <- function(headers, data, names) {
  if (any(names %in% headers)) {
    col <- which(headers %in% names)
    val <- data[, col]
  } else {
    col <- NULL
    val <- rep(startR::indices(1), length(data[, 1]))
  }
  return(list(val = val, col = col))
}

.load_cds_obs_monthly <- function(var = NULL,
                                  full_path = NULL,
                                  path = NULL,
                                  date = NULL,
                                  lat_selection = "all",
                                  lon_selection = "all") {
  # dimensions 'valid_time', 'latitude', 'longitude'
  if (length(var) == 1) {
    dataset <- paste0(path, "$var$_$date$.nc")
  } else {
    # if more than one variable is provided, assume
    # files are stored as path/var/var_date.nc
    dataset <- paste0(path, "$var$/$var$_$date$.nc")
  }
  # if date does not have sdate dimension, add it
  if (is.null(dim(date))) {
    dim(date) <- c(sdate = 1, time = length(date))
  } else if ((length(dim(date)) == 1) && names(dim(date)) == "time") {
    # if date has only time dimension, add sdate dimension
    dim(date) <- c(sdate = 1, time = length(date))
  } else if ((length(dim(date)) == 1) && names(dim(date)) == "sdate") {
    # if date has only sdate dimension, add time dimension
    dim(date) <- c(sdate = length(date), time = 1)
  }

  start_parameters <- list(dataset = dataset,
                           var = var,
                           date = date,
                           latitude = .spatial_select(lat_selection),
                           latitude_var = "latitude",
                           longitude = .spatial_select(lon_selection),
                           longitude_var = "longitude",
                           pattern_dims = "dataset",
                           latitude_reorder = startR::Sort(decreasing = TRUE),
                           longitude_reorder = startR::CircularSort(-180, 180),
                           return_vars = list(latitude = "dataset",
                                              longitude = "dataset",
                                              time = "date"),
                           synonims = list(time = c("time", "valid_time")),
                           split_multiselected_dims = TRUE,
                           largest_dims_length = TRUE,
                           silent = TRUE,
                           retrieve = TRUE)

  data <- do.call(startR::Start, start_parameters)

  # transform to sd2v_cube
  data <- CSTools::as.s2dv_cube(data)
  return(data)
}

.load_cds_obs_daily <- function(var = NULL,
                                full_path = NULL,
                                path = NULL,
                                date = NULL,
                                lat_selection = "all",
                                lon_selection = "all") {
  # dimensions 'valid_time', 'latitude', 'longitude'
  if (length(var) == 1) {
    dataset <- paste0(path, "$var$_$date$.nc")
  } else {
    # if more than one variable is provided, assume
    # files are stored as path/var/var_date.nc
    dataset <- paste0(path, "$var$/$var$_$date$.nc")
  }

  if (!("sdate" %in% names(dim(date)))) {
    start_parameters <- list(dataset = dataset,
                             var = var,
                             date = date,
                             time = "all",
                             latitude = .spatial_select(lat_selection),
                             latitude_var = "latitude",
                             longitude = .spatial_select(lon_selection),
                             longitude_var = "longitude",
                             pattern_dims = "dataset",
                             latitude_reorder =
                               startR::Sort(decreasing = TRUE),
                             longitude_reorder =
                               startR::CircularSort(-180, 180),
                             return_vars = list(latitude = "dataset",
                                                longitude = "dataset",
                                                time = "date"),
                             synonims = list(time = c("time", "valid_time")),
                             time_across = c("date"),
                             merge_across_dims = TRUE,
                             largest_dims_length = TRUE,
                             silent = TRUE,
                             retrieve = TRUE)

    data <- do.call(startR::Start, start_parameters)
    # transform to sd2v_cube
    data <- CSTools::as.s2dv_cube(data)
    data <- CSTools::CST_InsertDim(data, 3, 1, "sdate",
                                   values = data$attrs$Dates[1])
    dim(data$attrs$Dates) <- c(sdate = 1, time = length(data$attrs$Dates))
  } else {
    dims <- names(dim(date))
    sdate_dim <- which(dims == "sdate")
    data <- NULL
    for (i in seq(dim(date)[["sdate"]])) {
      idx <- vector("list", length(dims))
      idx[] <- TRUE
      idx[[sdate_dim]] <- i
      date_tmp <- do.call(`[`, c(list(date), idx))
      dim(date_tmp) <- c(sdate = 1, time = length(date_tmp))
      start_parameters <- list(dataset = dataset,
                               var = var,
                               date = date_tmp,
                               time = "all",
                               latitude = .spatial_select(lat_selection),
                               latitude_var = "latitude",
                               longitude = .spatial_select(lon_selection),
                               longitude_var = "longitude",
                               pattern_dims = "dataset",
                               latitude_reorder =
                                 startR::Sort(decreasing = TRUE),
                               longitude_reorder =
                                 startR::CircularSort(-180, 180),
                               return_vars = list(latitude = "dataset",
                                                  longitude = "dataset",
                                                  time = "date"),
                               synonims = list(time = c("time", "valid_time")),
                               time_across = c("date"),
                               merge_across_dims = TRUE,
                               largest_dims_length = TRUE,
                               silent = TRUE,
                               retrieve = TRUE)

      data_tmp <- do.call(startR::Start, start_parameters)
      # transform to sd2v_cube
      data_tmp <- CSTools::as.s2dv_cube(data_tmp)
      data_tmp <- CSTools::CST_InsertDim(data_tmp, 3, 1, "sdate",
                                         values = data_tmp$attrs$Dates[1])
      dim(data_tmp$attrs$Dates) <- c(sdate = 1,
                                     time = length(data_tmp$attrs$Dates))
      if (i == 1) {
        data <- data_tmp
      } else {
        data <- CST_BindDim(x = list(data, data_tmp), along = "sdate",
                            var_dim = "var", dat_dim = "dataset")
      }
    }
    data$coords$sdate <- data$attrs$Dates[, 1]
  }
  return(data)
}

.load_cds_seasonal_monthly <- function(var = NULL,
                                       full_path = NULL,
                                       path = NULL,
                                       date = NULL,
                                       lat_selection = "all",
                                       lon_selection = "all") {
  # dimensions 'number', 'forecast_reference_time', 'forecastMonth',
  # 'latitude', 'longitude'
  if (length(var) == 1) {
    dataset <- paste0(path, "$var$_$sdate$.nc")
  } else {
    # if more than one variable is provided, assume
    # files are stored as path/var/var_date.nc
    dataset <- paste0(path, "$var$/$var$_$sdate$.nc")
  }

  # in this case, date is corresponding to start date of the forecast
  if (!is.null(dim(date))) {
    dim(date) <- NULL
  }

  # first access the files to get the start dates
  start_parameters <- list(dataset = dataset,
                           var = var,
                           sdate = date,
                           time = "all",
                           forecast_reference_time = "all",
                           ensemble = "all",
                           latitude = .spatial_select(lat_selection),
                           latitude_var = "latitude",
                           longitude = .spatial_select(lon_selection),
                           longitude_var = "longitude",
                           pattern_dims = "dataset",
                           latitude_reorder = startR::Sort(decreasing = TRUE),
                           longitude_reorder = startR::CircularSort(-180, 180),
                           return_vars =
                             list(latitude = "dataset",
                                  longitude = "dataset",
                                  time = "sdate",
                                  forecast_reference_time = "sdate"),
                           synonims =
                             list(time = c("time", "forecastMonth"),
                                  ensemble = c("ensemble", "number")),
                           largest_dims_length = TRUE,
                           silent = TRUE,
                           retrieve = FALSE)

  data <- do.call(startR::Start, start_parameters)

  start_dates <- attr(data, "Variables")$common$forecast_reference_time
  lead_times <- attr(data, "Variables")$common$time

  metadata_dates <- lubridate::add_with_rollback(start_dates,
                                                 months(lead_times - 1))
  dim(metadata_dates) <- dim(lead_times)

  start_parameters <- list(dataset = dataset,
                           var = var,
                           sdate = date,
                           time = "all",
                           ensemble = "all",
                           latitude = .spatial_select(lat_selection),
                           latitude_var = "latitude",
                           longitude = .spatial_select(lon_selection),
                           longitude_var = "longitude",
                           pattern_dims = "dataset",
                           latitude_reorder = startR::Sort(decreasing = TRUE),
                           longitude_reorder = startR::CircularSort(-180, 180),
                           return_vars =
                             list(latitude = "dataset",
                                  longitude = "dataset",
                                  time = "sdate"),
                           synonims =
                             list(time = c("time", "forecastMonth"),
                                  ensemble = c("ensemble", "number")),
                           largest_dims_length = TRUE,
                           silent = TRUE,
                           retrieve = TRUE)

  # transform to sd2v_cube
  data <- do.call(startR::Start, start_parameters)
  data <- CSTools::as.s2dv_cube(data)

  # store dates
  data$attrs$Dates <- metadata_dates

  # store start dates
  sdates <- metadata_dates[, 1]
  data$coords$sdate <- sdates

  return(data)
}

.load_cds_seasonal_daily <- function() {
  # dimensions 'number', 'forecast_reference_time', 'forecast_period',
  # 'latitude', 'longitude'
}

.load_csv_tolimavalle <- function(data, var = NULL,
                                  dates = NULL,
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
  data[["time"]] <- as.POSIXct(paste(data[["date"]], data[["hour"]]), tz = "COT")
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

    # Fill array
    #for (row in 1:nrow(data_tmp)) {
    #  v <- data_tmp[row, ]
    #  data_array[1, v[["var_idx"]], 1, v[["time_idx"]], 1,
    #             v[["lat_idx"]], v[["lon_idx"]]] <- v[["value"]]
    #}
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
      data_out <- CST_BindDim(x = list(data_out, data_tmp), along = "sdate",
                              var_dim = "var", dat_dim = "dataset")
    }
  }
  data_out$coords$sdate <- data_out$attrs$Dates[, 1]
  return(data_out)
}



.load_csv_dr <- function(data, var = NULL,
                         dates = NULL) {

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

  data[["year"]][data[["year"]] == 22022] <- 2022
  data[["year"]][data[["year"]] == 32023] <- 2023
  data[["year"]][data[["year"]] == 32024] <- 2024

  date_tmp <- paste(data[["year"]],
                    sprintf("%02d", data[["month"]]),
                    sprintf("%02d", data[["day"]]), sep = "-")

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


.load_csv_unknown <- function(full_path, var = NULL,
                              date = NULL, data_type = "unknown",
                              lat_selection = "all",
                              lon_selection = "all") {
  
  # get lats/lons
  lats <- .get_csv_dims(headers, data, c("y", "lat", "latitude"))
  lons <- .get_csv_dims(headers, data, c("x", "lon", "longitude"))

  # get time columns
  ### test what happens if you have a year and month column
  possible_time_names <- .get_time_names()
  times <- .get_csv_dims(headers, data, possible_time_names)

  vars <- list()
  # get variable
  if (!is.null(var)) {
    if (any(headers %in% var)) {
      # if var name exists in file
      vars$col <- which(headers %in% var)
    } else {
      # if var name does not exist
      stop(paste0("var name ", var, " not found"))
    }
  } else {
    # if variable is not specified assume it is the last one
    vars$col <- length(headers)
    var <- headers[length(headers)]
  }
  vars$val <- data[, vars$col]

  # run through possible time/date combinations
  ### what about case YYYY and MM stored in separate columns?
  print("Trying date formats")
  sum_na <- length(times$val)
  dates <- NA
  for (f in list(lubridate::ymd, lubridate::dmy, lubridate::mdy,
                 lubridate::ym, lubridate::my)) {
    dates_tmp <- f(times$val)
    if (sum(is.na(dates_tmp)) < sum_na) {
      dates <- dates_all <- dates_tmp
      sum_na <- sum(is.na(dates_tmp))
      ## choose a date format that produces least NAs
    }
  }
  if (!typeof(dates) == "logical") {
    dates <- unique(dates)
  }

  # could be stored as "YYYY/MM/DD hh:mm:ss"
  print("Trying YYYY/MM/DD hh:mm:ss date format")
  dates_tmp <- tryCatch({
    as.POSIXct(times$val, tz = "UTC")
  },
  error = function(x) {
    return(NA)
  },
  warning = function(x) {
    return(NA)
  })

  # if length of dates_tmp is longer then hourly data could be stored
  if (length(unique(dates_tmp)) > length(dates)) {
    dates <- dates_all <- dates_tmp
  }

  # final option could be year is stored as date
  if (typeof(dates) == "logical") {
    print("Trying YYYY date format")
    dates <- tryCatch({
      lubridate::year(times$val)
    },
    error = function(x) {
      return(NA)
    },
    warning = function(x) {
      return(NA)
    })
    dates_all <- dates
  }
  if (typeof(dates) == "logical") {
    stop("Cannot parse dates. Stopping")
  }

  dates <- unique(dates)

  # obtain metadata
  lat <- c() # initialise
  lon <- c() # initialise

  if (!(is.null(lats$col)) && !(is.null(lons$col))) {
    coords <- unique(paste0(lats$val, "; ", lons$val))
    for (cc in seq_along(coords)){
      lat <- c(lat, as.integer(strsplit(coords[cc], "; ")[[1]][1]))
      lon <- c(lon, as.integer(strsplit(coords[cc], "; ")[[1]][2]))
    }
  } else {
    lat <- startR::indices(1)
    lon <- startR::indices(1)
  }

  # convert table to array of correct dimensions for s2dv_cube
  data_array <- array(data[, vars$col],
                      dim = c(dataset = 1, var = 1,
                              time = length(dates),
                              ensemble = 1,
                              latitude = length(lat), longitude = length(lon)))

  # reshape data and dates
  for (tt in seq_along(dates)) {
    ttx <- which(dates_all == dates[tt])
    for (ln in seq_along(lon)) {
      lnx <- which(lons$val == lon[ln])
      for (ll in seq_along(lat)) {
        llx <- which(lats$val == lat[ll])
        idxs <- Reduce(intersect, list(lnx, llx, ttx))
        if (!(length(idxs) == 0)) {
          data_array[, , tt, , ll, ln] <- data[idxs, vars$col]
        } else {
          data_array[, , tt, , ll, ln] <- NA
        }
      }
    }
  }

  # format to s2dv_cube
  data_tmp <- CSTools::s2dv_cube(data = data_array,
                                 coords = list(dataset = 1,
                                               var = var,
                                               time = dates,
                                               ensemble = 1,
                                               latitude = lat,
                                               longitude = lon),
                                 varName = var,
                                 Dates = dates)

  # subset to only the requested coordinates (lat_selection and lon_selection)
  if (any(lat_selection == "all")) {
    lat_select <- seq_along(lat)
  } else {
    lat_select <- which(lat >= lat_selection[1] & lat <= lat_selection[2])
  }
  if (any(lon_selection == "all")) {
    lon_select <- seq_along(lon)
  } else {
    lon_select <- which(lon >= lon_selection[1] & lon <= lon_selection[2])
  }
  data <- CSTools::CST_Subset(data_tmp, along = c("latitude", "longitude"),
                              indices = list(lat_select, lon_select),
                              drop = "none")

  if (!is.null(metadata)) {
    data$attrs$Variable$metadata <- metadata
  }
  return(data)
}