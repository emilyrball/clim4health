#source(paste0("https://earth.bsc.es/gitlab/external/cstools/-/blob/",
#              "master/R/CST_BindDim.R"))
source(paste0("R/CST_BindDim.R"))
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