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
  return(paste(hours, minutes, sep = ':'))
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
      times[i] <- paste0("0", times[i])
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
    spatial_val <- startR::NcVarReader(file_path = full_path[1],
                                       var_name = dim,
                                       synonims = list(dim = dim_name))[[1]]
  }
  return(spatial_val)
}



#' .get_start_parameters
#' 
#' TBC
#'
#' @param path TBC
#' @param full_path TBC
#' @param file_dims TBC
#' @param dataset TBC
#' @param var TBC
#' @param sdates TBC
#' @param data_type TBC
#' @param lat_selection TBC
#' @param lon_selection TBC
#'
#' @returns TBC
.get_start_parameters <- function(path, full_path, file_dims,
                                  dataset = NULL, var = NULL, sdates = NULL,
                                  data_type = NULL,
                                  lat_selection = NULL, lon_selection = NULL) {
  
  possible_time_names <- .get_time_names()
  possible_lat_names <- c("latitude", "lat", "lat_1", "lat_2")
  possible_lon_names <- c("longitude", "lon", "lon_1", "lon_2")
  
  # file dim names
  file_dim_names <- names(file_dims)
  
  # initialise startR parameters
  synonims <- list()
  return_vars <- list()
  
  # extract time dimensions from files
  time_dims <- c()
  lower_time_dims <- c()
  
  for (f in possible_time_names) {
    # add f to time_dims if included in file_dims
    if (any(grep(f, tolower(file_dim_names)))) {
      dim_name_in_file <- file_dim_names[grep(f, tolower(file_dim_names))]
      time_dims <- c(time_dims, dim_name_in_file)
      lower_time_dims <- c(lower_time_dims, f)
      if (dim_name_in_file != f) {
        synonims[[f]] <- c(f, dim_name_in_file)
      }
    }
  }
  
  # define initial input parameters to startR
  start_parameters <- list(dataset = dataset,
                           var = var,
                           sdate = sdates)
  
  if ("time" %in% names(synonims)) {
    if (!(data_type == "obs")) {
      # if loading fcst/hcst, rename time to lead_time
      names(synonims)[which("time" == names(synonims))] <- "lead_time"
    } else {
      # if loading obs, rename time to time
      names(synonims)[which("time" == names(synonims))] <- "time"
    }
    # case of ecmwf seasonal forecast data downloaded using get
    if (all(c("forecastMonth", "forecast_reference_time")
            %in% file_dim_names)) {
      names(synonims)[which("lead_time" == names(synonims))] <- "time"
      names(synonims)[which("month" == names(synonims))] <- "lead_time"
    }
  } else {
    if (!(data_type == "obs")) {
      # if loading fcst/hcst, rename time to lead_time
      synonims$lead_time <- c("lead_time", "time")
    } else if ("time" %in% lower_time_dims) {
      start_parameters[["time"]] <- "all"
      return_vars[["time"]] <- "sdate"
    }
  }
  
  for (f in names(synonims)) {
    start_parameters[[f]] <- "all"
    return_vars[[f]] <- "sdate"
  }
  
  # if file includes ensemble dimension, add to startR parameters
  if (any(c("ensemble", "member", "number") %in% file_dim_names)) {
    start_parameters$ensemble <- "all"
    synonims$ensemble <- c("ensemble", "member", "number")
  }
  
  # select lat and lon
  lat_select <- .spatial_select(lat_selection)
  lon_select <- .spatial_select(lon_selection)
  
  lat_name <- .get_spatial_name(file_dim_names, possible_lat_names)
  lon_name <- .get_spatial_name(file_dim_names, possible_lon_names)
  
  lat_length <- .get_spatial_length(lat_name, file_dims)
  lon_length <- .get_spatial_length(lon_name, file_dims)
  
  lat_val <- .get_spatial_val(lat_length, full_path, lat_name, "latitude")
  lon_val <- .get_spatial_val(lon_length, full_path, lon_name, "longitude")
  
  if (is.null(lat_val)) {
    if (lat_name != "latitude") {
      synonims$latitude <- c("latitude", lat_name)
    }
    return_vars$latitude <- "dataset"
    start_parameters$latitude <- lat_select
    start_parameters$latitude_reorder <- startR::Sort(decreasing = TRUE)
  }
  if (is.null(lon_val)) {
    if (lon_name != "longitude") {
      synonims$longitude <- c("longitude", lon_name)
    }
    return_vars$longitude <- "dataset"
    start_parameters$longitude <- lon_select
    start_parameters$longitude_reorder <- startR::CircularSort(-180, 180)
  }
  
  # define final parameters for startR calls
  start_parameters$synonims <- synonims
  start_parameters$return_vars <- return_vars
  start_parameters$split_multiselected_dims <- TRUE
  start_parameters$merge_across_dims <- TRUE
  start_parameters$merge_across_dims_narm <- TRUE
  start_parameters$largest_dims_length <- TRUE
  
  return(list(sp = start_parameters, lat = lat_val, lon = lon_val))
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