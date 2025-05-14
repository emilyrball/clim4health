#' Temporal aggregation of climate products.
#'
#' @param data_clim s2dv_cube object with the climate data to be aggregated,
#' usually read using `clim4health_load`.
#' @param aggregation string indicating the type of temporal aggregations to be
#' performed. Options are "monthly", "yearly", and "daily". Defaults to "yearly".
#' @param agg_fun string specifying the aggregation function. Defaults to "mean".
#' @param week_start for weekly aggregation, specify the starting weekday. Current
#' options include "Sunday" and "Monday".
#'
#' @returns 2dv_cube object with the temporally aggregated climate data.
#' @export
#' 
#' @examples
#' # Monthly aggregation of reanalysis data
#' obs_path <- system.file("extdata/reanalysis/", package = "clim4health")
#' obs_path <- paste0(obs_path, "/")
#' obs <- clim4health_load(obs_path, var = "t2m", data_type = "obs",
#'                          sdates = "201004", ext = 'nc')
#' obs_month <- clim4health_time(obs, aggregation = "monthly")
#' dim(obs_month$data)

clim4health_time <- function(data_clim,
                             aggregation = "yearly",
                             agg_fun = "mean",
                             week_start = NULL) {
  
  data <- data_clim$data
  time <- as.Date(data_clim$coords$time)
  
  # Check if aggregation type is valid
  if (!aggregation %in% c("monthly", "yearly", "weekly", "daily")) {
    stop("Aggregation must be 'daily', 'weekly', 'monthly' or 'yearly'")
  } else if ((aggregation == "weekly") & (is.null(week_start))) {
    stop(paste0("If aggregating to weekly, please provide the day on which",
                "your epiweek starts"))
  }
  
  # Sum or Mean
  if (agg_fun == "mean") {
    agg_fun <- mean
  } else if (agg_fun == "sum") {
    agg_fun <- sum
  } else {
    stop("Please select 'mean' or 'sum' to aggregate.")
  }
  
  # Define function to add left "0"
  paste_leadzero <- function(x) {
    return(paste0(ifelse(x < 10, "0", ""), x))
  }
  
  # Parse time into appropriate groups
  time_groups <- if (aggregation == "yearly") {
    # Extract years
    dates <- unique(lubridate::year(time))
    lubridate::year(time)
  } else if (aggregation == "monthly") {
    # Extract year-month as groups
    years <- lubridate::year(time)
    month <- lubridate::month(time)
    dates_tmp <- paste(years, paste_leadzero(month), sep = "-")
    dates <- unique(dates_tmp)
    paste(years, paste_leadzero(month), sep = "-")
  } else if (aggregation == "daily") {
    # Extract daily as groups
    dates <- unique(lubridate::ymd(time))
    lubridate::ymd(time)
  } else if (aggregation == "weekly") {
    if (week_start == "Sunday") {
      epiweeks <- lubridate::epiweek(time)
    } else if (week_start == "Monday") {
      epiweeks <- lubridate::isoweek(time)
    } else {
      stop("Please check 'week_start' parameter. Valid options are 'Sunday'
           and 'Monday'.")
    }
    epiyears <- lubridate::year(time)
    # assign previous year to first days of January if necessary
    epiyears[lubridate::month(time) == 1 & epiweeks > 50] <-
      epiyears[lubridate::month(time) == 1 & epiweeks > 50] - 1
    dates_tmp <- paste(epiyears, paste_leadzero(epiweeks), sep = "-")
    dates <- unique(dates_tmp)
    paste(epiyears, paste_leadzero(epiweeks), sep = "-")
  }
  
  # Assign time groups to data
  # Reshape the data into a long format for easier aggregation
  dims <- dim(data)
  date_dim <- which(names(dims) == "time")
  
  aggregated_data <- apply(data, seq(dims)[ -date_dim], 
                           function(x) tapply(x, time_groups, agg_fun))
  if (length(dims) == length(dim(aggregated_data))) {
    names(dim(aggregated_data)) <- c("time", names(dims)[ -date_dim])
    
    new_dims <- match(names(dims), names(dim(aggregated_data)))
    aggregated_data <- aperm(aggregated_data, new_dims)
  } else { # if there is only one 
    aggregated_data <- s2dv::InsertDim(aggregated_data, posdim = date_dim,
                                       lendim = 1, name = "time")
  }
  
  data_clim$data        <- aggregated_data
  data_clim$coords$time <- dates
  
  return(data_clim)
}
