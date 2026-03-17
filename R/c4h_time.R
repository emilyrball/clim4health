#' c4h_time
#'
#' Temporal aggregation of climate products.
#'
#' @param data s2dv_cube object with the climate data to be aggregated,
#' usually read using `c4h_load`.
#' @param time_aggregation string indicating the type of temporal aggregations
#' to be performed. Options are "monthly", "yearly", "weekly", and "daily".
#' @param dim_aggregation string indicating the dimension along which the
#' aggregation should be performed. Options are "sdate" and "time".
#' @param fun string specifying the aggregation function. Options are
#' "mean", "sum", "max", "min". Defaults to "mean".
#' @param week_start for weekly aggregation, specify the starting weekday from
#' "Monday" to "Sunday".
#' @param min_valid integer specifying the minimum number of valid (non-NA) 
#' values desired in a group to return a non-NA aggregated value. Defaults to 
#' 1, so that any group with at least one valid value will be aggregated.
#'
#' @returns an s2dv_cube object with the temporally aggregated climate data.
#' @export
#'
#' @examples
#' # Load station data
#' station_path <- system.file("extdata", "stations", package = "clim4health")
#' station_timeseries <- c4h_load(station_path, variable = "temp_mean",
#'                                ext = "csv")
#' stat_week <- c4h_time(station_timeseries, time_aggregation = "weekly",
#'                       dim_aggregation = "time", week_start = "Monday",
#'                       min_valid = 5)
#' dim(stat_week$data)
#'
#' # Load station data with sdate dimension
#' station_data <- c4h_load(station_path, variable = "temp_mean", ext = "csv",
#'                          month = 1, leadtime_month = 1:12)
#' # Aggregate to monthly max across sdate dimension
#' stat_month <- c4h_time(station_data, time_aggregation = "monthly",
#'                        dim_aggregation = "sdate", fun = "max",
#'                        min_valid = 20)
#' dim(stat_month$data)
c4h_time <- function(data,
                     time_aggregation,
                     dim_aggregation,
                     fun = "mean",
                     week_start = NULL,
                     min_valid = 1) {

  # Input checks ----

  # Check if aggregation type is valid
  weekdays_eng <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                    "Friday", "Saturday", "Sunday")
  if (!time_aggregation %in% c("daily", "weekly", "monthly", "yearly")) {
    stop("Aggregation must be 'daily', 'weekly', 'monthly' or 'yearly'")
  } else if (time_aggregation == "weekly") {
    if (is.null(week_start)) {
      stop("'week_start' is required when 'time_aggregation' is weekly.")
    } else if (!week_start %in% weekdays_eng) {
      stop("Invalid 'week_start', please check documentation.")
    }
  }

  # Sum or Mean
  if (fun %in% c("mean", "sum", "max", "min")) {
    fun <- get(fun)
  } else {
    stop("Invalid choice of parameter 'fun':  ",
         "Please select 'mean', 'sum', 'max' or 'min'.")
  }

  # Aggregation variable
  if (!dim_aggregation %in% c("sdate", "time")) {
    stop("Invalid choice of parameter 'dim_aggregation'. Please select",
         " 'sdate' or 'time'.")
  }

  # Auxiliary objects  ----

  # Data
  data_tmp <- data$data
  aggregated_data <- NULL

  # Time and fixed time dimension
  time <- data$attrs$Dates
  if (dim_aggregation == "time") {
    dim_fix <- "sdate"
  } else {
    dim_fix <- "time"
  }
  dim_fix_pos <- which(names(dim(time)) == dim_fix)

  if (is.null(attr(time, "tzone"))) {
    time_zone <- "UTC"
    message("Time zone not specified in the input data. Assuming UTC.")
  } else if (attr(time, "tzone") == "") {
    time_zone <- Sys.timezone()
    message(paste0("Time zone in the input data is empty. Assuming",
                   " system timezone: ", time_zone))
  } else {
    time_zone <- attr(time, "tzone")
    message(paste0("Time zone in the input data is: ", time_zone))
  }

  # Temporal aggregation ----

  # Loop through the fixed time dimension
  for (i in seq(dim(time)[dim_fix])) {

    ## Get time groupings ----

    # empty index of length 2 (time and sdate)
    index_list <- rep(list(quote(expr = )), length(dim(time)))
    # We fix the dimension that is not being aggregated
    index_list[[dim_fix_pos]] <- i
    # Subset dates accordingly
    time_i <- do.call(`[`, c(list(time), index_list))
    # Remove missing timestamps
    na_idx <- is.na(time_i)
    time_i <- time_i[!is.na(time_i)]

    # time_groups: same dimension as time_i, used for the aggregation
    # dates_i: resulting time stamps to be used after aggregation
    time_groups <- if (time_aggregation == "yearly") {
      dates_i <- as.POSIXct(paste0(unique(lubridate::year(time_i)),
                                   "-01-01"), tz = time_zone)
      lubridate::year(time_i)

    } else if (time_aggregation == "monthly") {
      # Extract year-month as groups
      dates_i <- unique(as.POSIXct(paste(format(time_i, "%Y-%m"),
                                         "01", sep = "-"), tz = time_zone))
      paste(format(time_i, "%Y-%m"), "01", sep = "-")

    } else if (time_aggregation == "daily") {
      # Extract daily as groups
      dates_i <- unique(as.POSIXct(format(time_i, "%Y-%m-%d"), tz = time_zone))
      as.POSIXct(format(time_i, "%Y-%m-%d"), tz = time_zone)

    } else if (time_aggregation == "weekly") {
      # Epidemiological weeks and years
      time_i <- as.Date(time_i)
      dates_tmp_i <- GHRexplore::date_to_epiyw(date = time_i,
                                               weekday = week_start)
      # convert back to ensure matching to the correct week start
      dates_i <- unique(GHRexplore::epiyw_to_date(epiyw = dates_tmp_i,
                                                  weekday = week_start))
      paste(dates_tmp_i)
    }

    # Compute aggregation ----

    # Construct full aggregation array: assign loop index to
    # the fixed time dimension
    # And compute across dim_aggregation
    dims <- dim(data_tmp)
    agg_dim_idx <- which(names(dims) == dim_aggregation)
    fix_dim_idx <- which(names(dims) == dim_fix)
    index <- rep(list(quote(expr = )), length(dims))
    index[[fix_dim_idx]] <- i
    dims_i <- dims
    dims_i[fix_dim_idx] <- 1
    index[[agg_dim_idx]] <- which(!na_idx)

    # Get data for that array slice
    data_tmp_i <- do.call(`[`, c(list(data_tmp), index, drop = FALSE))

    # Apply aggregation function
    aggregated_data_i <-
      apply(data_tmp_i, # sliced data
            seq(dims_i)[-agg_dim_idx], # all dimensions but dim_aggregation
            function(x) {
              tapply(x, time_groups,
                     .aggregate_temporally, fun = fun, min_valid = min_valid)
            })

    # Fix dimensions ----
    if (length(dims_i) == length(dim(aggregated_data_i))) {
      # All dimensions already present
      names(dim(aggregated_data_i)) <- c(dim_aggregation,
                                         names(dims_i)[-agg_dim_idx])
      # Reorder dimensions to the original
      new_dims <- match(names(dims_i), names(dim(aggregated_data_i)))
      aggregated_data_i <- aperm(aggregated_data_i, new_dims)

    } else {
      # Missing dimension after aggregation (when only one group in time)
      aggregated_data_i <- s2dv::InsertDim(aggregated_data_i,
                                           posdim = agg_dim_idx,
                                           lendim = 1,
                                           name = dim_aggregation)
    }

    # Pad daily data to have full months
    if (time_aggregation == "daily") {
      tmp <- .pad_array(aggregated_data_i,
                        dates_i,
                        dim_aggregation = dim_aggregation)

      aggregated_data_i <- tmp$array
      dates_i <- tmp$dates
    }

    # Create s2dv_cube object ----

    # Main s2dv object
    aggregated_data_i <- list(data = aggregated_data_i,
                              dims = dim(aggregated_data_i),
                              coords = data$coords,
                              attrs = data$attrs)

    # Formatting the dates attribute: format and dimensions
    if (time_aggregation == "daily") {
      aggregated_data_i$attrs$Dates <- as.POSIXct(dates_i, tz = time_zone)
    } else if (time_aggregation == "weekly") {
      aggregated_data_i$attrs$Dates <- as.POSIXct(dates_i, format = "%Y-%m-%d",
                                                  tz = time_zone)
    } else {
      aggregated_data_i$attrs$Dates <- dates_i
    }
    if (dim_aggregation == "time") {
      dim(aggregated_data_i$attrs$Dates) <- c(sdate = 1,
                                              time = length(dates_i))
    } else {
      dim(aggregated_data_i$attrs$Dates) <- c(sdate = length(dates_i),
                                              time = 1)
    }

    # Set class
    class(aggregated_data_i) <- "s2dv_cube"

    # Bind within the loop ----
    # Across the fixed dimension
    if (i == 1) {
      aggregated_data <- aggregated_data_i
    } else {
      aggregated_data <- CSTools::CST_BindDim(x = list(aggregated_data,
                                                       aggregated_data_i),
                                              along = dim_fix,
                                              var_dim = "var",
                                              dat_dim = "dataset")
      if (time_aggregation == "daily") {
        aggregated_data$attrs$Dates <- as.POSIXct(aggregated_data$attrs$Dates,
                                                  origin = "1970-01-01",
                                                  tz = time_zone)
      }
    }

    data <- aggregated_data
    data$coords[["time"]] <- seq((dim(data$attrs$Dates)["time"]))
    data$coords[["sdate"]] <- data$attrs$Dates[, 1]
  }

  data$dims <- dim(data$data)

  return(data)
}