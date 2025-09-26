#' c4h_time
#'
#' Temporal aggregation of climate products.
#'
#' @param data s2dv_cube object with the climate data to be aggregated,
#' usually read using `c4h_load`.
#' @param time_aggregation string indicating the type of temporal aggregations
#' to be performed. Options are "monthly", "yearly", "weekly", and "daily".
#' @param fun string specifying the aggregation function. Defaults to "mean".
#' @param dim_aggregation string indicating the dimension along which the
#' aggregation should be performed. Options are "sdate" and "time".
#' @param week_start for weekly aggregation, specify the starting weekday.
#' Current options include "Sunday" and "Monday".
#'
#' @returns an s2dv_cube object with the temporally aggregated climate data.
#' @export
#'
#' @examples
#' # # Monthly aggregation of reanalysis data
#' # obs_path <- system.file("extdata/reanalysis/", package = "clim4health")
#' # obs_path <- paste0(obs_path, "/")
#' # obs <- c4h_load(obs_path, variable = "t2m",
#' #                 year = 2010, month = 1, ext = "nc")
#' # obs_month <- c4h_time(obs, time_aggregation = "monthly",
#'                         dim_aggregation = "time")
#' # dim(obs_month$data)
c4h_time <- function(data,
                     time_aggregation,
                     fun = "mean",
                     dim_aggregation = NULL,
                     week_start = NULL) {
  # Check if aggregation type is valid
  if (!time_aggregation %in% c("monthly", "yearly", "weekly", "daily")) {
    stop("Aggregation must be 'daily', 'weekly', 'monthly' or 'yearly'")
  } else if ((time_aggregation == "weekly") && (is.null(week_start))) {
    stop(paste0("If aggregating to weekly, please provide 'week_start'",
                "parameter, the day on which your epiweek starts. Valid ",
                "options are 'Sunday' or 'Monday'."))
  }
  # Sum or Mean
  if (fun %in% c("mean", "sum", "max", "min")) {
    fun <- get(fun)
  } else {
    stop(paste0("Invalid choice of parameter 'fun'. ",
                "Please select 'mean', 'sum', 'max' or 'min' to aggregate."))
  }

  if (is.null(dim_aggregation) || !(dim_aggregation %in% c("sdate", "time"))) {
    stop("Invalid choice of parameter 'dim_aggregation'. Please select",
         " 'sdate' or 'time'.")
  }

  data_tmp <- data$data
  time <- data$attrs$Dates

  if (dim_aggregation == "time") {
    dim_fix <- "sdate"
  } else {
    dim_fix <- "time"
  }

  aggregated_data <- NULL
  dim_fix_pos <- which(names(dim(time)) == dim_fix)
  for (i in seq(dim(time)[dim_fix])) {
    index_list <- rep(list(quote(expr = )), length(dim(time)))
    index_list[[dim_fix_pos]] <- i
    time_i <- do.call(`[`, c(list(time), index_list))

    na_idx <- is.na(time_i)
    time_i <- time_i[!is.na(time_i)] # remove NAs

    # Parse time into appropriate groups
    time_groups <- if (time_aggregation == "yearly") {
      # Extract years
      dates_i <- unique(lubridate::year(time_i))
      lubridate::year(time_i)
    } else if (time_aggregation == "monthly") {
      # Extract year-month as groups
      dates_i <- unique(as.POSIXct(paste(format(time_i, "%Y-%m"),
                                         "01", sep = "-"), tz = "UTC"))
      paste(format(time_i, "%Y-%m"), "01", sep = "-")
    } else if (time_aggregation == "daily") {
      # Extract daily as groups
      dates_i <- unique(as.POSIXct(format(time_i, "%Y-%m-%d"), tz = "UTC"))
      as.POSIXct(format(time_i, "%Y-%m-%d"), tz = "UTC")
    } else if (time_aggregation == "weekly") {
      if (week_start == "Sunday") {
        epiweeks <- lubridate::epiweek(time_i)
      } else if (week_start == "Monday") {
        epiweeks <- lubridate::isoweek(time_i)
      } else {
        stop(paste0("Please check 'week_start' parameter. Valid options",
                    " are 'Sunday' and 'Monday'."))
      }
      epiyears <- lubridate::year(time_i)
      # assign previous year to first days of January if necessary
      epiyears[lubridate::month(time_i) == 1 & epiweeks > 50] <-
        epiyears[lubridate::month(time_i) == 1 & epiweeks > 50] - 1
      # assign next year to last days of December if necessary
      epiyears[lubridate::month(time_i) == 12 & epiweeks < 3] <-
        epiyears[lubridate::month(time_i) == 12 & epiweeks < 3] + 1
      dates_tmp_i <- paste(epiyears, sprintf("%02d", epiweeks), sep = "-")
      coords_i <- unique(dates_tmp_i)
      if (dim_aggregation == "time") {
        dim(coords_i) <- c(sdate = 1, time = length(coords_i))
      } else {
        dim(coords_i) <- c(sdate = length(coords_i), time = 1)
      }
      idx <- which(dates_tmp_i[-1] !=
                     dates_tmp_i[-length(dates_tmp_i)]) + 1
      dates_i <- time_i[idx]
      paste(epiyears, sprintf("%02d", epiweeks), sep = "-")
    }

    # Assign time groups to data
    dims <- dim(data_tmp)
    agg_dim_idx <- which(names(dims) == dim_aggregation)
    fix_dim_idx <- which(names(dims) == dim_fix)
    index <- rep(list(quote(expr = )), length(dims))
    index[[fix_dim_idx]] <- i
    dims_i <- dims
    dims_i[fix_dim_idx] <- 1
    index[[agg_dim_idx]] <- which(!na_idx)

    data_tmp_i <- do.call(`[`, c(list(data_tmp), index, drop = FALSE))

    aggregated_data_i <- apply(data_tmp_i, seq(dims_i)[-agg_dim_idx],
                               function(x) {
                                 tapply(x, time_groups,
                                        .aggregate_temporally, fun)
                               })

    if (length(dims_i) == length(dim(aggregated_data_i))) {
      names(dim(aggregated_data_i)) <- c(dim_aggregation, 
                                         names(dims_i)[-agg_dim_idx])

      new_dims <- match(names(dims_i), names(dim(aggregated_data_i)))
      aggregated_data_i <- aperm(aggregated_data_i, new_dims)
    } else { # if there is only one time group
      aggregated_data_i <- s2dv::InsertDim(aggregated_data_i,
                                           posdim = agg_dim_idx,
                                           lendim = 1,
                                           name = dim_aggregation)
    }

    if (time_aggregation == "daily") {
      tmp <- .pad_array(aggregated_data_i, dates_i,
                        dim_aggregation = dim_aggregation)

      aggregated_data_i <- tmp$array
      dates_i <- tmp$dates
    }

    aggregated_data_i <- list(data = aggregated_data_i,
                              dims = dim(aggregated_data_i),
                              coords = data$coords,
                              attrs = data$attrs)

    if (time_aggregation == "daily") {
      aggregated_data_i$attrs$Dates <- as.POSIXct(dates_i, tz = "UTC")
    } else if (time_aggregation == "weekly") {
      aggregated_data_i$attrs$Dates <- as.Date(dates_i, format = "%Y-%m-%d")
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
    class(aggregated_data_i) <- "s2dv_cube"

    if (i == 1) {
      aggregated_data <- aggregated_data_i
      dates <- aggregated_data_i$attrs$Dates
    } else {
      aggregated_data <- CST_BindDim(x = list(aggregated_data,
                                              aggregated_data_i),
                                     along = dim_fix,
                                     var_dim = "var", dat_dim = "dataset")
      if (time_aggregation == "daily") {
        aggregated_data$attrs$Dates <- as.POSIXct(aggregated_data$attrs$Dates,
                                                  origin = "1970-01-01",
                                                  tz = "UTC")
      }
    }

    data <- aggregated_data
    data$coords[["time"]] <- seq((dim(data$attrs$Dates)["time"]))
    data$coords[["sdate"]] <- data$attrs$Dates[, 1]
  }

  data$dims <- dim(data$data)

  return(data)
}