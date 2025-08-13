#' Temporal aggregation of climate products.
#'
#' @param data s2dv_cube object with the climate data to be aggregated,
#' usually read using `clim4health_load`.
#' @param aggregation string indicating the type of temporal aggregations to be
#' performed. Options are "monthly", "yearly", "weekly", and "daily". Defaults
#' to "yearly".
#' @param agg_fun string specifying the aggregation function. Defaults to
#' "mean".
#' @param week_start for weekly aggregation, specify the starting weekday.
#' Current options include "Sunday" and "Monday".
#'
#' @returns 2dv_cube object with the temporally aggregated climate data.
#' @export
#'
#' @examples
#' # # Monthly aggregation of reanalysis data
#' # obs_path <- system.file("extdata/reanalysis/", package = "clim4health")
#' # obs_path <- paste0(obs_path, "/")
#' # obs <- clim4health_load(obs_path, var = "t2m", data_type = "obs",
#' #                          sdates = "201004", ext = 'nc')
#' # obs_month <- clim4health_time(obs, aggregation = "monthly")
#' # dim(obs_month$data)

clim4health_time <- function(data,
                             aggregation = "yearly",
                             agg_fun = "mean",
                             week_start = NULL) {
  # Check if aggregation type is valid
  if (!aggregation %in% c("monthly", "yearly", "weekly", "daily")) {
    stop("Aggregation must be 'daily', 'weekly', 'monthly' or 'yearly'")
  } else if ((aggregation == "weekly") && (is.null(week_start))) {
    stop(paste0("If aggregating to weekly, please provide 'week_start'",
                "parameter, the day on which your epiweek starts"))
  }
  # Sum or Mean
  if (agg_fun %in% c("mean", "sum", "max", "min")) {
    agg_fun <- get(agg_fun)
  } else {
    stop(paste0("Invalid choice of parameter 'agg_fun'. ",
                "Please select 'mean', 'sum', 'max' or 'min' to aggregate."))
  }

  data_tmp <- data$data
  time <- data$attrs$Dates

  if ("sdate" %in% names(dim(time))) {
    if (dim(time)["sdate"] == 1) {
      # case of one sdate

      # Parse time into appropriate groups
      time_groups <- if (aggregation == "yearly") {
        # Extract years
        dates <- unique(lubridate::year(time))
        lubridate::year(time)
      } else if (aggregation == "monthly") {
        # Extract year-month as groups
        #years <- lubridate::year(time)
        #month <- lubridate::month(time)
        #dates <- paste(years, sprintf("%02d", month), "01", sep = "-")
        #paste(years, sprintf("%02d", month), sep = "-")
        dates <- unique(as.POSIXct(paste(format(time, "%Y-%m"),
                                         "01", sep = "-")), tz = "UTC")
        paste(format(time, "%Y-%m"), "01", sep = "-")
      } else if (aggregation == "daily") {
        # Extract daily as groups
        dates <- unique(as.Date(time))
        as.Date(time)
      } else if (aggregation == "weekly") {
        if (week_start == "Sunday") {
          epiweeks <- lubridate::epiweek(time)
        } else if (week_start == "Monday") {
          epiweeks <- lubridate::isoweek(time)
        } else {
          stop(paste0("Please check 'week_start' parameter. Valid options are",
                      " 'Sunday' and 'Monday'."))
        }
        epiyears <- lubridate::year(time)
        # assign previous year to first days of January if necessary
        epiyears[lubridate::month(time) == 1 & epiweeks > 50] <-
          epiyears[lubridate::month(time) == 1 & epiweeks > 50] - 1
        # assign next year to last days of December if necessary
        epiyears[lubridate::month(time) == 12 & epiweeks < 3] <-
          epiyears[lubridate::month(time) == 12 & epiweeks < 3] + 1
        dates_tmp <- paste(epiyears, sprintf("%02d", epiweeks), sep = "-")
        coords <- unique(dates_tmp)
        idx <- which(dates_tmp[-1] != dates_tmp[-length(dates_tmp)]) + 1
        if (length(idx) == 0 || idx[1] != 1) {
          idx <- c(1, idx)
        }
        dates <- time[idx]
        paste(epiyears, sprintf("%02d", epiweeks), sep = "-")
      }

      # Assign time groups to data
      # Reshape the data into a long format for easier aggregation
      dims <- dim(data_tmp)
      date_dim <- which(names(dims) == "time")

      aggregated_data <- apply(data_tmp, seq(dims)[-date_dim],
                               function(x) {
                                            tapply(x, time_groups,
                                                   agg_fun, na.rm = TRUE)})

      if (length(dims) == length(dim(aggregated_data))) {
        names(dim(aggregated_data)) <- c("time", names(dims)[-date_dim])

        new_dims <- match(names(dims), names(dim(aggregated_data)))
        aggregated_data <- aperm(aggregated_data, new_dims)
      } else { # if there is only one time group
        aggregated_data <- s2dv::InsertDim(aggregated_data, posdim = date_dim,
                                           lendim = 1, name = "time")
      }

      data$data        <- aggregated_data
      data$coords$time <- dates
      data$attrs$original_dates <- data$attrs$Dates
      data$attrs$Dates <- dates
      if (aggregation == "weekly") {
        data$attrs$epiweeks <- coords
      }
      dim(data$attrs$Dates) <- c(sdate = 1, time = length(dates))
    } else {
      # case of multiple sdates
      aggregated_data <- NULL
      for (i in seq(dim(time)["sdate"])) {
        time_i <- time[i, ]
        na_idx <- is.na(time_i)
        time_i <- time_i[!is.na(time_i)] # remove NAs

        # Parse time into appropriate groups
        time_groups <- if (aggregation == "yearly") {
          # Extract years
          dates_i <- unique(lubridate::year(time_i))
          lubridate::year(time_i)
        } else if (aggregation == "monthly") {
          # Extract year-month as groups
          #years <- lubridate::year(time_i)
          #month <- lubridate::month(time_i)
          #dates_i <- unique(paste(years, sprintf("%02d", month),
          #                        "01", sep = "-"))
          #paste(years, sprintf("%02d", month), sep = "-")
          dates_i <- unique(as.POSIXct(paste(format(time_i, "%Y-%m"),
                            "01", sep = "-"), tz = "UTC"))
          paste(format(time_i, "%Y-%m"), "01", sep = "-")
        } else if (aggregation == "daily") {
          # Extract daily as groups
          #dates_i <- paste(unique(as.Date(time_i)), "00:00:00")
          dates_i <- unique(as.POSIXct(format(time_i, "%Y-%m-%d"), tz = "UTC"))
          as.POSIXct(format(time_i, "%Y-%m-%d"), tz = "UTC")
        } else if (aggregation == "weekly") {
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
          dim(coords_i) <- c(sdate = 1, time = length(coords_i))
          idx <- which(dates_tmp_i[-1] !=
                         dates_tmp_i[-length(dates_tmp_i)]) + 1
          dates_i <- time_i[idx]
          paste(epiyears, sprintf("%02d", epiweeks), sep = "-")
        }

        # Assign time groups to data
        dims <- dim(data_tmp)
        date_dim <- which(names(dims) == "time")
        sdate_dim <- which(names(dims) == "sdate")
        index <- rep(list(quote(expr = )), length(dims))
        index[[sdate_dim]] <- i
        dims_i <- dims
        dims_i[sdate_dim] <- 1
        index[[date_dim]] <- which(!na_idx)

        data_tmp_i <- do.call(`[`, c(list(data_tmp), index, drop = FALSE))

        aggregated_data_i <- apply(data_tmp_i, seq(dims_i)[-date_dim],
                                   function(x) {
                                     tapply(x, time_groups,
                                            function(y) {
                                              if (all(is.na(y))) NA else agg_fun(y, na.rm = TRUE)
                                            })
                                   })

        if (length(dims_i) == length(dim(aggregated_data_i))) {
          names(dim(aggregated_data_i)) <- c("time", names(dims_i)[-date_dim])

          new_dims <- match(names(dims_i), names(dim(aggregated_data_i)))
          aggregated_data_i <- aperm(aggregated_data_i, new_dims)
        } else { # if there is only one time group
          aggregated_data_i <- s2dv::InsertDim(aggregated_data_i,
                                               posdim = date_dim,
                                               lendim = 1, name = "time")
        }
        aggregated_data_i <- list(data = aggregated_data_i,
                                  dims = dim(aggregated_data_i),
                                  coords = data$coords,
                                  attrs = data$attrs)
        #aggregated_data_i$coords$sdate <- i
        #aggregated_data_i$coords$time <- seq_along(dates_i)
        if (aggregation == "daily") {
          aggregated_data_i$attrs$Dates <- as.Date(dates_i)
        } else {
          aggregated_data_i$attrs$Dates <- dates_i
        }
        dim(aggregated_data_i$attrs$Dates) <- c(sdate = 1,
                                                time = length(dates_i))
        class(aggregated_data_i) <- "s2dv_cube"
        if (i == 1) {
          aggregated_data <- aggregated_data_i
          dates <- aggregated_data_i$attrs$Dates
        } else {
          aggregated_data <- CST_BindDim(x = list(aggregated_data,
                                                  aggregated_data_i),
                                         along = "sdate",
                                         var_dim = "var", dat_dim = "dataset")
          if (aggregation == "daily") {
            aggregated_data$attrs$Dates <-
              as.POSIXct(aggregated_data$attrs$Dates,
                         origin = "1970-01-01", tz = "UTC")
          }
        }
      }

      #data$data        <- aggregated_data
      #names(dim(data$data)) <- names(dims)
      #data$coords$time <- dates
      #data$attrs$original_dates <- data$attrs$Dates
      #data$attrs$Dates <- dates
      #if (aggregation == "weekly") {
      #  data$attrs$epiweeks <- coords
      #}
      data <- aggregated_data
      data$coords$time <- seq((dim(data$attrs$Dates)["time"]))
      data$coords$sdate <- data$attrs$Dates[, 1]
    }
  }
  data$dims <- dim(data$data)


  return(data)
}