
data= f.table.filter

var = "values"

time= "time"

lag= c(1,2)

area = c("ensemble", "latitude", "longitude")

merge=TRUE


lag_cov_group <- function(data = NULL,
                          var = NULL,
                          time = NULL,
                          lag = NULL,
                          area = NULL,
                          merge = FALSE) {

  # Validate required arguments
  args <- match.call()
  required_args <- c("data", "var", "time", "lag")
  missing_args <- setdiff(required_args, names(args))
  if (length(missing_args) > 0) {
    stop(paste("Error: Missing required argument(s):", paste(missing_args, collapse = ", ")))
  }

  # Check that 'var' and 'time' are present in the dataset
  if (!all(c(var, time) %in% names(data))) {
    stop("One or more 'var' or 'time' not found in the dataset")
  }

  # Check 'time' column is free of missing values
  if (any(is.na(data[[time]]))) {
    stop("Ensure 'time' column contains no missing values.")
  }

  # Validate the 'lag' argument
  if (!length(lag) %in% c(1, 2)) {
    stop("'lag' argument must be a vector of 1 or 2 elements")
  }
  if (length(lag) == 1) {
    lag <- rep(lag, 2)
  }

  # A) Handle single time series (no spatial units)
  if (is.null(area)) {
    if ((length(unique(data[[time]])) != nrow(data))) {
      stop("Duplicated time units detected. Consider using the 'area' argument.")
    }
    if (check_consecutive(data = data, time = time) == FALSE) {
      stop("Non-consecutive time series detected. Check for gaps or missing time points.")
    }

    # Sort and process
    data <- data[order(data[[time]]), ]
    data_to_lag <- data[, c(time, var)]
    lagged_data <- data_to_lag

    for (v in var) {
      for (i in lag[1]:lag[2]) {
        lagged_data[[paste0(v, ".l", i)]] <- c(rep(NA, i), data[[v]][1:(nrow(data) - i)])
      }
    }

    out <- lagged_data %>%
      dplyr::select(-all_of(var)) %>%
      dplyr::arrange(time)

    if (merge) {
      out <- data %>% dplyr::left_join(out, by = time)
    }
    
    return(out)

  } else if (!is.null(area)) {
    # Sort data by multiple columns in 'area' and 'time'
    order_columns <- c(area, time)  # Combine area and time columns for sorting
    data <- data[do.call(order, lapply(order_columns, function(col) data[[col]])), ]

    # Split the data by area
    groups <- split(data, f = data[, area])

   # Generate lagged variables (1 new column per variable and lag) per 'area'
    lagged_data <- NULL

    for (v in var) {
      lagged_data[[v]] <- lapply(X = groups, FUN = function(x) {
        for (i in lag[1]:lag[2]) {
          x[[paste0(v, ".l", i)]] <- c(
            rep(NA, i),
            x[[v]]
          )[1:length(x[[v]])]
        }
        return(x)
      })
    }

     lagged_data <- list()

    for (v in var) {
      lagged_data[[v]] <- lapply(groups, function(group_data) {
        for (i in lag[1]:lag[2]) {
          # Ensure that the indices don't go out of bounds
          valid_rows <- max(1, nrow(group_data) - i)  # Ensure the number of rows is valid
          group_data[[paste0(v, ".l", i)]] <- c(rep(NA, i), group_data[[v]][1:valid_rows])
        }
        return(group_data)
      })
    }


    # Combine the list of lagged data
    lagged_data_combined <- lapply(lagged_data, function(group_list) {
      do.call(rbind, group_list)
    })

    # Merge the data frames for all variables into a single data frame
    out <- Reduce(function(x, y) merge(x, y, by = c(area, time), all = TRUE), lagged_data_combined)

    # Remove original var columns
    out <- out %>% dplyr::select(-all_of(var)) %>%
      dplyr::arrange(!!!syms(area), time)

    if (merge) {
      out <- data %>% dplyr::left_join(out, by = c(area, time))
    }

    return(out)
  }
}
