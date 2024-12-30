#' lag_cov
#'
#' @param data An object of class 'data.frame' containing equally spaced
#'             time series data (daily, weekly, monthly data) for one or multiple spatial units or groups.
#'             The data must include columns for the temporal dimension and the variables to be lagged.
#' @param var  A character string specifying the names of variables for which lagged values
#'   will be calculated.
#' @param time A character string specifying the name of the column representing the
#'   temporal dimension in the dataset (e.g., "date").
#' @param lag A numeric vector of length 1 or 2. If a single value is provided, a single lag
#'   is calculated. If two values are provided, they define the range (minimum and maximum)
#'   of lags to compute.
#' @param area A character string specifying the name of the column representing spatial
#'   units (e.g., region or location). If `NULL`, the entire dataset is treated as a single
#'   time series.
#' @param merge @param merge A logical value indicating whether to merge the lagged values with the
#'   original dataset. If `TRUE`, the output includes both original and lagged variables;
#'   if `FALSE`, only the lagged variables are returned.
#' @return A `data.frame` containing lagged variables. If `merge = TRUE`, the output also
#'   includes the original dataset.
#'
#'   #' @details
#' The function first verifies that all required arguments are provided and checks that
#' the specified columns (`var`, `time`) exist in the dataset. It ensures the time column
#' is in a valid format (e.g., `Date`) and that time series are consecutive. For datasets
#' with multiple spatial units, the function computes lagged values separately for each unit.
#'
#' If `lag` is a vector of two elements, the function generates multiple lagged variables
#' for each specified lag within the range. Lagged variables are named using the format
#' `variable_name.lN`, where `N` is the lag value.
#'
#' @export
#'
#'
#' @examples
#' # Example usage with hypothetical dengue data
#' data(dengue_data)
#' data_lagged <- lag_cov(
#'   data = dengue_data, time = "date", area = "micro_code",
#'   var = c("tmin", "tmax"), lag = 3
#' )
#'

lag_cov <- function(data = NULL,
                    var = NULL,
                    time = NULL,
                    lag = NULL,
                    area = NULL,
                    merge = FALSE) {

  # Validate required arguments

  args <- match.call()

  # Check if required arguments are missing
  required_args <- c("data", "var", "time", "lag")
  missing_args <- setdiff(required_args, names(args))
  if (length(missing_args) > 0) {
    stop(paste("Error: Missing required argument(s):", paste(missing_args,
      collapse = ","
    )))
  }

  # Check that 'var' and 'time' are present in the dataset
  if ( !all(c(var,time) %in% names(data))) {
    stop("One or more 'var' or 'time' not found in the dataset")
  }

  # Check 'time' column is free of missing values
  if(any(is.na(data[[time]]))) {
    stop("Ensure 'time' column contains no missing values.")
  }

  # Validate the 'lag' argument
  if (!length(lag) %in% c(1,2)) {
    stop("'lag' argument must be a vector of 1 or 2 elements")
  }

  if (length(lag) == 1) {
    lag <- rep (lag, 2)
  } else {lag <- lag}


  # A) Handle single time series (no spatial units)
  if (is.null(area)) {
    # Check for duplicate time entries
    if ((length(unique(data[[time]])) != nrow(data))) {
      stop("Duplicated time units detected. Consider using the 'area' argument.")
    }
    # Check for non-consecutive time entries
    if(check_consecutive(data = data, time = time) == FALSE) {
      stop("Non-consecutive time series detected. Check for gaps or missing time points.")
    }

    # Sort data by time
    data_to_lag <- data[order(data[[time]]), ]

    # Select columns of interest
    data_to_lag <- data_to_lag[, c(time, var)]

    # Generate lagged variables (1 new column per variable and lag)
    lagged_data <- data_to_lag

    for (v in var) {
      for (i in lag[1]:lag[2]) {
        lagged_data[[paste0(v, ".l", i)]] <- c(rep(NA, i), data[[v]][1:(length(data[[v]]) - i)])
      }
    }

    # Return the lagged data
    out <- lagged_data

    # Remove original variables
    out <- out %>%
      dplyr::select(-({
        var
      })) %>%
      dplyr::arrange(time)

    # Optionally merge lagged data with the original dataset
    if (merge == TRUE) {
      out <- data %>% dplyr::left_join(out, by = {time})
    } else {
      out <- out
    }

    # B) Handle grouped time series (with spatial units)
  } else if (!is.null(area) && area %in% names(data)) {

    if(check_consecutive(data = data, time = time, area = area) == FALSE) {
      stop("Non-consecutive time series detected. Check for gaps or missing time points.")
    }

    # Sort data by area and time
    data <- data[order(data[[area]], data[[time]]), ]

    # Split the data by area
    groups <- split(data[, c(area, time, var)], data[[area]])

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

    # Recombine all groups with lagged values for each variable into a single data frame
    within_var <- lapply(lagged_data, FUN = function(x) {
      do.call(rbind, x)
    })

    # Merge the data frames for all variables into a single data frame
    out <- Reduce(
      function(x, y) merge(x, y, by = c(area, time, var), all = TRUE),
      within_var
    )

    # Remove original var
    out <- out %>%
      dplyr::select(-({
        var
      })) %>%
      dplyr::arrange(area, time)

    # Optionally merge lagged data with the original dataset
    if (merge == TRUE) {
      out <- data %>% dplyr::left_join(out, by = c(
        {
          area
        },
        {
          time
        }
      ))
    } else {
      out <- out
    }

  }


  # Return output
  return(out)
}
