#' c4h_convert_units
#'
#' @param data an s2dv_cube object containing the data to be converted.
#' @param to a character string specifying the target units to convert to. If
#' NULL, no conversion will be performed and the current units will be
#' displayed. The default is NULL.
#' @param variable a character string specifying the variable to convert.
#' If NULL, the first variable will be used. The default is NULL.
#' @param from a character string specifying the current units of the variable.
#' If NULL, the current units will be inferred from the data. The default is
#' NULL.
#'
#' @return an s2dv_cube object with the converted units.
#' @export
#'
#' @examples
#' # Forecast data included in the package
#' fcst_path <- system.file("extdata/forecast/", package = "clim4health")
#' fcst_path <- paste0(fcst_path, "/")
#'
#' # Load the forecast data
#' fcst <- c4h_load(fcst_path,
#'                  variable = "t2m",
#'                  year = 2025,
#'                  month = 1,
#'                  ext = 'nc')
#'
#' # Display the current units
#' c4h_convert_units(fcst, variable = "t2m")
#'
#' # Convert from Kelvin to Celsius
#' fcst_celsius <- c4h_convert_units(fcst,
#'                                   to = "celsius",
#'                                   variable = "t2m",
#'                                   from = "K")
c4h_convert_units <- function(data,
                              to = NULL,
                              variable = NULL,
                              from = NULL) {

  # Check class type
  if (!inherits(data, "s2dv_cube")) {
    stop("'c4h_convert_units' only accepts s2dv_cube objects.")
  }

  # Check existing relevant variable
  if (!is.null(variable) && !is.null(data$attrs$Variable$varName) &&
        !any(variable %in% data$attrs$Variable$varName)) {
    stop("'data' does not contain the specified variable. Variables are: ",
         paste(data$attrs$Variable$varName, collapse = ", "))
  } else if (!all(is.null(variable)) && !is.null(data$attrs$Variable$varName) &&
               any(variable %in% data$attrs$Variable$varName)) {
    var_idx <- which(data$attrs$Variable$varName %in% variable)
  } else {
    var_idx <- 1
  }

  # If 'to' is NULL, display current units and exit
  if (is.null(to)) {
    current_units <- data$attrs$Variable$metadata$units[var_idx]
    message(paste(paste0("The current units of variable '",
                         data$attrs$Variable$varName[var_idx], "' are: ",
                         current_units), sep = "\n"))
    return(invisible(NULL))
  } else {
    if (length(to) == 1 && length(var_idx) > 1) {
      to <- rep(to, length(var_idx))
    } else if (length(to) != length(var_idx)) {
      stop("'to' must be of length 1 or equal to the number of variables ",
           "being converted.")
    }
  }

  # Check valid from
  if (!is.null(from)) {
    for (idx in seq_along(var_idx)) {
      if (!is.null(data$attrs$Variable$metadata$units[[var_idx[idx]]])) {
        if (!any(data$attrs$Variable$metadata$units[[idx]] %in% from)) {
          stop(paste0("The units of 'from' do not match the variable",
                      " units in 'data' (",
                      data$attrs$Variable$metadata$units[[var_idx[idx]]], ")."))
        }
      } else {
        warning(paste0("The variable '",
                       data$attrs$Variable$varName[var_idx[idx]],
                       "' in 'data' does not have units metadata. ",
                       "Proceeding with the conversion assuming the ",
                       "specified 'from' units are correct."))
      }
    }
  } else {
    from <- data$attrs$Variable$metadata$units[var_idx]
  }
  # stop if not valid units
  if (!all(udunits2::ud.is.parseable(to))) {
    invalid <- to[!udunits2::ud.is.parseable(to)]
    stop(paste0("The specified 'to' units (", invalid, ") are not valid."))
  } else if (!(all(udunits2::ud.is.parseable(from)))) {
    invalid <- from[!udunits2::ud.is.parseable(from)]
    stop(paste0("The specified 'from' units (", invalid, ") are not valid."))
  }

  # Convert units
  data_out <- data$data
  for (idx in seq_along(var_idx)) {
    data_out <- do.call(`[<-`, c(
      list(data_out),
      rep(list(TRUE), length(dim(data$data))) |>
        (\(s) {
          s[[2]] <- var_idx[idx]; s
        })(),
      list(value = udunits2::ud.convert(
        .subset_array_named(data$data, var = var_idx[idx]),
        u1 = from[idx],
        u2 = to[idx]
      ))
    ))
  }

  # Update variable units
  data$attrs$Variable$metadata$units[var_idx] <- to

  # Replace data
  data$data <- data_out

  return(data)
}