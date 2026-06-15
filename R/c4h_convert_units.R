#' c4h_convert_units
#'
#' @param data An *s2dv_cube* object containing the data to be converted.
#' @param to A character string specifying the target units to convert to. If
#' NULL, no conversion will be performed and the current units will be
#' displayed. The default is NULL.
#' @param var A character string specifying the variable to convert.
#' If NULL, all variables will be converted. The default is NULL.
#' @param from A character string specifying the current units of the variable.
#' If NULL, the current units will be inferred from the data. The default is
#' NULL.
#'
#' @return An *s2dv_cube* object with the converted units.
#' @export
#'
#' @examples
#' # Forecast data included in the package
#' fcst_path <- system.file("extdata/forecast/", package = "clim4health")
#' fcst_path <- paste0(fcst_path, "/")
#'
#' # Load the forecast data
#' fcst <- c4h_load(fcst_path,
#'                  var = "t2m",
#'                  year = 2025,
#'                  month = 1,
#'                  ext = 'nc')
#'
#' # Display the current units
#' c4h_convert_units(fcst, var = "t2m")
#'
#' # Convert from Kelvin to Celsius
#' fcst_celsius <- c4h_convert_units(fcst,
#'                                   to = "celsius",
#'                                   var = "t2m",
#'                                   from = "K")
c4h_convert_units <- function(data, to = NULL, var = NULL, from = NULL) {
  # Check class type
  if (!inherits(data, "s2dv_cube")) {
    stop("'c4h_convert_units' only accepts s2dv_cube objects.")
  }

  # Check existing relevant var
  if (
    !is.null(var) &&
      !is.null(data$attrs$Variable$varName) &&
      !all(var %in% data$attrs$Variable$varName)
  ) {
    stop(
      "'data' does not contain the specified variable. Variables are: ",
      paste(data$attrs$Variable$varName, collapse = ", ")
    )
  } else if (
    !all(is.null(var)) &&
      !is.null(data$attrs$Variable$varName) &&
      any(var %in% data$attrs$Variable$varName)
  ) {
    var_idx <- which(data$attrs$Variable$varName %in% var)
  } else {
    var_idx <- seq_len(data$dims["var"])
  }

  # If 'to' is NULL, display current units and exit
  if (is.null(to)) {
    current_units <- data$attrs$Variable$metadata$units[var_idx]
    result <- paste0(
      "'",
      data$attrs$Variable$varName[var_idx],
      "': ",
      current_units
    )
    message(paste0(
      "The current variable units are ",
      paste(result, collapse = ", "),
      "."
    ))
    return(invisible(NULL))
  } else {
    if (length(to) == 1 && length(var_idx) > 1) {
      to <- rep(to, length(var_idx))
    } else if (length(to) != length(var_idx)) {
      stop(
        "'to' must be of length 1 or equal to the number of variables ",
        "being converted."
      )
    }
  }

  # Check valid from
  if (!is.null(from)) {
    for (idx in seq_along(var_idx)) {
      if (!is.null(data$attrs$Variable$metadata$units[[var_idx[idx]]])) {
        if (!any(data$attrs$Variable$metadata$units[[idx]] %in% from)) {
          stop(paste0(
            "The units of 'from' do not match the variable",
            " units in 'data' (",
            data$attrs$Variable$metadata$units[[var_idx[idx]]],
            ")."
          ))
        }
      } else {
        warning(paste0(
          "The variable '",
          data$attrs$Variable$varName[var_idx[idx]],
          "' in 'data' does not have units metadata. ",
          "Proceeding with the conversion assuming the ",
          "specified 'from' units are correct."
        ))
      }
    }
  } else {
    from <- data$attrs$Variable$metadata$units[var_idx]
  }
  ## stop if not valid units
  #if (!all(udunits2::ud.is.parseable(to))) {
  #  invalid <- to[!udunits2::ud.is.parseable(to)]
  #  stop(paste0("The specified 'to' units (", invalid, ") are not valid."))
  #} else if (!(all(udunits2::ud.is.parseable(from)))) {
  #  invalid <- from[!udunits2::ud.is.parseable(from)]
  #  stop(paste0("The specified 'from' units (", invalid, ") are not valid."))
  #}
  convertible <- apply(rbind(from, to), 2, function(x) {
    units::ud_are_convertible(x[1], x[2])
  })
  if (!all(convertible)) {
    non_convertible <- paste0(from[!convertible], " to ", to[!convertible])
    stop(paste0(
      "The specified units are not convertible: ",
      paste(non_convertible, collapse = "; ")
    ))
  }

  # Convert units
  data_out <- data$data
  for (idx in seq_along(var_idx)) {
    s <- rep(list(TRUE), length(dim(data$data)))
    s[[2]] <- var_idx[idx]

    data_out <- do.call(
      `[<-`,
      c(
        list(data_out),
        s,
        list(
          value = units::ud_convert(
            .subset_array_named(data$data, var = var_idx[idx]),
            from = from[idx],
            to = to[idx]
          )
        )
      )
    )
  }

  # Update var units
  if (!is.null(data$attrs$Variable$metadata$units)) {
    data$attrs$Variable$metadata$units[var_idx] <- to
  } else {
    data$attrs$Variable$metadata$units <-
      stats::setNames(to, data$attrs$Variable$varName[var_idx])
  }

  # Replace data
  data$data <- data_out

  return(data)
}
