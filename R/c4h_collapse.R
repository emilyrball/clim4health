#' Collapse one dimension of a s2dv_cube object
#'
#' @description
#' Collapse one dimension of the *s2dv_cube*. `c4h_collapse()` works
#' differently depending on the dimension being collapsed:
#' * Use dim = "ensemble" to compute a summary statistic across ensemble members
#' in forecast and hindcast datasets.
#' * Use dim = "time" to compute a time aggregation with observational data
#' (i.e. the length of the 'sdate' dimension must be one). In this case the
#' first "time" of the dataset is assigned as the new s2dv time coordinate.
#' To compute time aggregations when 'sdate' has length greater than one,
#' use the function [c4h_time] instead.
#' * Use dim = "sdate" to compute a climatology, if each 'sdate' corresponds to
#' a different year. In this case the first "sdate" of the dataset is assigned
#' as the new s2dv sdate coordinate.
#'
#' @param data *s2dv_cube* object with the climate data.
#' @param dim Dimension to collapse. Options include 'ensemble', 'sdate' and
#' 'time'.
#' @param fun Function to perform the aggregation, defaults to 'mean'. Other
#' options include 'median', 'min' and 'max'.
#' @param na.rm Logical, indicating whether NA values should be ignored when
#' applying the collapsing function. Defaults to TRUE.
#'
#' @returns An *s2dv_cube* with the collapsed dimension is kept with length 1.
#'
#' @seealso [c4h_time] to do time aggregations of length greater than one.
#' @export
#'
#' @examples
#' \donttest{
#' # Load a forecast
#' fcst <- c4h_load(system.file("extdata", "forecast", package = "clim4health"),
#'                  var = "t2m",
#'                  year = 2025,
#'                  month = 1,
#'                  ext = 'nc')
#' fcst$dims
#'
#' # Ensemble median of a forecast
#' fcst_coll <- c4h_collapse(fcst, dim = "ensemble", fun = "median")
#' fcst_coll$dims
#'
#' # Load an observational time-series dataset
#' stations_path <- system.file("extdata", "stations", package = "clim4health")
#' stations <- c4h_load(stations_path, var = "temp_mean", ext = "csv")
#' stations$dims
#'
#' # Compute time-mean (average temp conditions)
#' stations_coll <- c4h_collapse(stations, dim = "time")
#' stations_coll$dims
#'
#' # Load an observational dataset aligned to a hindcast
#' obs_path <- system.file("extdata", "reanalysis", package = "clim4health")
#' obs <- c4h_load(obs_path, var = "t2m", ext = "nc",
#'                 year = 2010:2012, month = 1, leadtime_month = 1:3)
#' obs$dims
#'
#' # Compute climatology (average over years)
#' obs_coll <- c4h_collapse(obs, dim = "sdate")
#' obs_coll$dims
#' }
c4h_collapse <- function(data, dim, fun = "mean", na.rm = TRUE) {
  # Input checks ----

  # Check class type
  if (!inherits(data, "s2dv_cube")) {
    stop("'c4h_collapse' only accepts s2dv_cube objects.")
  }

  # Check collapsing dimension
  if (missing(dim) || !is.character(dim) || length(dim) != 1 || is.na(dim)) {
    stop("'dim' must be a single character string.")
  }
  if (!dim %in% c("ensemble", "time", "sdate")) {
    stop(
      "c4h_collapse can only collapse the 'ensemble', 'sdate' ",
      "and 'time' dimensions."
    )
  } else if (!dim %in% names(data$dims)) {
    stop(paste0("The data doesn't have a ", dim, " dimension."))
  } else if (data$dims[dim] <= 1) {
    stop(paste0("The size of the dimension ", dim, " must be larger than one."))
  }
  if (
    dim == "time" && "sdate" %in% names(data$dims) && data$dims[["sdate"]] != 1
  ) {
    stop(
      "The 'sdate' dimension must have length one to collapse 'time'."
    )
  }

  # Check valid fun
  if (!is.character(fun) || length(fun) != 1 || is.na(fun)) {
    stop("'fun' must be a single character string.")
  }
  if (!fun %in% c("mean", "median", "min", "max")) {
    stop("Valid 'fun' include 'mean', 'median', 'min' and 'max'.")
  }

  # Check that the data array is consistent with the 'dims' metadata, since
  # the margins to aggregate are derived from the latter
  if (!identical(as.integer(unname(data$dims)), as.integer(dim(data$data)))) {
    stop("The dimensions of 'data$data' do not match 'data$dims'.")
  }
  if (is.null(names(dim(data$data)))) {
    names(dim(data$data)) <- names(data$dims)
  } else if (!identical(names(dim(data$data)), names(data$dims))) {
    stop("The dimension names of 'data$data' do not match 'data$dims'.")
  }

  # Aggregation ----

  # Start from a copy of the input so that any extra fields are preserved
  out <- data

  # Collapse the data along `dim`, keeping it with length one
  keep_margins <- which(names(data$dims) != dim)
  out$data <- .apply_keepdim(data$data, keep_margins, fun, na.rm = na.rm)

  # Update the metadata
  out$dims[dim] <- 1
  out$coords[dim] <- 1
  attributes(out$coords[[dim]]) <- attributes(data$coords[[dim]])

  # Update the Dates attribute
  if (dim == "time") {
    out$attrs$Dates <- data$attrs$Dates[, 1, drop = FALSE]
    dim(out$attrs$Dates) <- c(sdate = out$dims[["sdate"]], time = 1)
  } else if (dim == "sdate") {
    out$attrs$Dates <- data$attrs$Dates[1, , drop = FALSE]
    dim(out$attrs$Dates) <- c(sdate = 1, time = out$dims[["time"]])
  }

  out
}


#' Aggregate a multidimensional array while keeping the aggregate dimension
#'
#' @param data an array to be aggregated.
#' @param margins dimensions to keep.
#' @param fun function to apply.
#' @param na.rm logical, indicating whether NA values should be ignored.
#'
#' @returns The collapsed array with the same number of dimensions as the input.
#' @noRd
.apply_keepdim <- function(data, margins, fun, na.rm) {
  # Return NA (instead of NaN, Inf or -Inf plus a warning) when all values
  # are missing, so that all supported functions behave consistently
  fun <- match.fun(fun)
  safe_fun <- function(x, na.rm) {
    if (na.rm && all(is.na(x))) {
      return(NA_real_)
    }
    fun(x, na.rm = na.rm)
  }
  out <- apply(data, margins, safe_fun, na.rm = na.rm)
  d <- dim(data)
  newdim <- rep(1, length(d))
  names(newdim) <- names(d)
  newdim[margins] <- dim(data)[margins]
  array(out, dim = newdim)
}
