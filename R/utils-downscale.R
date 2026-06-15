#' .reduce_source_file
#'
#' Create a temporary netcdf file with reduced dimensions
#'
#' @param source_file String with the path to the source netCDF file.
#'
#' @returns String with the path to the reduced temporary netCDF file.
#' @noRd
.reduce_source_file <- function(source_file) {
  # replace the source file in the attributes of the s2dv_cube object
  nc <- ncdf4::nc_open(source_file)
  file_dims <- sapply(nc$dim, function(x) x$len)
  ncdf4::nc_close(nc)

  target_file <- source_file
  if (length(file_dims) > 4) {
    dims_to_reduce <- names(file_dims[
      !names(file_dims) %in%
        c("latitude", "longitude", "lon", "lat")
    ])
    target_file <- paste0("tmp_", format(Sys.time(), "%Y%m%d%H%M"), ".nc")

    system(paste0(
      "ncwa -a ",
      paste(dims_to_reduce, collapse = ","),
      " ",
      source_file,
      " ",
      target_file
    ))
  }

  return(target_file)
}

#' .manage_dims
#'
#' @param data an array of data.
#' @param req_dims the ordered names of the required dimensions
#' (character vector).
#'
#' @returns an array with the specified dimensions.
#' @noRd
.manage_dims <- function(data, req_dims) {
  # CST_Interpolation returns list with reordered s2dv_cube objects
  if ("aux" %in% names(dim(data$data))) {
    dim <- which(names(dim(data$data)) == "aux")
    data$data <- .drop_dim(data$data, dim)
  }

  data <- .add_dim(data, req_dims)
  data$data <- s2dv::Reorder(data$data, order = req_dims)
  data$dims <- dim(data$data)

  # longitude circular sort should be preserved
  if ("longitude" %in% req_dims) {
    if (any(data$coords$longitude > 180)) {
      data$coords$longitude <- ifelse(
        data$coords$longitude > 180,
        data$coords$longitude - 360,
        data$coords$longitude
      )
    }
    data$coords$longitude <- round(data$coords$longitude, 4)
  }
  if ("latitude" %in% req_dims) {
    data$coords$latitude <- round(data$coords$latitude, 4)
  }
  return(data)
}

#' .add_dim
#'
#' @param data an array of data.
#' @param dim the dimension to be added (integer). It will have length 1.
#'
#' @returns an array with the specified dimension added.
#' @noRd
.add_dim <- function(data, exp_dims) {
  for (dim in exp_dims) {
    if (!dim %in% names(dim(data$data))) {
      data$data <- s2dv::InsertDim(
        data$data,
        posdim = 1,
        lendim = 1,
        name = dim
      )
    }
  }
  return(data)
}

#' .drop_dim
#'
#' @param data an array of data.
#' @param dim the dimension to be dropped (integer). It must have length 1.
#'
#' @returns an array with the specified dimension dropped.
#' @noRd
.drop_dim <- function(data, dim) {
  dims <- dim(data)
  if (dims[dim] != 1) {
    stop("Cannot drop a dimension that has length > 1")
  }

  index_list <- lapply(dims, function(d) quote(expr = ))
  index_list[[dim]] <- 1

  data <- do.call(`[`, c(list(data), index_list, list(drop = FALSE)))

  dim(data) <- dims[-dim]
  return(data)
}

#' .cdo_check
#'
#' Check if CDO command line operators are installed
#'
#' @returns NULL
#' @noRd
.cdo_check <- function() {
  cdo_path <- Sys.which("cdo")
  if (cdo_path == "") {
    stop(paste0(
      "CDO command line operators not found. ",
      "Please install CDO to use this function. Instructions can be ",
      "found in the clim4health installation tutorials."
    ))
  }
}
