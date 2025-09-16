#' .reduce_source_file
#'
#' Create a temporary netcdf file with reduced dimensions
#'
#' @param source_file String with the path to the source netCDF file.
#'
#' @return String with the path to the reduced temporary netCDF file.
#' @noRd
#'
#' @examples
#' TBD
.reduce_source_file <- function(source_file) {
  # replace the source file in the attributes of the s2dv_cube object
  nc <- ncdf4::nc_open(source_file)
  file_dims <- sapply(nc$dim, function(x) x$len)
  ncdf4::nc_close(nc)

  target_file <- source_file
  if (length(file_dims) > 4) {
    dims_to_reduce <- names(file_dims[!names(file_dims) %in%
                                        c("latitude", "longitude")])
    target_file <- paste0("tmp_", format(Sys.time(), "%Y%m%d%H%M"),
                          ".nc")

    system(paste0("ncwa -a ", paste(dims_to_reduce, collapse = ","), " ",
                  source_file, " ", target_file))
  }

  return(target_file)
}

#' .drop_dim
#'
#' @param data an array of data.
#' @param dim the dimension to be dropped (integer). It must have length 1.
#'
#' @return an array with the specified dimension dropped.
#' @noRd
#'
#' @examples
#' TBD
.drop_dim <- function(data, dim) {
  dims <- dim(data)
  if (dims[dim] != 1) stop("Cannot drop a dimension that has length > 1")

  index_list <- lapply(dims, function(d) quote(expr = ))
  index_list[[dim]] <- 1

  data <- do.call(`[`, c(list(data), index_list, list(drop = FALSE)))

  dim(data) <- dims[-dim]
  return(data)
}
