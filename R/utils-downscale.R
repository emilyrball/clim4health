
#' Determine spatial type
#'
#' @param data TBD
#'
#' @returns TBD
#'
#' @examples
#' # TBD

.determine_spatial_type <- function(data) {
  if (any(names(dim(data)) == "latitude") &&
        any(names(dim(data)) == "longitude")) {
    output <- "grid"
  } else if (any(names(dim(data)) == "region")) {
    output <- "region"
  } else {
    stop("ref must have either latitude and longitude or region dimensions")
  }
  return(output)
}


#' Reduce netcdf file dimensions
#'
#' @param source_file String with the path to the source netCDF file.
#'
#' @return String with the path to the reduced temporary netCDF file.
#' @export
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