#' Title
#'
#' @param data An s2dv_cube or an array containing the data to be processed.
#'
#' @returns The same array or the data from the s2dv_cube.
#'
#' @examples
#' # TBD
#' @noRd

.get_array <- function(data) {
  if (inherits(data, "s2dv_cube")) {
    out <- data$data
  } else if (inherits(data, "array")) {
    out <- data
  } else {
    stop("data must be an s2dv_cube or an array")
  }
  return(out)
}
