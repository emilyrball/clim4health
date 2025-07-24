#' Title
#'
#' @param data TBD 
#'
#' @returns TBD
#'
#' @examples
#' # TBD

.get_array <- function(data) {
  if (class(data) == "s2dv_cube") {
    return(data$data)
  } else if (class(data) == "array") {
    return(data)
  } else {
    stop("data must be an s2dv_cube or an array")
  }
}
