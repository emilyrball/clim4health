#' .get_array
#'
#' @param data An s2dv_cube or an array containing the data to be processed.
#'
#' @returns The same array or the data from the s2dv_cube.
#' @noRd
.get_array <- function(data) {
  if (inherits(data, "s2dv_cube")) {
    out <- data$data
  } else if (inherits(data, "array")) {
    out <- data
  }
  return(out)
}

#' .format_output
#'
#' @param data An array containing the data to format.
#' @param out_dims A named vector with the dimensions of the output.
#' @param out_coords A list with the coordinates of the output.
#' @param out_attrs A list with the attributes of the output.
#'
#' @returns The s2dv_cube from the array.
#' @noRd
.format_output <- function(data, out_dims, out_coords, out_attrs) {
  for (dim in names(out_dims)) {
    if (!(dim %in% names(dim(data)))) {
      data <- s2dv::InsertDim(
        data,
        posdim = which(names(out_dims) == dim),
        lendim = 1,
        name = dim
      )
    }
  }
  data <- s2dv::Reorder(data, order = names(out_dims))
  out <- list(
    data = data,
    dims = out_dims,
    coords = out_coords,
    attrs = out_attrs
  )
  class(out) <- "s2dv_cube"

  return(out)
}
