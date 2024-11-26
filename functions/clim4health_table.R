#' Convert s2dv_cube Data to a Well-Formatted Data Frame
#'
#' This function processes data in an `s2dv_cube` object into a data frame format,
#' assigning to each data value the appropriate factor levels from each dimension
#' using the `coords` metadata. 
#'
#' @param array An`s2dv_cube` object. This object has the form of a multidimensional array 
#' and must contain:
#'   \itemize{
#'     \item `data`: A multidimensional array containing the primary data.
#'     \item `dims`: A named list specifying the dimensions of the array.
#'     \item `coords`: A named list of coordinate values for each dimension.
#'   }
#' @return A data frame where:
#'   \itemize{
#'     \item Each dimension of the array is represented as a factor column.
#'     \item The `values` column contains the corresponding array values.
#'   }
#'   Factor levels for each dimension are set based on the `coords` metadata.
#' 
#' @example
#' path <- getwd()
#' fcst <- clim4health_load(paste0(path, '/sample_data/ecmwf51/forecast/t2m_20240401.nc'))        
#' class(fcst)
#' #[1] "s2dv_cube"
#' dim(fcst$data)
#' #  dataset       var      time  ensemble  latitude longitude
#' #        1         1         3        51        57        77
#' fcst$coords$time
#' # [1] "2024-04-01" "2024-05-01" "2024-06-01"
#' formatted.table <- clim4health_table(fcst)
#' head(formatted.table)
#' @export

clim4health_table <- function(array) {

 # Check if input data is an s2dv_cube multidimensional array object
  if (!("s2dv_cube" %in% class(array))) {
    stop("Input 'array' must be an object of class 's2dv_cube'.")
  }
  
  # Required fields in s2dv_cube
  required_fields <- c("data", "coords")
  
  # Check if required fields are present
  missing_fields <- setdiff(required_fields, names(array))

  if (length(missing_fields) > 0) {
    stop(paste("The following required fields are missing from the input 'array':",
               paste(missing_fields, collapse = ", ")))
  }
  
  # Validate the 'data' field
  if (!is.array(array$data)) {
    stop("'array$data' must be a multidimensional array.")
  }
  
  # Validate the 'coords' field
  if (!is.list(array$coords)) {
    stop("'array$coords' must be a list.")
  }
  
  # Ensure the names of the dimensions in 'data' match the names in 'coords'
  dim_names <- names(dim(array$data))
  if (is.null(dim_names)) {
    stop("The dimensions of 'array$data' must have names.")
  }
  
  missing_coords <- setdiff(dim_names, names(array$coords))
  if (length(missing_coords) > 0) {
    stop(paste("The following dimension names are missing in 'array$coords':",
               paste(missing_coords, collapse = ", ")))
  }
  
  # Ensure each coordinate in 'coords' matches the length of
  # its corresponding dimension in 'data'
  for (dim_name in dim_names) {
    if (length(array$coords[[dim_name]]) != dim(array$data)[[dim_name]]) {
      stop(paste("The length of 'array$coords[[", dim_name, "]]' does not match 
      the size of the '", dim_name, "' dimension in 'array$data'.", sep = ""))
    }
  }

  # Convert s2dv_cube data to a data frame with factor levels
  table.format <- as.data.frame.table(array$data)
  
  # Set the column names of table.format
  # Include dimension names and 'values' column
  names(table.format) <- c(names(dim(array$data)), 'values') 
  
  # Loop through each dimension in the s2dv_cube
  for (dim_name in names(dim(array$data))) {
    # Check if the column exists in table.format
    if (dim_name %in% colnames(table.format)) {
      # Update factor levels in each column using 
      # the corresponding s2dv_cube coords element
      levels(table.format[[dim_name]]) <- 
      array$coords[[dim_name]][1:length(array$coords[[dim_name]])]
    }
  }

  # Round values to 2 digits 
  table.format$values <- round(table.format$values, 2)
  
  # Return the updated data frame 
  return(table.format)
}
