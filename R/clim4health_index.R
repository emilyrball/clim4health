#' Masking climate data based on their value
#'
#' @param data s2dv_cube object with the climate data to be masked, usually 
#' read using `clim4health_load`.
#' @param mod_vals Boolean. If TRUE (default), return original values where the 
#' condition is fulfilled and NA otherwise. If FALSE, return 1 where the 
#' condition is met and 0 otherwise.
#' @param lower_threshold Numeric. Lower threshold of the mask.
#' @param upper_threshold Numeric. Upper threshold of the mask.
#' @param lower_close Boolean. If FALSE (default), open lower interval. If TRUE,
#' it is closed.
#' @param upper_close Boolean. If FALSE (default), open upper interval. If TRUE,
#' it is closed.
#'
#' @returns A s2dv_cube with the values masked or the mask (depending on `mod_vals`).
#' @export
#' 
#' @examples
#' # Load forecast data included in the package
#' fcst_path <- system.file("extdata/forecast/", package = "clim4health")
#' fcst_path <- paste0(fcst_path, "/")
#' fcst <- clim4health_load(fcst_path, var = "t2m", data_type = "fcst",
#'                          sdates = "20240401", ext = 'nc')
#'                          
#' # Mask values
#' mask <- clim4health_index(fcst, mod_vals = TRUE,  
#'                           lower_threshold = 280, upper_threshold = 290,
#'                           lower_close = FALSE, upper_close = FALSE)

clim4health_index <- function (data,
                               mod_vals = TRUE,  
                               lower_threshold = NULL, 
                               upper_threshold = NULL,
                               lower_close = FALSE, 
                               upper_close = FALSE) {
  
  qube <- data$data
  
  # Define operators
  greater_op <- if (lower_close) `>=` else `>`
  lower_op <- if (upper_close) `<=` else `<`

  # Modify the values in the indicated range
  qube_modified <- as.integer(greater_op(qube, lower_threshold) &
                                lower_op(qube, upper_threshold))
  
  # if mod_vals == TRUE, return original values where the condition is fulfilled 
  # and NA where it is not 
  # else, return 1 where the condition is fulfieled and 0 where it is not
  if (mod_vals) {
    qube[which(qube_modified == 0)] <- NA
  } else {
    qube <- qube_modified
  }
  
  # metadata
  result <- data
  result$data <- qube
  result$attrs$index$mod_vals <- mod_vals
  result$attrs$index$lower_threshold <- paste0(ifelse(lower_close, '>=', '>'), lower_threshold)
  result$attrs$index$upper_threshold <- paste0(ifelse(upper_close, '<=', '<'), upper_threshold)
  
  return (result)
}