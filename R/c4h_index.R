#' Apply a threshold-based mask to climate data
#'
#' @param data s2dv_cube object with the climate data to be masked, usually
#' read using `c4h_load`.
#' @param return_mask Boolean. If TRUE (default), return original values where
#' the condition is fulfilled and NA otherwise. If FALSE, return 1 where the
#' condition is met and 0 otherwise.
#' @param lower_threshold Numeric. Optional lower threshold of the mask. All
#' cells below that value will be masked out.
#' @param upper_threshold Numeric. Optional upper threshold of the mask. All
#' cells above that value will be masked out.
#' @param closed Boolean. If TRUE (default), closed intervals including
#' the threshold values. If TRUE, itnervals are open.
#'
#' @returns A s2dv_cube with the values masked or the mask (depending on
#' `return_mask`).
#' @export
#'
#' @examples
#' # Read reanalysis data
#' obs_path <- system.file("extdata", "reanalysis", package = "clim4health")
#' obs <- c4h_load(obs_path,
#'                 variable = "t2m",
#'                 year = 2010,
#'                 month = 1,
#'                 ext = "nc")
#' c4h_plot(obs)
#'
#' # Compute index, temperature higher than 290K
#' index <- c4h_index(obs, lower_threshold = 290)
#' c4h_plot(index)
#'
#' # Return mask, temperature higher than 290K
#' mask <- c4h_index(obs, return_mask = TRUE, lower_threshold = 290)
#' c4h_plot(mask)

c4h_index <- function(data,
                      return_mask = FALSE,
                      lower_threshold = NULL,
                      upper_threshold = NULL,
                      closed = TRUE) {

  # Retrieve data
  qube <- data$data

  # Define operators
  lower_op <- if (is.null(upper_threshold)) {
    function(x, y) TRUE
  } else if (isTRUE(closed)) {
    `<=`
  } else {
    `<`
  }
  greater_op <- if (is.null(lower_threshold)) {
    function(x, y) TRUE
  } else if (isTRUE(closed)) {
    `>=`
  } else {
    `>`
  }
  # Modify the values in the indicated rangelower_threshold
  qube_modified <- greater_op(qube, lower_threshold) &
    lower_op(qube, upper_threshold)
  storage.mode(qube_modified) <- "integer"

  # if return_mask == TRUE, return original values where the condition is met
  # and NA where it is not; else, return the actual mask
  if (isTRUE(return_mask)) {
    qube <- qube_modified
  } else {
    qube[which(qube_modified == 0)] <- NA
  }

  # metadata
  out <- data
  out$data <- qube
  out$attrs$index$return_mask <- return_mask
  out$attrs$index$lower_threshold <- paste0(ifelse(closed, ">=", ">"),
                                            lower_threshold)
  out$attrs$index$upper_threshold <- paste0(ifelse(closed, "<=", "<"),
                                            upper_threshold)

  return(out)
}