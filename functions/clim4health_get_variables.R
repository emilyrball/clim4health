#' Display Available Variables
#'
#' The `clim4health_get_variables` function displays a list of
#' available variables for the selected dataset.
#'
#' @return Prints a list of available variables to the console.
#'
#' @examples
#' # Display the available variables
#' clim4health_get_variables("reanalysis-era5-land-monthly-means")
#'
#' @export

source("dataset_info.R")

clim4health_get_variables <- function(dataset) {

  datasets <- names(datasets_info)

  if (missing(dataset)) {
    stop("You need to specify which dataset you want to use. Available datasets are: ",
         paste(datasets, collapse = ", "))
  }

  if (!dataset %in% datasets) {
    stop("Provided dataset is not valid. Available datasets are: ",
         paste(datasets, collapse = ", "))
  }


  cat("Available climate variables are:\n")
  for (variable in datasets_info[[dataset]][["variables"]]) {
    cat("-", variable, "\n")
  }
}

### END