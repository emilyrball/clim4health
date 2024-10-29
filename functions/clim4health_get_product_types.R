#' Display Available Product types
#'
#' The `clim4health_get_product_types` function displays a list of
#' available product types for the selected dataset.
#'
#' @return Prints a list of available product types to the console.
#'
#' @examples
#' # Display the available product types
#' clim4health_get_product_types("reanalysis-era5-land-monthly-means")
#'
#' @export

source("dataset_info.R")

clim4health_get_product_types <- function(dataset) {

  datasets <- names(datasets_info)

  if (missing(dataset)) {
    stop("You need to specify which dataset you want to use. Available datasets are: ",
         paste(datasets, collapse = ", "))
  }

  if (!dataset %in% datasets) {
    stop("Provided dataset is not valid. Available datasets are: ",
         paste(datasets, collapse = ", "))
  }


  cat("Available climate product types are:\n")
  for (product_type in datasets_info[[dataset]][["product_types"]]) {
    cat("-", product_type, "\n")
  }
}

### END