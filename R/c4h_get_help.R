#' Display Available Climate Datasets, parameters and variables
#'
#' The `c4h_get_help()` function displays a list of available climate
#' datasets that can be usedfor health and climate research, specifically
#' using ERA5 reanalysis datasets.
#'
#' @param dataset If dataset is provided, then the list of required parameters
#' for that dataset will be returned. If no dataset is provided, a list of
#' available datasets will be returned.
#' @param parameter If parameter is provided, a list with all possible options
#' for that parameter is returned. If no parameter is provided then then the
#' list of required parameters for the given dataset will be returned
#' @returns Prints a list of either available climate datasets, required
#' parameters or options to the console.
#'
#' @examples
#' c4h_get_help()
#' c4h_get_help("reanalysis-era5-land")
#' c4h_get_help("reanalysis-era5-land", "product_type")
#' c4h_get_help("reanalysis-era5-land", "variable")
#' @export

c4h_get_help <- function(dataset, parameter) {
  
  # Retrieve the long list with the catalogue
  datasets <- names(.datasets_info)

  # if dataset is missing, return list of available datasets
  if (missing(dataset)) {
    message("Available climate datasets:")
    for (d in datasets) {
      cat("-", d, "\n")
    }
    return(invisible(NULL))
  }

  # if dataset is wrong, return lsit of available datasets
  if (!dataset %in% datasets) {
    stop("Incorrect dataset, available climate datasets are:\n",
         paste("-", datasets, collapse = "\n"))
  }

  # get required parameters for selected dataset
  required_params <- names(.datasets_info[[dataset]])

  # if dataset correct and parameter missing, return list of parameters
  if (dataset %in% datasets && missing(parameter)) {
    message("Required parameters:")
    for (p in required_params) {
      cat("-", p, "\n")
    }
    return(invisible(NULL))
  }

  # if dataset correct and parameter missing, return list of parameters
  if (dataset %in% datasets && !parameter %in% required_params) {
    stop("Incorrect parameter. Required parameters for ", dataset, ":\n",
         paste("-", required_params, collapse = "\n"))
  }

  # get options for parameters
  options <- .datasets_info[[dataset]][[parameter]]

  cat(paste0("Possible options for ", parameter, ":\n"))
  for (o in options) {
    cat("-", o, "\n")
  }
}

### END