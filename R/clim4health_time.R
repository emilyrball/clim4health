#' Temporal aggregation of climate products.
#'
#' @param data_clim s2dv_cube object with the climate data to be aggregated,
#' usually read using `clim4health_load`.
#' @param aggregation string indicating the type of temporal aggregations to be
#' performed. Options are "monthly", "yearly", and "daily". Defaults to "yearly".
#'
#' @returns 2dv_cube object with the temporally aggregated climate data.
#' @export
#' 
#' @examples
#' # Monthly aggregation of reanalysis data
#' obs_path <- system.file("extdata/reanalysis/", package = "clim4health")
#' obs_path <- paste0(obs_path, "/")
#' obs <- clim4health_load(obs_path, var = "t2m", data_type = "obs",
#'                          sdates = "201004", ext = 'nc')
#' obs_month <- clim4health_time(obs, aggregation = "monthly")
#' dim(obs_month$data)

clim4health_time <- function(data_clim,
                             aggregation = "yearly") {
    
  # Check if aggregation type is valid
  if (!aggregation %in% c("monthly", "yearly", "daily")) {
    stop("Aggregation must be 'daily', 'monthly' or 'yearly'")
  }

  perm_array <- aperm(data_clim$data, c(1, 2, 3, 4, 5, 7, 8, 9, 6))
  average_time_of_day <- rowMeans(perm_array, dims = 8)
  
  perm_array <- aperm(average_time_of_day, c(1, 2, 3, 4, 6, 7, 8, 5))
  average_day <- rowMeans(perm_array, dims = 7)
  
  perm_array <- aperm(average_day, c(1, 2, 3, 5, 6, 7, 4))
  average_month <- rowMeans(perm_array, dims = 6)
  
  perm_array <- aperm(average_month, c(1, 2, 4, 5, 6, 3))
  average_year <- rowMeans(perm_array, dims = 5)
  
  # Parse time into appropriate groups
  if (aggregation == "yearly") {
    data_clim$data <- average_year
  } else if (aggregation == "monthly") {
    data_clim$data <- average_month
  } else if (aggregation == "daily") {
    data_clim$data <- average_day
  }
  
  # data_clim$coords$time <- dates
  
  return(data_clim)
}
