#Dependencies
# library(lubridate)
# library(dplyr)

# Function for Aggregation
clim4health_time <- function(data_clim, aggregation = "yearly") {
    
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
  time_groups <- if (aggregation == "yearly") {
    data_clim$data <- average_year
  } else if (aggregation == "monthly") {
    data_clim$data <- average_month
  } else if (aggregation == "daily") {
    data_clim$data <- average_day
  }
  
  # data_clim$coords$time <- dates
  
  return(data_clim)
}
