#Dependencies
# library(lubridate)
# library(dplyr)

# Function for Aggregation
clim4health_time <- function(data_clim, aggregation = "yearly") {

  data <- data_clim$data
  time <- data_clim$coords$time
    
  # Check if aggregation type is valid
  if (!aggregation %in% c("monthly", "yearly", "daily")) {
    stop("Aggregation must be 'daily', 'monthly' or 'yearly'")
  }
  
  # Parse time into appropriate groups
  time_groups <- if (aggregation == "yearly") {
    dates <- unique(format(data_clim$coords$time, "%Y"))
    year(time)  # Extract years
  } else if (aggregation == "monthly") {
    dates <- unique(format(time, "%Y-%m"))
    format(time, "%Y-%m")  # Extract year-month as groups
  } else if (aggregation == "daily") {
    dates <- unique(format(time, "%Y-%m-%d"))
    format(time, "%Y-%m-%d")  # Extract daily as groups
  }
  
  # Assign time groups to data
  # Reshape the data into a long format for easier aggregation
  dims <- dim(data)
  reshaped_data <- array(data, dim = c(prod(dims[1:2]), dims[3], dims[4], dims[5], dims[6]))
    
  # Aggregate along the time dimension (dimension 3)
  aggregated_data <- apply(reshaped_data, c(1, 3, 4, 5), function(x) tapply(x, time_groups, mean))
                           
  # Reshape back into the required format
  final_dims <- c(dims[1:2], length(unique(time_groups)), dims[4], dims[5], dims[6])
  aggregated_data <- array(unlist(aggregated_data), dim = final_dims)
                           
  data_clim$data        <- aggregated_data
  data_clim$coords$time <- dates
  
  return(data_clim)
}