# dependencies
library(startR)

clim4health_load <- function(path, var = NULL) {

    # Read .csv files
    if (all(tools::file_ext(path) == 'csv')) {
  
      data <- read.csv(path)

    # Read .nc files
    } else if (all(tools::file_ext(path) == 'nc')) {
      
      # Use startR function to open the files
      data <- Start(
             dataset     = path,
             var         = var,
             time        = "all",
             ensemble    = "all",
             latitude    = "all",
             longitude   = "all",
             return_vars = list(time = NULL, latitude = NULL, longitude = NULL),
             retrieve    = TRUE)

    } else {

      # If the extension of the files is not in the previous if conditions stop the script
      stop ("The type_file has to be csv or nc, not other type files are allowed at the moment")
    
    }

  return (data)
}