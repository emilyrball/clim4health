# load data into R object

# dependencies
suppressPackageStartupMessages({
    library(startR)
})

clim4health_load <- function(path, var = NULL) {
    if (all(tools::file_ext(path) == 'csv')) {
      data <- read.csv(path)

    } else if (all(tools::file_ext(path) == 'nc')) {
      
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
      stop ("The type_file has to be csv or nc, not other type files are allowed at the moment")
    
    }

  return (data)
}