# function to postprocess climate data, maintaining the same object dimensions
# and adding information to the metadata

# dependencies
library(CSTools)

clim4health_calibrate <- function() {
  # calls CST_Calibrate
}

clim4health_skill <- function() {
  # calls CST_RPSS
}

rename_region <- function() {
  # if using output of clim4health_spatial
}

clim4health_postprocess <- function(data) {
  # data an s2dv cube object with dimensions: dataset, var, time, ensemble,
  # latitude, longitude (output of load)
  # calls clim4health_calibrate
  # calls clim4health_skill
}